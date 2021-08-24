/*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

open Cmdliner;
open Printf;
open Lwt;

let prompt = "Enter Github password: ";

/* Cmdliner converter for Github scope lists */
let scope = {
  let parse = s =>
    switch (Github.Scope.of_string(s)) {
    | None => `Error("unknown scope")
    | Some(s) => `Ok(s)
    };
  let print = (f, s) =>
    Format.pp_print_string(f, Github.Scope.to_string(s));
  (parse, print);
};

let complete_2fa = c => {
  let rec try_again = f =>
    Github.(
      Monad.(
        f()
        >>~ (
          fun
          | Result(auths) => return(auths)
          | Two_factor(mode) =>
            embed(Lwt_io.printf("Enter 2FA code from '%s': ", mode))
            >>= (
              () =>
                embed(Lwt_io.(read_line(stdin)))
                >>= (
                  otp => {
                    let otp = Some(otp);
                    try_again(c(~otp?));
                  }
                )
            )
        )
      )
    );
  let otp = None;
  try_again(c(~otp?));
};

/* Command definitions */
let list_auth = (user, pass) =>
  Github_t.(
    Lwt_main.run(
      Github_cookie_jar.init()
      >>= (
        jar =>
          Passwd.get_if_unset(~prompt, pass)
          >>= (
            (pass: string) =>
              Github.(
                Monad.(run(complete_2fa(Token.get_all(~user, ~pass))))
              )
              >>= (
                auths =>
                  Github_cookie_jar.get_all(jar)
                  >>= (
                    local => {
                      printf(
                        "%-13s | %-8s | %-40s | %-10s\n",
                        "Cookie Name",
                        "ID",
                        "Application",
                        "Note",
                      );
                      printf("%s\n", String.make(80, '-'));
                      List.iter(
                        a => {
                          /* Check if this id is local */
                          let id = a.auth_id;
                          let localnames =
                            List.fold_left(
                              (acc, (n, a)) =>
                                if (a.auth_id == id) {
                                  [n, ...acc];
                                } else {
                                  acc;
                                },
                              [],
                              local,
                            );
                          let print_line = name =>
                            Printf.printf(
                              "%13s | %-8Ld | %-40s | %-10s\n",
                              switch (name) {
                              | None => "<remote>"
                              | Some(n) => n
                              },
                              a.auth_id,
                              a.auth_app.app_name,
                              switch (a.auth_note) {
                              | None => ""
                              | Some(b) => b
                              },
                            );

                          switch (localnames) {
                          | [] => print_line(None)
                          | names =>
                            List.iter(x => print_line(Some(x)), names)
                          };
                        },
                        auths,
                      );
                      return();
                    }
                  )
              )
          )
      ),
    )
  );

let list_local = () =>
  Lwt_main.run(
    Github_cookie_jar.init()
    >>= (
      jar =>
        Github_cookie_jar.get_all(jar)
        >|= (
          local =>
            List.iter(
              ((name, auth)) => {
                open Github_t;
                printf(
                  "%-13s | %-8s | %-40s | %-10s\n",
                  "Cookie Name",
                  "ID",
                  "Application",
                  "Note",
                );
                printf("%s\n", String.make(80, '-'));
                printf(
                  "%13s | %-8Ld | %-40s | %-10s\n",
                  name,
                  auth.auth_id,
                  auth.auth_app.app_name,
                  switch (auth.auth_note) {
                  | None => ""
                  | Some(s) => s
                  },
                );
              },
              local,
            )
        )
    ),
  );

let make_auth =
    (
      user,
      pass,
      name,
      scopes,
      note,
      note_url,
      client_id,
      client_secret,
      fingerprint,
    ) => {
  open Github_t;
  let note =
    switch (note) {
    | None => name
    | Some(note) => note
    };
  Lwt_main.run(
    Github_cookie_jar.init()
    >>= (
      jar =>
        Passwd.get_if_unset(~prompt, pass)
        >>= (
          pass =>
            Github.Monad.run(
              complete_2fa(
                Github.Token.create(
                  ~scopes,
                  ~note,
                  ~note_url?,
                  ~client_id?,
                  ~client_secret?,
                  ~fingerprint?,
                  ~user,
                  ~pass,
                ),
              ),
            )
            >>= (
              auth =>
                Github_cookie_jar.save(jar, ~name, ~auth)
                >>= (
                  _jar => {
                    Printf.printf(
                      "Created token %s (%Ld): %s\n",
                      name,
                      auth.auth_id,
                      Github.Token.(to_string(of_auth(auth))),
                    );
                    return();
                  }
                )
            )
        )
    ),
  );
};

let revoke_auth = (user, pass, name_or_id) =>
  Github_t.(
    Lwt_main.run(
      Github_cookie_jar.init()
      >>= (
        jar =>
          Github_cookie_jar.get(jar, ~name=name_or_id)
          >>= (
            fun
            | Some(auth) => Lwt.return(auth.auth_id)
            | None =>
              try(Lwt.return(Int64.of_string(name_or_id))) {
              | _ =>
                Printf.eprintf("unknown name or id %s\n", name_or_id);
                exit(1);
              }
          )
          >>= (
            id =>
              Passwd.get_if_unset(~prompt, pass)
              >>= (
                pass =>
                  Github.Monad.run(
                    complete_2fa(Github.Token.delete(~user, ~pass, ~id)),
                  )
                  >>= (
                    () =>
                      Github_cookie_jar.get_all(jar)
                      >>= (
                        local =>
                          Lwt_list.fold_left_s(
                            (jar, (name, a)) =>
                              if (a.auth_id == id) {
                                Github_cookie_jar.delete(jar, ~name);
                              } else {
                                return(jar);
                              },
                            jar,
                            local,
                          )
                          >>= (_ => return_unit)
                      )
                  )
              )
          )
      ),
    )
  );

/* Command declarations for Cmdliner */
let user =
  Arg.(
    required
    & pos(0, some(string), None)
    & info([], ~docv="USERNAME", ~doc="GitHub username.")
  );
let pass =
  Arg.(
    value
    & opt(some(string), None)
    & info(
        ["p", "password"],
        ~docv="PASSWORD",
        ~doc=
          "GitHub password. If not specified, this will be obtained interactively.",
      )
  );
let name_or_id =
  Arg.(
    required
    & pos(1, some(string), None)
    & info(
        [],
        ~docv="TOKEN_NAME_OR_ID",
        ~doc="Cookie name or numeric GitHub token id.",
      )
  );

let list_local_cmd = (
  Term.(pure(list_local) $ pure()),
  Term.info("local", ~doc="list local active GitHub authorization tokens"),
);

let list_cmd = (
  Term.(pure(list_auth) $ user $ pass),
  Term.info(
    "show",
    ~doc="list all active GitHub authorization tokens, including remote ones.",
  ),
);

let make_cmd = {
  let scopes = {
    let scopes =
      Github.Scope.(String.concat(", ", List.map(to_string, all)));
    let doc =
      Printf.sprintf(
        "Comma delimited list of repository scopes. Can be: %s",
        scopes,
      );
    Arg.(
      value
      & opt(list(scope), [])
      & info(["s", "scopes"], ~docv="SCOPES", ~doc)
    );
  };
  let note =
    Arg.(
      value
      & opt(some(string), None)
      & info(
          ["note"],
          ~docv="NOTE",
          ~doc="Informational note to record beside the authorization token",
        )
    );
  let note_url =
    Arg.(
      value
      & opt(some(string), None)
      & info(
          ["url"],
          ~docv="URL",
          ~doc="URL to record beside the authorization token",
        )
    );
  let client_id =
    Arg.(
      value
      & opt(some(string), None)
      & info(
          ["client-id"],
          ~docv="CLIENT_ID",
          ~doc=
            "Optional oAuth client id to register this token with an application.",
        )
    );
  let client_secret =
    Arg.(
      value
      & opt(some(string), None)
      & info(
          ["client-secret"],
          ~docv="CLIENT_SECRET",
          ~doc=
            "Optional oAuth client secret to register this token with an application.",
        )
    );
  let fingerprint =
    Arg.(
      value
      & opt(some(string), None)
      & info(
          ["fingerprint"],
          ~docv="FINGERPRINT",
          ~doc="Unique token fingerprint",
        )
    );
  let tname =
    Arg.(
      required
      & pos(1, some(string), None)
      & info(
          [],
          ~docv="COOKIE",
          ~doc=
            "The local name for the authorization token that applications can look up.",
        )
    );
  (
    Term.(
      pure(make_auth)
      $ user
      $ pass
      $ tname
      $ scopes
      $ note
      $ note_url
      $ client_id
      $ client_secret
      $ fingerprint
    ),
    Term.info("make", ~doc="create a new GitHub authorization token"),
  );
};

let revoke_cmd = (
  Term.(pure(revoke_auth) $ user $ pass $ name_or_id),
  Term.info(
    "revoke",
    ~doc=
      "revoke a remote GitHub authorization token and remove it from the local cookie jar.",
  ),
);

let default_cmd = {
  let doc = "let local applications use GitHub authorization tokens";
  (
    Term.(ret(pure(`Help((`Pager, None))))),
    {
      let man = [
        `S("DESCRIPTION"),
        `P(
          "Applications that want to use GitHub will need to save authorization tokens locally, and $(b,git-jar) provides a CLI interface to manipulate them. GitHub authorization tokens are mapped onto a local $(i,name), and applictions can query that name at runtime to retrieve a token to use in GitHub API calls.",
        ),
        `P(
          "All the tokens are stored in $(i,$HOME/.github/jar/<name>), where $(i,<name>) is the local name of the token.",
        ),
        `S("COMMON OPTIONS"),
        `P(
          "$(b,--password) optionally specifies the GitHub password on the command-line. If it isn't present, then the password will be obtained interactively.",
        ),
        `P(
          "$(b,--help) will show more help for each of the sub-commands above.",
        ),
        `S("BUGS"),
        `P(
          "Email bug reports to <mirageos-devel@lists.xenproject.org>, or report them online at <http://github.com/mirage/ocaml-github/issues>.",
        ),
      ];
      Term.info("git-jar", ~version=Jar_version.t, ~doc, ~man);
    },
  );
};

let cmds = [list_cmd, list_local_cmd, make_cmd, revoke_cmd];

let () =
  try(
    switch (Term.eval_choice(~catch=false, default_cmd, cmds)) {
    | `Error(_) => exit(1)
    | _ => exit(0)
    }
  ) {
  | [@implicit_arity] Github.Message(_, m) =>
    eprintf("GitHub API error: %s\n", Github.API.string_of_message(m));
    exit(1);
  };
