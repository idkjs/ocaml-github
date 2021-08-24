/*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

module T = Github_t;

let help_sections = [
  `S("BUGS"),
  `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
];

let print_repository =
    (
      {
        T.repository_full_name,
        repository_description,
        repository_stargazers_count,
        repository_language,
        repository_html_url,
        _,
      },
    ) => {
  let language =
    switch (repository_language) {
    | None => ""
    | Some(lang) => " (" ++ lang ++ ")"
    };

  Lwt_io.printf(
    "%s%s [%d stars]\n<%s>\n%s\n\n",
    repository_full_name,
    language,
    repository_stargazers_count,
    repository_html_url,
    switch (repository_description) {
    | None => ""
    | Some(d) => d
    },
  );
};

let search = (token, ~language=?, ~sort=?, keywords) => {
  let basic_qs = [`In([`Name, `Description, `Readme])];
  let qualifiers =
    switch (language) {
    | None => basic_qs
    | Some(lang) => [`Language(lang), ...basic_qs]
    };

  Github.(
    Monad.(
      run(
        {
          let results =
            Github.Search.repos(~token, ~sort?, ~qualifiers, ~keywords, ());
          Stream.next(results)  /* TODO: option for count? */
          >>= (
            fun
            | Some((
                {T.repository_search_items, repository_search_total_count, _},
                _,
              )) =>
              embed(
                Lwt_io.printf(
                  "%d results returned of %d total\n\n",
                  List.length(repository_search_items),
                  repository_search_total_count,
                ),
              )
              >>= (
                () =>
                  embed(
                    Lwt_list.iter_s(
                      print_repository,
                      repository_search_items,
                    ),
                  )
              )
            | None => embed(Lwt_io.printf("No more results.\n\n"))
          );
        },
      )
    )
  );
};

let repo_cmd = {
  let cookie = Jar_cli.cookie();

  let doc = "sort by stars, forks, updated, or magic (default)";
  let docv = "SORTBY";
  let sort =
    Arg.(
      value
      & opt(
          enum([
            ("stars", Some(`Stars)),
            ("forks", Some(`Forks)),
            ("updated", Some(`Updated)),
            ("magic", None),
          ]),
          None,
        )
      & info(["sort"], ~docv, ~doc)
    );

  let doc = "language filter";
  let docv = "LANGUAGE";
  let language =
    Arg.(
      value & opt(some(string), None) & info(["language"], ~docv, ~doc)
    );

  let doc = "keywords";
  let docv = "KEYWORDS";
  let keywords = Arg.(value & pos_all(string, []) & info([], ~docv, ~doc));

  let doc = "search GitHub repositories";
  let man = help_sections;
  (
    Term.(
      pure((t, language, keywords, sort) =>
        Lwt_main.run(search(t, ~language?, ~sort?, keywords))
      )
      $ cookie
      $ language
      $ keywords
      $ sort
    ),
    Term.info("repo", ~version=Jar_version.t, ~doc, ~man),
  );
};

let default_cmd = {
  let doc = "search GitHub";
  let man =
    [
      `S("DESCRIPTION"),
      `P(
        "$(b, git search) searches GitHub for repositories, code, issues, or users.",
      ),
    ]
    @ help_sections;

  let no_cmd_err = `Error((true, "No search object type given."));
  Term.(ret(pure(no_cmd_err)), info("git-search", ~doc, ~man));
};

switch (Term.eval_choice(default_cmd, [repo_cmd])) {
| `Error(_) => exit(1)
| _ => exit(0)
};
