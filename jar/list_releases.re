/*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt;
open Cmdliner;
open Printf;

let parse_iso8601_from_github = t =>
  /* This parses just a subset of ISO8601 that GitHub returns:
     e.g. 2014-02-21T13:39:04Z */
  Scanf.sscanf(
    t,
    "%4d-%2d-%2dT%2d:%2d:%2dZ",
    (tm_year, tm_mon, tm_mday, tm_hour, tm_min, tm_sec) =>
    Unix.(
      mktime({
        tm_year: tm_year - 1900,
        tm_mon: tm_mon - 1,
        tm_mday,
        tm_hour,
        tm_min,
        tm_sec,
        tm_wday: 0,
        tm_yday: 0,
        tm_isdst: false,
      })
    )
  );

let release_to_markdown = ((user, repo, r)) => {
  open Github_t;
  let (_, tm) = parse_iso8601_from_github(r.release_created_at);
  let name =
    switch (r.release_name) {
    | Some(name) => name
    | None => "NULL"
    };
  printf("### %s-%s: %s\n\n", repo, r.release_tag_name, name);
  printf(
    "Released on %4d-%02d-%02d as [%s](%s). See <https://github.com/%s/%s> for full history.\n\n",
    tm.Unix.tm_year + 1900,
    tm.Unix.tm_mon + 1,
    tm.Unix.tm_mday,
    r.release_tag_name,
    r.release_html_url,
    user,
    repo,
  );
  switch (r.release_body) {
  | None =>
    printf("NULL\n\n");
    return_unit;
  | Some("") => return_unit
  | Some(body) =>
    printf("%s\n\n", body);
    return();
  };
};

let releases_to_json = rs =>
  print_endline(
    Github_j.string_of_release_repos(
      List.map(
        ((release_repo_user, release_repo_repo, release_repo_release)) =>
          {
            Github_j.release_repo_user,
            release_repo_repo,
            release_repo_release,
          },
        rs,
      ),
    ),
  );

let list_releases = (token, repos, json) => {
  let repos =
    List.map(
      r =>
        switch (Stringext.split(~max=2, ~on='/', r)) {
        | [user, repo] => (user, repo)
        | _ =>
          eprintf(
            "Repositories must be in username/repo format (e.g. mirage/ocaml-cohttp\n",
          );
          exit(1);
        },
      repos,
    );
  /* Get the releases per repo */
  Lwt_list.fold_left_s(
    (a, (user, repo)) =>
      Github.(
        Monad.(
          run(
            {
              let releases = Release.for_repo(~token, ~user, ~repo, ());
              Stream.to_list(releases);
            },
          )
        )
      )
      >>= (r => return(List.map(r => (user, repo, r), r) @ a)),
    [],
    repos,
  )
  >>= (
    releases => {
      /* Sort them by tag creation date */
      let rtime = ((_, _, r)) =>
        fst(parse_iso8601_from_github(r.Github_t.release_created_at));
      let releases =
        List.sort((b, a) => compare(rtime(a), rtime(b)), releases);
      switch (json) {
      | false => Lwt_list.iter_s(release_to_markdown, releases)
      | true =>
        releases_to_json(releases);
        return_unit;
      };
    }
  );
};

let cmd = {
  let cookie = Jar_cli.cookie();
  let repos = Jar_cli.repos(~doc_append=" to scan for changelogs", ());
  let doc = "list releases on GitHub repositories";
  let man = [
    `S("BUGS"),
    `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
  ];
  let json = {
    let doc = "Output in JSON format.";
    Arg.(value & flag & info(["json"], ~doc));
  };
  (
    Term.(
      pure((t, r, j) => Lwt_main.run(list_releases(t, r, j)))
      $ cookie
      $ repos
      $ json
    ),
    Term.info("git-list-releases", ~version=Jar_version.t, ~doc, ~man),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
