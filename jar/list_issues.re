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
open Printf;

module T = Github_t;

let ask_github = fn => Github.(Monad.run(fn()));

let string_of_labels = labels => {
  let names = List.map(({T.label_name, _}) => label_name, labels);
  String.concat(", ", names);
};

let print_issue = (user, repo, issue) => {
  let {
    T.issue_number,
    issue_title,
    issue_labels,
    issue_comments,
    issue_state,
    issue_created_at,
    issue_closed_at,
    _,
  } = issue;
  printf("%s/%s#%d %s\n", user, repo, issue_number, issue_title);
  printf("  Labels: %s\n", string_of_labels(issue_labels));
  printf("  Comments: %d\n", issue_comments);
  switch (issue_state) {
  | `Open => printf("  Created at %s\n", issue_created_at)
  | `Closed =>
    switch (issue_closed_at) {
    | Some(timestamp) => printf("  Closed at %s\n", timestamp)
    | None => printf("  Closed timestamp missing!")
    }
  };
};

let list_issues = (token, repos, ~all, ~closed, ~prs, ~issues) => {
  let repos =
    List.map(
      r =>
        switch (Stringext.split(~max=2, ~on='/', r)) {
        | [user, repo] => (user, repo)
        | _ =>
          eprintf("Repositories must be in username/repo format");
          exit(1);
        },
      repos,
    );
  /* Get the issues per repo */
  Lwt_list.iter_s(
    ((user, repo)) => {
      let state =
        if (all) {
          `All;
        } else if (closed) {
          `Closed;
        } else {
          `Open;
        };
      Github.(
        Monad.(
          run(
            {
              let issues_s = Issue.for_repo(~token, ~state, ~user, ~repo, ());
              Stream.to_list(issues_s)  /* TODO: bound?!?! */
              >>= (
                list =>
                  return(
                    List.iter(
                      i =>
                        switch (i) {
                        | {T.issue_pull_request: None, _} when issues =>
                          print_issue(user, repo, i)
                        | {T.issue_pull_request: Some(_), _} when prs =>
                          print_issue(user, repo, i)
                        | _ => ()
                        },
                      list,
                    ),
                  )
              );
            },
          )
        )
      );
    },
    repos,
  );
};

let cmd = {
  let cookie = Jar_cli.cookie();
  let repos = Jar_cli.repos(~doc_append=" to list issues and PRs", ());

  let doc = "show only closed issues";
  let docv = "CLOSED";
  let closed = Arg.(value & flag & info(["closed"], ~docv, ~doc));
  let doc = "show all issues";
  let docv = "ALL";
  let all = Arg.(value & flag & info(["all"], ~docv, ~doc));

  let doc = "show PRs";
  let docv = "PRS";
  let no_prs = Arg.(value & flag & info(["prs"], ~docv, ~doc));
  let doc = "show regular (non-PR) issues";
  let docv = "ISSUES";
  let no_issues = Arg.(value & flag & info(["issues"], ~docv, ~doc));

  let doc = "list issues on GitHub repositories (open only by default)";
  let man = [
    `S("BUGS"),
    `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
  ];
  (
    Term.(
      pure((t, r, all, closed, prs_flag, issues_flag) =>{
        let prs = prs_flag || !issues_flag;
        let issues = issues_flag || !prs_flag;
        Lwt_main.run(list_issues(t, r, ~all, ~closed, ~prs, ~issues));
     } )
      $ cookie
      $ repos
      $ all
      $ closed
      $ no_prs
      $ no_issues
    ),
    Term.info("git-list-issues", ~version=Jar_version.t, ~doc, ~man),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
