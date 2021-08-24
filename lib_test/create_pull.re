/*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
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
open Printf;

let token = Config.access_token;
let user = "dsheets";
let repo = "opam-repository";

let t = {
  let issue = {
    Github_t.new_issue_title: "ocaml-github regression test issue",
    new_issue_body: Some("ocaml-github body"),
    new_issue_assignee: Some("dsheets"),
    new_issue_milestone: None,
    new_issue_labels: [],
  };

  Github.(
    Monad.(
      run(Issue.create(~token, ~user, ~repo, ~issue, ()) >|= Response.value)
    )
  )
  >>= (
    issue => {
      eprintf("created issue number %d\n%!", issue.Github_t.issue_number);

      let pull_issue =
        Github_t.{
          new_pull_issue_issue: issue.issue_number,
          new_pull_issue_head: "ocamlot:master",
          new_pull_issue_base: "master",
        };

      Github.(
        Monad.(
          run(
            Pull.create_from_issue(~token, ~user, ~repo, ~pull_issue, ())
            >|= Response.value,
          )
        )
      )
      >>= (
        pull => {
          let num = pull.Github_t.pull_number;
          eprintf(
            "created pull request number %d from issue %d\n%!",
            num,
            issue.Github_t.issue_number,
          );

          let update_pull =
            Github_t.{
              update_pull_title:
                Some("ocaml-github regression test pull request"),
              update_pull_body: Some("ocaml-github pull request body"),
              update_pull_state: None,
              update_pull_base: None,
            };

          Github.(
            Monad.(
              run(
                Pull.update(~token, ~user, ~repo, ~update_pull, ~num, ())
                >|= Response.value,
              )
            )
          )
          >>= (
            pull => {
              eprintf(
                "updated pull request number %d with title \"%s\"\n%!",
                num,
                pull.Github_t.pull_title,
              );

              Github.(
                Monad.(
                  run(
                    Pull.is_merged(~token, ~user, ~repo, ~num, ())
                    >|= Response.value,
                  )
                )
              )
              >>= (
                merged_flag => {
                  eprintf(
                    "is pull request number %d merged? %b\n%!",
                    num,
                    merged_flag,
                  );

                  return();
                }
              );
            }
          );
        }
      );
    }
  );
};

let _ = Lwt_main.run(t);
