open Printf;

let token = Config.access_token;
let user = "ocaml";
let repo = "opam";

let t =
  Github.(
    Monad.(
      Github_t.(
        run(
          {
            let issues = Issue.for_repo(~token, ~user, ~repo, ());
            Stream.iter(
              issue => {
                let num = issue.issue_number;
                eprintf("issue %d: %s\n%!", num, issue.issue_title);
                let issue_comments =
                  Issue.comments(~token, ~user, ~repo, ~num, ());
                Stream.to_list(issue_comments)
                >>= (
                  comments => {
                    List.iter(
                      c =>
                        eprintf(
                          "  > %Ld: %s\n",
                          c.issue_comment_id,
                          c.issue_comment_body,
                        ),
                      comments,
                    );
                    return();
                  }
                );
              },
              issues,
            );
          },
        )
      )
    )
  );

Lwt_main.run(t);
