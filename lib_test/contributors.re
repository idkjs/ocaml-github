open Printf;
open Github_t;

let user = "ocaml";
let repo = "opam-repository";

let get_auth_token_from_jar = auth_id =>
  Lwt.(
    Github_cookie_jar.init()
    >>= (
      jar =>
        Github_cookie_jar.(get(~name=auth_id, jar))
        >>= (
          fun
          | Some(x) => return(x)
          | None =>
            Lwt.fail(Failure("id '" ++ auth_id ++ "' not in cookie jar"))
        )
    )
  );

let last_seen =
  List.fold_left(
    (
      last,
      {
        Github_t.repo_contribution_week_w: w,
        repo_contribution_week_a: a,
        repo_contribution_week_d: d,
        repo_contribution_week_c: c,
      },
    ) => {
      let active = a != 0 || d != 0 || c != 0;
      switch (last) {
      | None =>
        if (active) {
          Some(w);
        } else {
          last;
        }
      | Some(last_week) when w > last_week =>
        if (active) {
          Some(w);
        } else {
          last;
        }
      | Some(_) => last
      };
    },
    None,
  );

let month_of_time_opt =
  fun
  | None => "never"
  | Some(time) =>
    input_line(Unix.open_process_in(sprintf("date -r %d +%%Y-%%m", time)));

let space_after = s => String.init(20 - String.length(s), _ => ' ');

let t =
  Github.(
    Monad.(
      run(
        embed(get_auth_token_from_jar("test"))
        >>= (
          auth => {
            let token = Token.of_auth(auth);
            let contributors = Repo.contributors(~token, ~user, ~repo, ());
            Stream.to_list(contributors)
            >>= (
              contributors => {
                let table = Hashtbl.create(256);
                List.iter(
                  c =>
                    Hashtbl.replace(
                      table,
                      c.contributor_login,
                      c.contributor_contributions,
                    ),
                  contributors,
                );
                let contributor_stats =
                  Stats.contributors(~token, ~user, ~repo, ());
                Stream.to_list(contributor_stats)
                >|= List.rev
                >>= (
                  fun
                  | [] => {
                      eprintf(
                        "No contributors found OR data not yet computed and cached.",
                      );
                      return();
                    }
                  | stats => {
                      printf(
                        "login%s:\ttotal commits in %s/%s\t:\tlast month of contribution\n",
                        space_after("login"),
                        user,
                        repo,
                      );
                      List.iter(
                        c =>
                          switch (c.repo_contributor_stats_author) {
                          | Some(author) =>
                            let user = author.user_login;
                            let from_table =
                              try(string_of_int(Hashtbl.find(table, user))) {
                              | Not_found => "?"
                              };

                            let commits =
                              sprintf(
                                "%d (%s)",
                                c.repo_contributor_stats_total,
                                from_table,
                              );

                            printf(
                              "%s%s:\t%s%s:\t%s\n",
                              user,
                              space_after(user),
                              commits,
                              space_after(commits),
                              month_of_time_opt(
                                last_seen(c.repo_contributor_stats_weeks),
                              ),
                            );
                            Hashtbl.remove(table, user);
                          | None => ()
                          },
                        stats,
                      );
                      let remaining =
                        Hashtbl.fold(
                          (k, v, l) => [(k, v), ...l],
                          table,
                          [],
                        );
                      let remaining =
                        List.sort(
                          ((_, x), (_, y)) => compare(y, x),
                          remaining,
                        );
                      List.iter(
                        ((k, v)) => {
                          let commits = sprintf("! (%d)", v);
                          printf(
                            "%s%s:\t%s%s:\t?\n",
                            k,
                            space_after(k),
                            commits,
                            space_after(commits),
                          );
                        },
                        remaining,
                      );
                      return();
                    }
                );
              }
            );
          }
        ),
      )
    )
  );

try(Lwt_main.run(t)) {
| [@implicit_arity] Github.Message(_, message) =>
  eprintf("GitHub API error: %s\n", Github.API.string_of_message(message));
  exit(1);
};
