open Printf;
open Github_t;

let token = Config.access_token;

let org = "mirage";

let t =
  Github.(
    Monad.(
      run(
        {
          let get_teams = Organization.teams(~token, ~org);
          Stream.next(get_teams())
          >>= (
            fun
            | None => {
                eprintf("no teams for %s\n", org);
                exit(1);
              }
            | Some((first_team, _)) => {
                let get_first_team =
                  Team.info(~token, ~id=first_team.team_id);
                get_first_team()
                >>~ (
                  team => {
                    eprintf(
                      "team %Ld: %s (%s)\n%!",
                      team.team_info_id,
                      team.team_info_name,
                      team.team_info_url,
                    );
                    let get_team_repos =
                      Team.repositories(~token, ~id=team.team_info_id);

                    Stream.next(get_team_repos())
                    >>= (
                      fun
                      | None => {
                          eprintf("no repos for %s\n", team.team_info_name);
                          exit(1);
                        }
                      | Some((first_repo, _)) => {
                          eprintf(
                            "repo %Ld: %s\n%!",
                            first_repo.repository_id,
                            first_repo.repository_name,
                          );
                          return();
                        }
                    );
                  }
                );
              }
          );
        },
      )
    )
  );

Lwt_main.run(t);
