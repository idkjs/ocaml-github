open Cmdliner;

let token = Config.access_token;

let list_check_runs_for_ref = (owner, repo, sha, ()) =>
  Lwt_main.run(
    Github.(
      Monad.(
        run(
          Check.list_check_runs_for_ref(~token, ~owner, ~repo, ~sha, ())
          >>~ (
            check => {
              List.iter(
                x =>
                  Printf.printf(
                    "check_run_name %s check_run_id %Ld\n",
                    x.Github_j.check_run_name,
                    x.Github_j.check_run_id,
                  ),
                check.Github_j.check_runs,
              );
              return();
            }
          ),
        )
      )
    ),
  );

let get_check_run = (owner, repo, check_run_id, ()) =>
  Lwt_main.run(
    Github.(
      Monad.(
        run(
          Check.get_check_run(~token, ~owner, ~repo, ~check_run_id, ())
          >>~ (
            check_run => {
              Printf.printf(
                "check_run_name %s check_run_id %s\n",
                check_run.Github_j.check_run_name,
                Github_j.string_of_check_status(
                  check_run.Github_j.check_run_status,
                ),
              );
              return();
            }
          ),
        )
      )
    ),
  );

module CommandLine = {
  let repo = {
    let doc = "Repository";
    Arg.(
      required
      & opt(some(string), None)
      & info(["r", "repository"], ~docv="REPOSITORY", ~doc)
    );
  };

  let owner = {
    let doc = "Owner";
    Arg.(
      required
      & opt(some(string), None)
      & info(["o", "owner"], ~docv="OWNER", ~doc)
    );
  };

  let sha = {
    let doc = "Git SHA";
    Arg.(
      required
      & opt(some(string), None)
      & info(["s", "sha"], ~docv="GIT_SHA", ~doc)
    );
  };

  let check_run_id = {
    let doc = "Check Run Id";
    Arg.(
      required
      & opt(some(string), None)
      & info(["c", "check_run_id"], ~docv="CHECK_RUN_ID", ~doc)
    );
  };

  let list_cmd = (
    Term.(pure(list_check_runs_for_ref) $ owner $ repo $ sha $ pure()),
    Term.info("list", ~doc="List Check Runs for a git sha"),
  );

  let get_check_run = (
    Term.(pure(get_check_run) $ owner $ repo $ check_run_id $ pure()),
    Term.info("get-check", ~doc="Get a Check Run for check run id"),
  );

  let cmds = [get_check_run, list_cmd];

  let default_cmd = (
    Term.(ret(pure(`Help((`Pager, None))))),
    Term.info("checks", ~version="0.1", ~doc="Github Checks API."),
  );
};

let cmdliner =
  switch (
    Term.eval_choice(~catch=false, CommandLine.default_cmd, CommandLine.cmds)
  ) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
