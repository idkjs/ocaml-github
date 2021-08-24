open Github_t;

let token = Config.access_token;

let org = "mirage";

let t =
  Github.(
    Monad.(
      run(
        {
          let orgs = Organization.repositories(~token, ~org, ());
          Stream.to_list(orgs)
          >>= (
            fun
            | [] => {
                Printf.eprintf("no repos for organisation\n");
                exit(1);
              }
            | x => {
                List.iter(
                  fun
                  | repo =>
                    Printf.eprintf(
                      "org %Ld: %s\n%!",
                      repo.repository_id,
                      repo.repository_name,
                    ),
                  x,
                );
                return();
              }
          );
        },
      )
    )
  );

Lwt_main.run(t);
