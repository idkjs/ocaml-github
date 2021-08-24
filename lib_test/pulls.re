open Printf;

let token = Config.access_token;

let print_pulls = pl =>
  Github.(
    Monad.(
      Stream.iter(
        p => {
          open Github_t;
          eprintf(
            "pull request %d: %s (%s)\n%!",
            p.pull_number,
            p.pull_title,
            p.pull_created_at,
          );
          return();
        },
        pl,
      )
      >>= (
        () => {
          eprintf("--\n%!");
          return();
        }
      )
    )
  );

let t =
  Github.(
    Monad.(
      run(
        {
          let user = "ocaml";
          let repo = "opam";
          let opam_repo_pulls = Pull.for_repo(~user, ~repo);
          return(opam_repo_pulls(~state=`Open, ()))
          >>= print_pulls
          >>= (
            () =>
              return(opam_repo_pulls(~state=`Closed, ()))
              >>= print_pulls
              >>= (
                () =>
                  return(opam_repo_pulls())
                  >>= Stream.iter(hd =>
                        Pull.get(
                          ~token,
                          ~user,
                          ~repo,
                          ~num=hd.Github_t.pull_number,
                          (),
                        )
                        >>~ (
                          p => {
                            eprintf(
                              "Inside monad: pull %d: %s\n%!",
                              p.Github_t.pull_number,
                              p.Github_t.pull_title,
                            );
                            return(
                              Pull.commits(
                                ~token,
                                ~user,
                                ~repo,
                                ~num=hd.Github_t.pull_number,
                                (),
                              ),
                            )
                            >>= Stream.iter(commit => {
                                  eprintf(
                                    "    %s\n",
                                    commit.Github_t.commit_sha,
                                  );
                                  return();
                                })
                            >>= (
                              () => {
                                eprintf("---------\n%!");
                                return(
                                  Pull.files(
                                    ~token,
                                    ~user,
                                    ~repo,
                                    ~num=hd.Github_t.pull_number,
                                    (),
                                  ),
                                )
                                >>= Stream.iter(file => {
                                      eprintf(
                                        "    %s\n",
                                        file.Github_t.file_filename,
                                      );
                                      return();
                                    });
                              }
                            );
                          }
                        )
                      )
              )
          );
        },
      )
    )
  );

Lwt_main.run(t);
