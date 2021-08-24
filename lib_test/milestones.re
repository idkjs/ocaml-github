open Printf;

let token = Config.access_token;

let print_milestones = m => {
  List.iter(
    m =>
      Github_t.(
        eprintf(
          "milestone %d: %s (%s)\n%!",
          m.milestone_number,
          m.milestone_title,
          m.milestone_created_at,
        )
      ),
    m,
  );
  eprintf("--\n%!");
};

let t =
  Github.(
    Monad.(
      run(
        {
          let opro_milestones =
            Milestone.for_repo(~user="ocaml", ~repo="opam");
          let milestones = opro_milestones(~state=`Closed, ());
          Stream.to_list(milestones)
          >|= print_milestones
          >>= (
            () => {
              let milestones =
                opro_milestones(~state=`Closed, ~direction=`Asc, ());
              Stream.to_list(milestones)
              >|= print_milestones
              >>= (
                () => {
                  let milestones =
                    Milestone.for_repo(
                      ~sort=`Completeness,
                      ~direction=`Asc,
                      ~user="mxcl",
                      ~repo="homebrew",
                      (),
                    );
                  Stream.to_list(milestones)
                  >|= print_milestones
                  >>= (
                    () => {
                      let milestones =
                        Milestone.for_repo(
                          ~sort=`Completeness,
                          ~direction=`Desc,
                          ~user="mxcl",
                          ~repo="homebrew",
                          (),
                        );
                      Stream.to_list(milestones)
                      >|= print_milestones
                      >>= (
                        () => {
                          let user = "mxcl";
                          let repo = "homebrew";
                          API.set_token(token)
                          >>= (
                            () => {
                              let milestones =
                                Milestone.for_repo(
                                  ~sort=`Completeness,
                                  ~direction=`Desc,
                                  ~user,
                                  ~repo,
                                  (),
                                );
                              Stream.iter(
                                ({Github_t.milestone_number: num, _}) =>
                                  Milestone.get(~user, ~repo, ~num, ())
                                  >>~ (
                                    ({Github_t.milestone_title, _}) => {
                                      eprintf(
                                        "Inside monad: milestone %d: %s\n",
                                        num,
                                        milestone_title,
                                      );
                                      return();
                                    }
                                  ),
                                milestones,
                              );
                            }
                          );
                        }
                      );
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
