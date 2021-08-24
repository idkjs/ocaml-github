open Printf;
open Github_t;

let token = Config.access_token;

let opam_first_commit = "3656b4d1b03a8ae356cf82f7052f1976df8787be";

let t =
  Github.(
    Monad.(
      run(
        Repo.info(~token, ~user="ocaml", ~repo="opam", ())
        >>~ (
          info => {
            let descr =
              switch (info.repository_description) {
              | Some(descr) => descr
              | None => ""
              };

            eprintf("repo %s\n", descr);
            switch (info.repository_permissions) {
            | Some(permissions) =>
              eprintf(
                "permissions admin(%B) push(%B) pull(%B)\n",
                permissions.repository_permissions_admin,
                permissions.repository_permissions_push,
                permissions.repository_permissions_pull,
              )
            | None => ()
            };
            let branches =
              Repo.branches(~token, ~user="ocaml", ~repo="opam", ());
            Stream.to_list(branches)
            >>= (
              branches => {
                List.iter(
                  b =>
                    eprintf(
                      "branch %s %s\n",
                      b.repo_branch_name,
                      b.repo_branch_commit.repo_commit_sha,
                    ),
                  branches,
                );
                Repo.get_commit(
                  ~token,
                  ~user="ocaml",
                  ~repo="opam",
                  ~sha=opam_first_commit,
                  (),
                )
                >>~ (
                  commit => {
                    eprintf(
                      "opam first commit author date: %s\n",
                      commit.commit_git.git_commit_author.info_date,
                    );
                    eprintf(
                      "opam first commit committer date: %s\n",
                      commit.commit_git.git_commit_author.info_date,
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

Lwt_main.run(t);
