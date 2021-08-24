/*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
open Cmdliner;
open Printf;

let sync_releases = (token, src_user, src_repo, dst_user, dst_repo) =>
  Github.(
    Monad.(
      run(
        {
          let releases =
            Release.for_repo(~token, ~user=src_user, ~repo=src_repo, ());
          Stream.to_list(releases);
        },
      )
    )
  )
  >>= (
    src =>
      /* TODO: unused?? */
      Github.(
        Monad.(
          run(
            {
              let releases =
                Release.for_repo(~token, ~user=dst_user, ~repo=dst_repo, ());
              Stream.to_list(releases);
            },
          )
        )
      )
      >>= (
        _dst =>
          Github.(
            Monad.(
              run(
                {
                  let releases =
                    Repo.tags(~token, ~user=src_user, ~repo=src_repo, ());
                  Stream.to_list(releases);
                },
              )
            )
          )
          >>= (
            src_tags =>
              Github_t.(
                Lwt_list.iter_s(
                  r => {
                    let tag =
                      List.find(
                        x => x.repo_tag_name == r.release_tag_name,
                        src_tags,
                      );
                    let _target =
                      switch (r.release_target_commitish) {
                      | None => "master"
                      | Some(t) => t
                      };

                    let sha = tag.repo_tag_commit.repo_commit_sha;
                    let name =
                      switch (r.release_name) {
                      | Some(name) => name
                      | None => "NULL"
                      };
                    printf(
                      "%s %s %s %b %b\n",
                      r.release_tag_name,
                      sha,
                      name,
                      r.release_draft,
                      r.release_prerelease,
                    );
                    let release = {
                      new_release_tag_name: r.release_tag_name,
                      new_release_target_commitish: sha,
                      new_release_name: r.release_name,
                      new_release_body: r.release_body,
                      new_release_draft: r.release_draft,
                      new_release_prerelease: r.release_prerelease,
                    };
                    print_endline(Github_j.string_of_new_release(release));
                    Github.(
                      Monad.(
                        run(
                          Release.create(
                            ~token,
                            ~user=dst_user,
                            ~repo=dst_repo,
                            ~release,
                            (),
                          ),
                        )
                      )
                    )
                    >>= (_r => return_unit);
                  },
                  src,
                )
              )
          )
      )
  );

let run = (token, src_user, src_repo, dst_user, dst_repo) =>
  Lwt_main.run(sync_releases(token, src_user, src_repo, dst_user, dst_repo));

let cmd = {
  let cookie = Jar_cli.cookie();
  let src_user = {
    let doc = "The source user name on GitHub";
    Arg.(
      required
      & pos(0, some(string), None)
      & info([], ~docv="SRC_USER", ~doc)
    );
  };

  let src_repo = {
    let doc = "The source repository on GitHub";
    Arg.(
      required
      & pos(1, some(string), None)
      & info([], ~docv="SRC_REPO", ~doc)
    );
  };

  let dst_user = {
    let doc = "The destination user name on GitHub";
    Arg.(
      required
      & pos(2, some(string), None)
      & info([], ~docv="DST_USER", ~doc)
    );
  };

  let dst_repo = {
    let doc = "The destination repository on GitHub";
    Arg.(
      required
      & pos(3, some(string), None)
      & info([], ~docv="DST_REPO", ~doc)
    );
  };

  let doc = "synchronize releases between GitHub repositories";
  let man = [
    `S("BUGS"),
    `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
  ];
  (
    Term.(pure(run) $ cookie $ src_user $ src_repo $ dst_user $ dst_repo),
    Term.info("git-sync-releases", ~version=Jar_version.t, ~doc, ~man),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };
