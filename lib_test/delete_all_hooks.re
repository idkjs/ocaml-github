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

open Printf;

let token = Config.access_token;
let user = "ocamlot";
let repo = "opam-repository";

let get_hooks = Github.Repo.Hook.for_repo(~user, ~repo, ());

let t =
  Github.(
    Monad.(
      run(
        Github_t.(
          API.set_user_agent("delete_all_hooks")
          >>= (
            () =>
              API.set_token(token)
              >>= (
                () =>
                  Stream.to_list(get_hooks)
                  >>= (
                    hooks => {
                      printf("Present: %d hooks\n", List.length(hooks));
                      List.fold_left(
                        (m, h) =>
                          m
                          >>= (
                            () =>
                              Repo.Hook.delete(
                                ~user,
                                ~repo,
                                ~id=h.hook_id,
                                (),
                              )
                              >|= Response.value
                          ),
                        return(),
                        hooks,
                      )
                      >>= (
                        () =>
                          Stream.to_list(get_hooks)
                          >>= (
                            hooks => {
                              printf(
                                "Present: %d hooks\n",
                                List.length(hooks),
                              );
                              return();
                            }
                          )
                      );
                    }
                  )
              )
          )
        ),
      )
    )
  );

let _ = Lwt_main.run(t);
