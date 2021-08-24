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

let print_hooks = label =>
  Github_t.(
    Github.(
      Stream.iter(hook => {
        eprintf(
          "%s %s hook %Ld created on %s %b detecting %s\n%!",
          label,
          switch (hook.hook_config) {
          | `Web(_) => "web"
          | `Unknown(cons, _) => cons
          },
          hook.hook_id,
          hook.hook_created_at,
          hook.hook_active,
          List.fold_left(
            (s, ev) => s ++ Github_j.string_of_event_type(ev) ++ " ",
            "",
            hook.hook_events,
          ),
        );
        Monad.return();
      })
    )
  );

let make_web_hook_config = (url, secret) =>
  Github_t.{
    web_hook_config_url: url,
    web_hook_config_content_type: Some("json"),
    web_hook_config_insecure_ssl: false,
    web_hook_config_secret: secret,
  };

let make_hook = (url, events) =>
  Github_t.{
    new_hook_config: `Web(make_web_hook_config(url, None)),
    new_hook_events: events,
    new_hook_active: true,
  };

let get_hooks = Github.Repo.Hook.for_repo(~token, ~user, ~repo, ());

let t =
  Github.(
    Monad.(
      run(
        Github_t.(
          API.set_user_agent("create_hook")
          >>= (
            () =>
              print_hooks("Present:", get_hooks)
              >>= (
                () => {
                  let hook =
                    make_hook(
                      "http://example.com/",
                      [`Push, `PullRequest, `Status],
                    );
                  Repo.Hook.create(~token, ~user, ~repo, ~hook, ())
                  >>~ (
                    hook_a =>
                      print_hooks("Created:", Stream.of_list([hook_a]))
                      >>= (
                        () => {
                          let hook =
                            make_hook(
                              "http://example.org/",
                              [
                                `CommitComment,
                                `IssueComment,
                                `PullRequestReviewComment,
                              ],
                            );
                          Repo.Hook.create(~token, ~user, ~repo, ~hook, ())
                          >>~ (
                            hook_b =>
                              print_hooks(
                                "Created:",
                                Stream.of_list([hook_b]),
                              )
                              >>= (
                                () =>
                                  Repo.Hook.get(
                                    ~token,
                                    ~user,
                                    ~repo,
                                    ~id=hook_b.hook_id,
                                    (),
                                  )
                                  >>~ (
                                    hook =>
                                      print_hooks(
                                        "Just:",
                                        Stream.of_list([hook]),
                                      )
                                      >>= (
                                        () =>
                                          Repo.Hook.update(
                                            ~token,
                                            ~user,
                                            ~repo,
                                            ~id=hook.hook_id,
                                            ~hook={
                                              update_hook_config:
                                                `Web(
                                                  make_web_hook_config(
                                                    "http://example.net/",
                                                    None,
                                                  ),
                                                ),
                                              update_hook_events:
                                                Some([
                                                  `Watch,
                                                  ...hook.hook_events,
                                                ]),
                                              update_hook_active: false,
                                            },
                                            (),
                                          )
                                          >>~ (
                                            hook =>
                                              print_hooks(
                                                "Updated:",
                                                Stream.of_list([hook]),
                                              )
                                              >>= (
                                                () =>
                                                  API.set_user_agent(
                                                    "lib_test/create_hook.ml",
                                                  )
                                                  >>= (
                                                    () =>
                                                      print_hooks(
                                                        "Retrieved:",
                                                        get_hooks,
                                                      )
                                                      >>= (
                                                        () =>
                                                          Repo.Hook.delete(
                                                            ~token,
                                                            ~user,
                                                            ~repo,
                                                            ~id=hook.hook_id,
                                                            (),
                                                          )
                                                          >>~ (
                                                            () =>
                                                              print_hooks(
                                                                "Deleted:",
                                                                Stream.of_list([
                                                                  hook,
                                                                ]),
                                                              )
                                                              >>= (
                                                                () =>
                                                                  print_hooks(
                                                                    "Retrieved:",
                                                                    get_hooks,
                                                                  )
                                                                  >>= (
                                                                    () =>
                                                                    Repo.Hook.delete(
                                                                    ~token,
                                                                    ~user,
                                                                    ~repo,
                                                                    ~id=
                                                                    hook_a.
                                                                    hook_id,
                                                                    (),
                                                                    )
                                                                    >>~ (
                                                                    () =>
                                                                    print_hooks(
                                                                    "Deleted:",
                                                                    Stream.of_list([
                                                                    hook_a,
                                                                    ]),
                                                                    )
                                                                    >>= (
                                                                    () =>
                                                                    print_hooks(
                                                                    "Present:",
                                                                    get_hooks,
                                                                    )
                                                                    )
                                                                    )
                                                                  )
                                                              )
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          );
                        }
                      )
                  );
                }
              )
          )
        ),
      )
    )
  );

Lwt_main.run(t);
