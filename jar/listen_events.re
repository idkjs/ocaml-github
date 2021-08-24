/*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

let string_of_wiki_page_action =
  fun
  | `Created => "Created"
  | `Edited => "Edited"
  | `Unknown(cons) => "Unknown:" ++ cons;

let string_of_issue_comment_event_action =
  fun
  | `Created => "Created"
  | `Edited(_) => "Edited"
  | `Deleted => "Deleted"
  | `Unknown(cons, _json) => "Unknown:" ++ cons;

let string_of_issue = (user, repo, issue) =>
  Github_t.(
    sprintf(
      "%s/%s#%d (%s)",
      user,
      repo,
      issue.issue_number,
      issue.issue_title,
    )
  );

let string_of_issues_action = Github_j.string_of_issues_action;

let string_of_member_event_action =
  fun
  | `Added => "Added"
  | `Unknown(cons) => "Unknown:" ++ cons;

let string_of_pull = (user, repo, number) =>
  sprintf("%s/%s#%d", user, repo, number);

let string_of_pull_request_action = Github_j.string_of_pull_request_action;

let string_of_pull_request_review_comment_action =
  fun
  | `Created => "Created"
  | `Edited(_) => "Edited"
  | `Deleted => "Deleted"
  | `Unknown(cons, _json) => "Unknown:" ++ cons;

let string_of_release_event_action =
  fun
  | `Published => "Published"
  | `Unknown(cons) => "Unknown:" ++ cons;

let string_of_status_state = Github_j.string_of_status_state;

let string_of_watch_event_action =
  fun
  | `Started => "Started"
  | `Unknown(cons) => "Unknown:" ++ cons;

let print_event = event => {
  open Github_t;
  let (user, repo) =
    switch (Stringext.split(~max=2, ~on='/', event.event_repo.repo_name)) {
    | [user, repo, ..._] => (user, repo)
    | [_]
    | [] => failwith("nonsense repo name")
    };

  printf("#--> %s:", event.event_actor.user_login);
  switch (event.event_payload) {
  | `CommitComment({commit_comment_event_comment: comment}) =>
    printf(
      "CommitComment on %s/%s %s\n%!",
      user,
      repo,
      comment.commit_comment_commit_id,
    )
  | `Create({create_event_ref: `Repository, _}) =>
    printf("CreateEvent on repository %s/%s\n%!", user, repo)
  | `Create({create_event_ref: `Branch(branch), _}) =>
    printf("CreateEvent on branch %s/%s %s\n%!", user, repo, branch)
  | `Create({create_event_ref: `Tag(tag), _}) =>
    printf("CreateEvent on tag %s/%s %s\n%!", user, repo, tag)
  | `Delete({delete_event_ref: `Repository}) =>
    printf("DeleteEvent on repository %s/%s\n%!", user, repo)
  | `Delete({delete_event_ref: `Branch(branch)}) =>
    printf("DeleteEvent on branch %s/%s %s\n%!", user, repo, branch)
  | `Delete({delete_event_ref: `Tag(tag)}) =>
    printf("DeleteEvent on tag %s/%s %s\n%!", user, repo, tag)
  | `Download => printf("DownloadEvent deprecated\n%!")
  | `Follow => printf("FollowEvent deprecated\n%!")
  | `Fork({fork_event_forkee: {repository_full_name, _}}) =>
    printf("ForkEvent on %s/%s to %s\n%!", user, repo, repository_full_name)
  | `ForkApply => printf("ForkApplyEvent deprecated\n%!")
  | `Gist => printf("GistEvent deprecated\n%!")
  | `Gollum({gollum_event_pages}) =>
    printf(
      "GollumEvent on %s/%s: %s\n%!",
      user,
      repo,
      String.concat(
        ", ",
        List.map(
          ({wiki_page_title, wiki_page_action, _}) =>
            string_of_wiki_page_action(wiki_page_action)
            ++ " "
            ++ wiki_page_title,
          gollum_event_pages,
        ),
      ),
    )
  | `IssueComment({
      issue_comment_event_action,
      issue_comment_event_issue: issue,
      issue_comment_event_comment: comment,
    }) =>
    printf(
      "IssueCommentEvent %s on %s: %s\n%!",
      string_of_issue_comment_event_action(issue_comment_event_action),
      string_of_issue(user, repo, issue),
      comment.issue_comment_body,
    )
  | `Issues({issues_event_action: action, issues_event_issue: issue, _}) =>
    printf(
      "IssuesEvent on %s: %s\n%!",
      string_of_issue(user, repo, issue),
      string_of_issues_action(action),
    )
  | `Member({member_event_action, member_event_member: member}) =>
    printf(
      "MemberEvent %s on %s/%s: %s added\n%!",
      string_of_member_event_action(member_event_action),
      user,
      repo,
      member.linked_user_login,
    )
  | `Public => printf("PublicEvent on %s/%s\n%!", user, repo)
  | `PullRequest({
      pull_request_event_action: action,
      pull_request_event_number,
      _,
    }) =>
    printf(
      "PullRequestEvent on %s: %s\n%!",
      string_of_pull(user, repo, pull_request_event_number),
      string_of_pull_request_action(action),
    )
  | `PullRequestReviewComment({
      pull_request_review_comment_event_action: action,
      pull_request_review_comment_event_pull_request: pull,
      pull_request_review_comment_event_comment: comment,
    }) =>
    printf(
      "PullRequestReviewCommentEvent %s on %s: %s\n%!",
      string_of_pull_request_review_comment_action(action),
      string_of_pull(user, repo, pull.pull_number),
      comment.pull_request_review_comment_body,
    )
  | `Push({push_event_ref, push_event_size, _}) =>
    printf(
      "PushEvent on %s/%s ref %s of %d commits\n%!",
      user,
      repo,
      push_event_ref,
      push_event_size,
    )
  | `Release({release_event_action, release_event_release}) =>
    printf(
      "ReleaseEvent %s on %s/%s: %s\n%!",
      user,
      repo,
      string_of_release_event_action(release_event_action),
      release_event_release.release_tag_name,
    )
  | `Repository({
      repository_event_action,
      repository_event_repository: {repository_full_name, _},
    }) =>
    printf(
      "RepositoryEvent %s on %s\n%!",
      Github_j.string_of_repository_action(repository_event_action),
      repository_full_name,
    )
  | `Status({status_event_state, status_event_sha, _}) =>
    printf(
      "StatusEvent on %s/%s: %s %s\n%!",
      user,
      repo,
      status_event_sha,
      string_of_status_state(status_event_state),
    )
  | `Watch({watch_event_action}) =>
    printf(
      "WatchEvent %s on %s/%s\n%!",
      string_of_watch_event_action(watch_event_action),
      user,
      repo,
    )
  | `Unknown(cons, _json) => printf("UnknownEvent '%s'\n%!", cons)
  };
  return();
};

let listen = (~token, user, repo, s, ()) =>
  Lwt_io.printf("listening for events on %s/%s\n", user, repo)
  >>= (
    () => {
      let rec loop = s =>
        Github.(
          Monad.(
            Stream.poll(s)
            >>= (
              stream_opt =>
                API.get_rate_remaining(~token, ())
                >>= (
                  remaining => {
                    let now = Unix.gettimeofday();
                    switch (stream_opt) {
                    | None =>
                      embed(
                        Lwt_io.printf(
                          "%f no new events on %s/%s (%d)\n",
                          now,
                          user,
                          repo,
                          remaining,
                        ),
                      )
                      >>= (() => loop(s))
                    | Some(s) =>
                      embed(
                        Lwt_io.printf(
                          "%f new events on %s/%s (%d)\n",
                          now,
                          user,
                          repo,
                          remaining,
                        ),
                      )
                      >>= (() => loop(s))
                    };
                  }
                )
            )
          )
        );
      Github.Monad.run(loop(s));
    }
  );

let listen_events = (token, repos) => {
  let repos =
    List.map(
      r =>
        switch (Stringext.split(~max=2, ~on='/', r)) {
        | [user, repo] => (user, repo)
        | _ =>
          eprintf("Repositories must be in username/repo format");
          exit(1);
        },
      repos,
    );
  /* Get the events per repo */
  Lwt_list.iter_s(
    ((user, repo)) =>
      Github.(
        Monad.(
          run(
            {
              let events = Event.for_repo(~token, ~user, ~repo, ());
              Stream.next(events)
              >|= (
                fun
                | Some((_, s)) => async(listen(~token, user, repo, s))
                | None => assert(false)
              );
            },
          )
        )
      ),
    repos,
  )
  >>= (
    _events => {
      let (forever, _wakener) = Lwt.wait();
      forever;
    }
  );
};

let cmd = {
  let cookie = Jar_cli.cookie();
  let repos = Jar_cli.repos(~doc_append=" to query for events", ());
  let doc = "listen to events on GitHub repositories";
  let man = [
    `S("BUGS"),
    `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
  ];
  (
    Term.(
      pure((t, r) => Lwt_main.run(listen_events(t, r))) $ cookie $ repos
    ),
    Term.info("git-listen-events", ~version=Jar_version.t, ~doc, ~man),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Error(_) => exit(1)
  | _ => exit(0)
  };