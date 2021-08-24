let token = Config.access_token;
let user = "docker";
let repo = "for-mac";
let num = 1131;

let t =
  Github.(
    Monad.(
      run(
        {
          let issue_events = Issue.events(~token, ~user, ~repo, ~num, ());
          Stream.to_list(issue_events) >>= (_ => return());
        },
      )
    )
  );

Lwt_main.run(t);
