open Printf;

let token = Config.access_token;
let user = "ocaml";
let repo = "opam";

let t =
  Github.(
    Monad.(
      Github_t.(
        run(
          {
            let labels = Label.for_repo(~token, ~user, ~repo, ());
            printf("labels for %s/%s\n\n", user, repo);
            Stream.iter(
              label => {
                let name = label.label_name;
                printf("%s\n", name);
                return();
              },
              labels,
            );
          },
        )
      )
    )
  );

Lwt_main.run(t);
