open Lwt;
open Printf;

let get_auth_token_from_jar = auth_id =>
  Github_cookie_jar.init()
  >>= (
    jar =>
      Github_cookie_jar.get(jar, ~name=auth_id)
      >>= (
        fun
        | Some(auth) => return(auth)
        | None =>
          Lwt.fail(Failure("id '" ++ auth_id ++ "' not in cookie jar"))
      )
  );

let get_tags_and_times = (~user, ~repo) => {
  let stream = Github.Repo.get_tags_and_times(~user, ~repo, ());
  Github.Stream.iter(
    ((k, v)) => {
      eprintf("%s %s\n", k, v);
      Github.Monad.return();
    },
    stream,
  );
};

Lwt_main.run(
  Github.Monad.(
    run(
      embed(get_auth_token_from_jar("test"))
      >>= (
        auth =>
          Github.(API.set_token(Token.of_auth(auth)))
          >>= (
            () =>
              get_tags_and_times(~user="dsheets", ~repo="axtls")
              >>= (
                () =>
                  get_tags_and_times(~user="ocaml", ~repo="opam")
                  >>= (
                    () =>
                      get_tags_and_times(
                        ~user="mirage",
                        ~repo="ocaml-cstruct",
                      )
                  )
              )
          )
      ),
    )
  ),
);
