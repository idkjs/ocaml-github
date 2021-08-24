let t =
  Github.(
    Monad.(
      run(
        {
          let note = "get_token via ocaml-github";
          Token.create(~user=Config.user, ~pass=Config.pass, ~note, ())
          >>~ (
            fun
            | Result(auth) => {
                let token = Token.of_auth(auth);
                prerr_endline(Token.to_string(token));
                return();
              }
            | Two_factor(_) =>
              fail(Failure("get_token doesn't support 2fa, yet"))
          );
        },
      )
    )
  );

Lwt_main.run(t);
