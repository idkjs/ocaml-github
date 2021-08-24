open Lwt;
open Printf;

let token = Config.access_token;

let print_deploy_keys = m => {
  List.iter(
    m =>
      Github_t.(
        eprintf(
          "title %Ld: %s (%s)\n%!",
          m.deploy_key_id,
          m.deploy_key_title,
          m.deploy_key_key,
        )
      ),
    m,
  );
  eprintf("--\n%!");
};

let t = {
  let k =
    Github.Deploy_key.for_repo(
      ~token,
      ~user="mirage",
      ~repo="mirage-www-deployment",
    );
  Github.(Monad.run(Stream.to_list(k()))) >|= print_deploy_keys;
};

let _ = Lwt_main.run(t);
