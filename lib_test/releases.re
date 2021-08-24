open Printf;

let token = Config.access_token;

let name_of_release =
  Github_t.(
    fun
    | {release_name: Some(name), _} => name
    | {release_name: None, _} => "NULL"
  );

let latest_release = m => {
  open Github_t;
  let name = name_of_release(m);
  eprintf(
    "latest release %Ld: %s (%s)\n%!",
    m.release_id,
    name,
    m.release_created_at,
  );
  eprintf("--\n%!");
  ();
};

let print_releases = m =>
  Github.(
    Monad.(
      Stream.iter(
        m => {
          open Github_t;
          let name = name_of_release(m);
          eprintf(
            "release %Ld: %s (%s)\n%!",
            m.release_id,
            name,
            m.release_created_at,
          );
          return();
        },
        m,
      )
      >>= (
        () => {
          eprintf("--\n%!");
          return();
        }
      )
    )
  );

let print_release_assets = m => {
  open Github_t;
  List.iter(
    x => eprintf("asset %Ld %s\n%!", x.release_asset_id, x.release_asset_url),
    m,
  );
  eprintf("--\n%!");
  ();
};

let t =
  Github.(
    Monad.(
      run(
        return(Release.for_repo(~user="mirage", ~repo="ocaml-github", ()))
        >>= print_releases
        >>= (
          () =>
            Release.get_latest(~user="mirage", ~repo="ocaml-github", ())
            >|= Response.value
            >>= (
              x => {
                latest_release(x);
                return(x);
              }
            )
            >>= (
              a =>
                Release.list_assets(
                  ~user="mirage",
                  ~repo="ocaml-github",
                  ~id=a.Github_t.release_id,
                  (),
                )
                >|= Response.value
                >|= print_release_assets
                >>= (
                  _ =>
                    return(
                      Release.for_repo(~user="mirage", ~repo="mirage", ()),
                    )
                    >>= print_releases
                )
            )
        ),
      )
    )
  );

Lwt_main.run(t);
