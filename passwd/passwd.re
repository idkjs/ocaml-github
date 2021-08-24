let get = (~prompt) =>
  Lwt.(
    Lwt_unix.(
      tcgetattr(stdin)
      >>= (
        term_io =>
          tcsetattr(stdin, TCSANOW, {...term_io, c_echo: false})
          >>= (
            () =>
              Lwt_io.print(prompt)
              >>= (
                () =>
                  Lwt_io.read_line(Lwt_io.stdin)
                  >>= (
                    input =>
                      tcsetattr(stdin, TCSANOW, term_io) >|= (() => input)
                  )
              )
          )
      )
    )
  );

let get_if_unset = (~prompt) =>
  fun
  | None => get(~prompt)
  | Some(p) => Lwt.return(p);
