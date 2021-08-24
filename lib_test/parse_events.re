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
open Printf;

module Http = Cohttp_lwt_unix;

type hour = {
  year: int,
  month: int,
  day: int,
  hour: int,
};

let archive_base = Uri.of_string("http://data.githubarchive.org/");

let download_hour_file = file_name => {
  let gz_name = file_name ++ ".gz";
  let uri = Uri.(resolve("", archive_base, of_string(gz_name)));
  let uri_s = Uri.to_string(uri);
  Http.Client.call(`GET, uri)
  >>= (
    ((resp, body)) => {
      open Cohttp;
      let status = Response.status(resp);
      let status_s = Code.string_of_status(status);
      if (!Code.is_success(Code.code_of_status(status))) {
        fail(
          Failure(sprintf("Retrieving %s got status %s", uri_s, status_s)),
        );
      } else {
        Lwt_io.(
          with_file(~mode=output, gz_name, oc =>
            Lwt_stream.iter_s(write(oc), Cohttp_lwt.Body.to_stream(body))
          )
        )
        >>= (
          () => {
            let gunzip = sprintf("gunzip %s", gz_name);
            Lwt_unix.system(gunzip)
            >>= Unix.(
                  fun
                  | WEXITED(0) => return(file_name)
                  | _ => fail(Failure(gunzip ++ " failed"))
                );
          }
        );
      };
    }
  );
};

let save_hour = ({year, month, day, hour}) => {
  /* so close for lexicographic sort... but they still managed to break it */
  let file_name = sprintf("%d-%02d-%02d-%d.json", year, month, day, hour);

  catch(
    () =>
      Lwt_unix.(access(file_name, [R_OK])) >>= (() => return(file_name)),
    _ => download_hour_file(file_name),
  );
};

let parse_events = (file_name, ic) => {
  let event_ss = Lwt_io.read_lines(ic);
  Lwt_stream.fold_s(
    (event_s, (k, t)) =>
      catch(
        () => {
          let t0 = Unix.gettimeofday();
          ignore(Github_j.event_of_string(event_s));
          let t1 = Unix.gettimeofday();
          return((k + 1, t +. t1 -. t0));
        },
        fun
        | Yojson.Json_error(msg) =>
          Lwt_io.eprintf(
            "%s\n\nParse failure in %s on event %d:\n%s\n",
            event_s,
            file_name,
            k,
            msg,
          )
          >>= (() => exit(1))
        | exn => fail(exn),
      ),
    event_ss,
    (1, 0.),
  );
};

let parse_hours = (~clean, hours) =>
  Lwt_list.iter_s(
    hour =>
      save_hour(hour)
      >>= (
        hour_file =>
          Lwt_io.(with_file(~mode=input, hour_file, parse_events(hour_file)))
          >>= (
            ((k, t)) => {
              let eps = float_of_int(k) /. t;
              Lwt_io.print(
                sprintf(
                  "Parsed %d events from %s successfully (%f e/s)\n",
                  k,
                  hour_file,
                  eps,
                ),
              )
              >>= (
                () =>
                  if (clean) {
                    Lwt_unix.unlink(hour_file);
                  } else {
                    return_unit;
                  }
              );
            }
          )
      ),
    hours,
  );

let day_of_hour = hour => {
  let rec next_hour = acc =>
    fun
    | 24 => acc
    | k => next_hour([{...hour, hour: k}, ...acc], k + 1);

  next_hour([], 0);
};

let parse_cmd = (year, month, day, hour, ~clean) => {
  let hours =
    switch (hour) {
    | None => day_of_hour({year, month, day, hour: 0})
    | Some(hour) => [{year, month, day, hour}]
    };

  parse_hours(~clean, hours);
};

open Cmdliner;

let cmd = {
  let doc = "the year to query";
  let docv = "YEAR";
  let year =
    Arg.(required & pos(0, some(int), None) & info([], ~docv, ~doc));
  let doc = "the 1-indexed month to query";
  let docv = "MONTH";
  let month =
    Arg.(required & pos(1, some(int), None) & info([], ~docv, ~doc));
  let doc = "the 1-indexed day to query";
  let docv = "DAY";
  let day =
    Arg.(required & pos(2, some(int), None) & info([], ~docv, ~doc));
  let doc = "the 0-indexed hour to query";
  let docv = "HOUR";
  let hour = Arg.(value & pos(3, some(int), None) & info([], ~docv, ~doc));
  let doc = "clean downloaded files after use";
  let docv = "CLEAN";
  let clean = Arg.(value & flag & info(["clean"], ~docv, ~doc));
  let doc = "attempt to parse GitHub Archive events";
  let man = [
    `S("BUGS"),
    `P("Email bug reports to <mirageos-devel@lists.xenproject.org>."),
  ];
  Term.(
    pure((y, m, d, h, clean) => Lwt_main.run(parse_cmd(y, m, d, h, ~clean)))
    $ year
    $ month
    $ day
    $ hour
    $ clean,
    info("parse_events", ~doc, ~man),
  );
};

switch (Term.eval(cmd)) {
| `Error(_) => exit(1)
| _ => exit(0)
};
