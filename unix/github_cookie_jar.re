/*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
open Lwt;

type t = {jar_path: string};

exception InvalidName(string);

let invalid_names =
  Re.(
    List.map(
      compile,
      [
        seq([bos, str(".")]),
        str("../"),
        seq([bos, str(Filename.dir_sep)]),
        seq([str(Filename.dir_sep), eos]),
      ],
    )
  );

let jar_path = ({jar_path}) => jar_path;

let file_kind_match = (path, ~reg, ~dir, ~other) =>
  Lwt_unix.(
    stat(path)
    >>= (
      ({st_kind, _}) =>
        switch (st_kind) {
        | S_REG => reg()
        | S_DIR => dir()
        | S_CHR
        | S_BLK
        | S_LNK
        | S_FIFO
        | S_SOCK => other()
        }
    )
  );

let rec mkdir_p = dir =>
  Sys.file_exists(dir)
    ? return()
    : mkdir_p(Filename.dirname(dir)) >>= (() => Lwt_unix.mkdir(dir, 0o700));

let rec init = (~jar_path=?, ()) => {
  let jar_path =
    switch (jar_path) {
    | None =>
      let home =
        try(Sys.getenv("HOME")) {
        | Not_found => "."
        };
      let basedir = Filename.concat(home, ".github");
      Filename.concat(basedir, "jar");
    | Some(jar_path) => jar_path
    };

  Sys.file_exists(jar_path)
    ? return({jar_path: jar_path})
    : {
      printf("Github cookie jar: initialized %s\n", jar_path);
      mkdir_p(jar_path) >>= init(~jar_path);
    };
};

/* Save an authentication token to disk, under the [name]
 * file in the jar */
let save = ({jar_path} as jar, ~name, ~auth) =>
  (
    if (List.exists(re => Re.execp(re, name), invalid_names)) {
      fail(InvalidName(name));
    } else {
      return();
    }
  )
  >>= (
    () => {
      let rec backup_path = (~dirok=false, name) => {
        let fullname = Filename.concat(jar_path, name);
        let backup = () => {
          open Unix;
          let tm = gmtime(gettimeofday());
          let backfname =
            sprintf(
              "%s.%.4d%.2d%.2d.%2d%2d%2d.bak",
              name,
              1900 + tm.tm_year,
              1 + tm.tm_mon,
              tm.tm_mday,
              tm.tm_hour,
              tm.tm_min,
              tm.tm_sec,
            );
          let fullback = Filename.concat(jar_path, backfname);
          printf(
            "Github cookie jar: backing up\n%s -> %s\n",
            fullname,
            fullback,
          );
          Lwt_unix.rename(fullname, fullback);
        };

        catch(
          () =>
            file_kind_match(
              fullname,
              ~reg=backup,
              ~dir=if (dirok) {return} else {backup},
              ~other=backup,
            ),
          fun
          | [@implicit_arity] Unix.Unix_error(Unix.ENOENT, _, _)
          | [@implicit_arity] Unix.Unix_error(Unix.ENOTDIR, _, _) =>
            switch (Filename.dirname(name)) {
            | "." => return()
            | parent => backup_path(~dirok=true, parent)
            }
          | exn => fail(exn),
        );
      };

      backup_path(name)
      >>= (
        () => {
          let fullname = Filename.concat(jar_path, name);
          mkdir_p(Filename.dirname(fullname))
          >>= (
            () => {
              let auth_fd =
                Unix.(
                  openfile(fullname, [O_CREAT, O_TRUNC, O_WRONLY], 0o600)
                );
              let auth_oc = Unix.out_channel_of_descr(auth_fd);
              fprintf(auth_oc, "%s", Github_j.string_of_auth(auth));
              close_out(auth_oc);
              printf("Github cookie jar: created %s\n", fullname);
              return(jar);
            }
          );
        }
      );
    }
  );

/* Delete an authentication token from disk, given the [name] in the jar */
let delete = (jar, ~name) =>
  if (List.exists(re => Re.execp(re, name), invalid_names)) {
    fail(InvalidName(name));
  } else {
    Lwt_unix.unlink(Filename.concat(jar.jar_path, name))
    >>= (() => return(jar));
  };

/* Read a JSON auth file in and parse it */
let read_auth_file = ({jar_path}, name) => {
  let fname = Filename.concat(jar_path, name);
  let {Unix.st_perm, _} = Unix.stat(fname);
  let safe_perm = 0o7770 land st_perm;
  if (safe_perm != st_perm) {
    Unix.chmod(fname, safe_perm);
  };
  Lwt_io.with_file(~mode=Lwt_io.input, fname, ic =>
    Lwt_stream.fold_s((b, a) => return(a ++ b), Lwt_io.read_lines(ic), "")
    >>= (buf => return(Github_j.auth_of_string(buf)))
  );
};

/* Retrieve all the cookies */
let get_all = ({jar_path} as jar) => {
  let rec traverse = dir => {
    let base = Filename.concat(jar_path, dir);
    let files = Lwt_unix.files_of_directory(base);
    Lwt_stream.fold_s(
      (b, a) =>
        if (b == "." || b == "..") {
          return(a);
        } else {
          let path = Filename.concat(base, b);
          let ident = Filename.concat(dir, b);
          file_kind_match(
            path,
            ~reg=
              () =>
                read_auth_file(jar, ident)
                >>= (auth => return([(ident, auth), ...a])),
            ~dir=() => traverse(ident) >>= (sub => return(sub @ a)),
            ~other=() => return(a),
          );
        },
      files,
      [],
    );
  };
  traverse("");
};

/* Get one cookie by name */
let get = (jar, ~name) =>
  catch(
    () => read_auth_file(jar, name) >>= (auth => return(Some(auth))),
    _ => return_none,
  );
