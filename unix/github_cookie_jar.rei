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

type t;

let init: (~jar_path: string=?, unit) => Lwt.t(t);
let save: (t, ~name: string, ~auth: Github_t.auth) => Lwt.t(t);
let delete: (t, ~name: string) => Lwt.t(t);
let get_all: t => Lwt.t(list((string, Github_t.auth)));
let get: (t, ~name: string) => Lwt.t(option(Github_t.auth));

let jar_path: t => string;
