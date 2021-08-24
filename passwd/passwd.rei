let get: (~prompt: string) => Lwt.t(string);

let get_if_unset: (~prompt: string, option(string)) => Lwt.t(string);
