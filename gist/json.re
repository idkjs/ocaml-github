open Printf;
open Easy_format;

let json_string_of_int = sprintf("\027[31m%i\027[m");
let json_string_of_string = sprintf("\027[32m%s\027[m");
let json_string_of_ident = sprintf("\027[33m%s\027[m:");

let std_json_string_of_float = sprintf("\027[34m%f\027[m:");
let json_string_of_float = sprintf("\027[34m%f\027[m:");

let is_object_or_array = x =>
  switch (x) {
  | `List(_)
  | `Assoc(_) => true
  | _ => false
  };

let array = list;
let record = list;
let tuple = {
  ...list,
  space_after_opening: false,
  space_before_closing: false,
  align_closing: false,
};
let variant = {...list, space_before_closing: false};

let rec format = (std, x: Yojson.t) =>
  switch (x) {
  | `Null => [@implicit_arity] Atom("null", atom)
  | `Bool(x) => [@implicit_arity] Atom(if (x) {"true"} else {"false"}, atom)
  | `Int(x) => [@implicit_arity] Atom(json_string_of_int(x), atom)
  | `Float(x) =>
    let s =
      if (std) {
        std_json_string_of_float(x);
      } else {
        json_string_of_float(x);
      };

    [@implicit_arity] Atom(s, atom);
  | `String(s) => [@implicit_arity] Atom(json_string_of_string(s), atom)
  | `Intlit(s)
  | `Floatlit(s)
  | `Stringlit(s) => [@implicit_arity] Atom(s, atom)
  | `List([]) => [@implicit_arity] Atom("[]", atom)
  | `List(l) =>
    [@implicit_arity]
    List(("[", ",", "]", array), List.map(format(std), l))
  | `Assoc([]) => [@implicit_arity] Atom("{}", atom)
  | `Assoc(l) =>
    [@implicit_arity]
    List(("{", ",", "}", record), List.map(format_field(std), l))
  | `Tuple(l) =>
    if (std) {
      format(std, `List(l));
    } else if (l == []) {
      [@implicit_arity] Atom("()", atom);
    } else {
      [@implicit_arity]
      List(("(", ",", ")", tuple), List.map(format(std), l));
    }

  | `Variant(s, None) =>
    if (std) {
      format(std, `String(s));
    } else {
      [@implicit_arity] Atom("<" ++ json_string_of_string(s) ++ ">", atom);
    }

  | `Variant(s, Some(x)) =>
    if (std) {
      format(std, `List([`String(s), x]));
    } else {
      let op = "<" ++ json_string_of_string(s) ++ ":";
      [@implicit_arity] List((op, "", ">", variant), [format(std, x)]);
    }
  }

and format_field = (std, (name, x)) => {
  /*let s = sprintf "%s:" (json_string_of_string name) in*/
  let s = json_string_of_ident(name);
  [@implicit_arity]
  Label(([@implicit_arity] Atom(s, atom), label), format(std, x));
};

let format = (~std=false, x) =>
  if (std && !is_object_or_array(x)) {
    Yojson.json_error(
      "Root is not an object or array as requested by the JSON standard",
    );
  } else {
    format(std, (x :> Yojson.t));
  };

let to_string = (~std=?, x) =>
  Easy_format.Pretty.to_string(format(~std?, x));

let to_channel = (~std=?, oc, x) =>
  Easy_format.Pretty.to_channel(oc, format(~std?, x));
