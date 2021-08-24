open Lwt;
open Printf;

let token = Config.access_token;
let user = "mirage";
let repo = "ocaml-github";

let t = {
  open Github;

  let pp_sep = (fmt, ()) => Format.fprintf(fmt, "; ");

  let print_yearly_stats = stats => {
    Format.printf("Yearly commit activity.@.");

    List.iter(
      x => {
        Format.printf(
          "{ days: [ %a ]",
          Format.pp_print_list(~pp_sep, Format.pp_print_int),
          x.Github_t.commit_activity_days,
        );
        Format.printf(
          ", total: %i, week: %i }@.",
          x.Github_t.commit_activity_total,
          x.Github_t.commit_activity_week,
        );
      },
      stats,
    );
  };

  let print_weekly_activity = stats => {
    let week = x =>
      Format.printf(
        " [ %a ]@.",
        Format.pp_print_list(~pp_sep, Format.pp_print_int),
        x,
      );

    Format.printf("Testing weekly commit activity.@.");
    List.iter(x => week(x), stats);
  };

  let print_weekly_count = weekly_count => {
    Format.printf("Testing weekly commit count.@.");
    Format.printf(
      " all: [ %a ]@.",
      Format.pp_print_list(~pp_sep, Format.pp_print_int),
      weekly_count.Github_t.participation_all,
    );
    Format.printf(
      " owner: [ %a ]@.",
      Format.pp_print_list(~pp_sep, Format.pp_print_int),
      weekly_count.Github_t.participation_owner,
    );
  };

  let print_hourly_stats = stats => {
    let week = x =>
      Format.printf(
        " [ %a ]@.",
        Format.pp_print_list(~pp_sep, Format.pp_print_int),
        x,
      );

    Format.printf("Testing hourly commit activity.@.");
    List.iter(x => week(x), stats);
  };

  Monad.(
    run(
      {
        let frequency = Stats.yearly_commit_activity(~token, ~user, ~repo, ());
        Stream.to_list(frequency);
      },
    )
  )
  >>= (
    fun
    | [] => {
        printf("No yearly stats found OR data not yet computed and cached.");
        return();
      }
    | yearly_stats => {
        print_yearly_stats(yearly_stats);

        Monad.(
          run(
            {
              let frequency =
                Stats.weekly_commit_activity(~token, ~user, ~repo, ());
              Stream.to_list(frequency);
            },
          )
        )
        >>= (
          fun
          | [] => {
              printf(
                "No contributors found OR data not yet computed and cached.",
              );
              return();
            }
          | stats => {
              print_weekly_activity(stats);

              Monad.(
                run(
                  Stats.weekly_commit_count(~token, ~user, ~repo, ())
                  >|= Response.value,
                )
              )
              >>= (
                weekly_count => {
                  print_weekly_count(weekly_count);

                  Monad.(
                    run(
                      {
                        let frequency =
                          Stats.hourly_commit_count(~token, ~user, ~repo, ());
                        Stream.to_list(frequency);
                      },
                    )
                  )
                  >>= (
                    fun
                    | [] => {
                        printf(
                          "No punch cards found OR data not yet computed and cached.",
                        );
                        return();
                      }
                    | stats => {
                        print_hourly_stats(stats);

                        return();
                      }
                  );
                }
              );
            }
        );
      }
  );
};

try(Lwt_main.run(t)) {
| [@implicit_arity] Github.Message(_, message) =>
  eprintf("GitHub API error: %s\n", Github.API.string_of_message(message));
  exit(1);
};
