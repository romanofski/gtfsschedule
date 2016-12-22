# ChangeLog

## 0.5.0.0 (unreleased)

Major changes:

* Supports to specify multiple stops via positional args. (Contributed
  by Fraser Tweedale)

Bugfixes:

* Shows message if no services are leaving a stop for the next couple of
  minutes.

## 0.4.0.0 (17-11-2016)

Major changes:

* Tool uses subcommands. Options of the former tool can now be found under
  `gtfsschedule monitor --help`.
* Addition of an automatic setup command. Running `gtfsschedule setup` downloads
  static dataset, unpacks and import all CSV data into an sqlite database. The
  setup is finished by overwriting the existing database.
