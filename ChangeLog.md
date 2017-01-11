# ChangeLog

## 0.5.0.0 (unreleased)

Major changes:

* Support for automatic updates (issue 18). This removes any form of
  "offline" mode only relying on the static data as every execution
  tries to determine if the static data is up to date. If the lookup
  fails the application will print an error to stderr and continues
  relying on information of the static schedule.
* Consistently formatted if service is delayed or running ahead
* Easier to read service delays (issue #6)
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
