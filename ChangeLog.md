# ChangeLog

## 0.4.0.0 (Unreleased)

Major changes:

* Tool now uses subcommands. Options of the former tool can now be found under
  `gtfsschedule monitor --help`.
* Addition of an automatic setup command. Running `gtfsschedule setup` downloads
  static dataset, unpacks and import all CSV data into an sqlite database. The
  setup is finished by overwriting the existing database.
