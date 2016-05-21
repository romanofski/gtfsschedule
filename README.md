GTFSBrisbane
============

Shows next departing trains. The command is intended to be invoked frequently
showing continously next departing vehicles for one particular stop.

### Preparing the data

* Download the GTFS dataset from translink

* Create the SQLite database by running:

    cat data/importCSVToSqlite.sql | sqlite3 -interactive -csv gtfs.sqlite

### Usage

Invoke the command to show next departing vehicles from your favorite stop, e.g:

    # Transport leaves in 11 minutes
    gtfsbrisbane-exe gtfs.sqlite 600248 --walktime 0
    Desitnation 11 min (13:17:00)
