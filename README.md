GTFS-Schedule
=============

Shows next departing trains. The command is intended to be invoked frequently
showing continously next departing vehicles for one particular stop.

### Preparing the data

* Go to the data directory `gtfsschedule/data`

* Download the GTFS dataset from Translink into the directory

* Run `make`. This should:

    * unpack the zip file
    * fix up the times
    * import the CSV files into an sqlite database
    
* Move the database to a location of your choice, e.g. `$HOME/.config/gtfs/`

### Usage

Invoke the command to show next departing vehicles from your favorite stop, e.g:

    # Transport leaves in 11 minutes
    gtfsschedule gtfs.sqlite 600248 --walktime 0
    Desitnation 11 min (13:17:00)

    # Transport leaves in 11 minutes, factor in a walk time to the stop/station of 7 minutes
    gtfsschedule gtfs.sqlite 600248 --walktime 7

    # Factor in realtime updates
    gtfsschedule gtfs.sqlite 600248 --walktime 7 -r
    
