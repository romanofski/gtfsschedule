GTFSBrisbane
============

Shows next departing trains. The command is intended to be invoked frequently
showing continously next departing vehicles for one particular stop.

### Preparing the data

Download the GTFS dataset from translink. It's a zip file whith a gazzilion CSV
txt files in it.

At this point in time you only need stop_times.txt.

Since the text file is huge and would incur a big performance hit when used
repetitively, extract the station into a separate CSV file:

    grep ",<yourstationid>," > mystation.csv

### Usage

Invoke the command to show next departing vehicles from your favorite stop, e.g:

    # Transport leaves in 11 minutes
    gtfsbrisbane-exe --stationID 600248 --walktime 0 --stopTimesTxt mystation.csv
    11 min (13:17:00) 26 min (13:32:00)

    # Leave your spot in 5 minutes in order to make it on time while walking to
    # the stop in 7 minutes
    gtfsbrisbane-exe --stationID 600248 --walktime 7 --stopTimesTxt mystation.csv
    5 min (13:17:00) 20 min (13:32:00)

