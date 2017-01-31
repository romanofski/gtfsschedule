GTFS-Schedule
=============

Be on time for your next public transport service.

## Motivation

>> What time do I need to leave the office to minimize waiting time for
>> my train?

The command is intended to be invoked frequently in a status bar (such as
[xmobar](https://github.com/jaor/xmobar)) showing next departing services for
one particular stop.

The command shows remaining minutes in order to leave the current spot (e.g.
office) to minimize the waiting time at the bus or train stop.

### Non Goals

This programs main goal isn't a:

* a route planner


## Installation

The tool currently only supports [SEQ
Translink](https://gtfsrt.api.translink.com.au) datasets. See [issue
12](https://github.com/romanofski/gtfsschedule/issues/12) for supporting
more.

### From source

Haskell provides a tool called
[stack](https://docs.haskellstack.org/en/stable/README/) which can be installed
on many systems.

Using `stack` you can compile and install:

    stack install

in your local folder (`~/.local`). Make sure it is in your `$PATH`.

### Packages

Experimental packages are available for Fedora 24 via
[copr](https://copr.fedorainfracloud.org/coprs/romanofski/gtfsschedule/).

## Using

### Preparing the data

Run the setup command like:

    gtfsschedule setup --static-url https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip

The database is created by default in: `$HOME/.local/share/gtfs/`

### Invoking the command

Find out which stop code it is you need to monitor from your service provider.

As an example this is Brisbane's Roma St, platform 8 stop code:
[600029](https://jp.translink.com.au/plan-your-journey/stops/600029).

Once you have your stop code, invoke the command to show next departing services:

    # Transport leaves in 11 minutes
    gtfsschedule monitor 600248
    Desitnation 11 min (13:17:00)

### Using a count down

If you want to know when to leave your position in order to reach the service
upon departure time, you can specify a delay:

    # Transport leaves in 11 minutes, factor in a walk time to the stop/station of 7 minutes
    gtfsschedule monitor 600248+7

    # Include in realtime updates with possible delays
    gtfsschedule monitor 600248+7 --realtime-url http://gtfsrt.api.translink.com.au/Feed/SEQ

### Monitoring multiple stops

You can specify multiple stops with different delays to reach them:

    gtfsschedule monitor 600248+7 600249+5

### Schedule change indicators format

When you run the program with realtime updates, the following changes are indicated as follows:

    # Delayed service
    !GTPB Destination 11 min (13:17:06 (46s)) <-- delay
                              ^ departure time including delay

    # Canceled service
    GTPB Destination 11 min (13:17:00 !CANC!)


### Updating the database

If you run the gtfsschedule tool with the `static-url` option, it automatically checks if the local dataset is up to date and prints a warning if it isn't. Simply run:

    gtfsschedule setup --static-url https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip

to update download a new dataset and update your database.

### Automatically keeping the database up-to-date

You can invoke `gtfsschedule monitor` with `-u` to keep your static dataset up-to-date:

    gtfsschedule monitor -u --static-url https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip 600029

## Configuration file

A configuration file helps with making the use of the command line tool easier,
especially if you're always receiving updates from the same API and the dataset
from the same URL. The command line options and arguments have precedence over
the configuration file however.

The configuration file should be placed in `~/.conf/gtfs/config.cfg` and
supports setting the URLs to the realtime API and the static dataset. For
example, for Brisbane the config file would look:

    [default]
    static-url = https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip
    realtime-url = http://gtfsrt.api.translink.com.au/Feed/SEQ

The format of the configuration file is documented here: [https://hackage.haskell.org/package/ini/docs/Data-Ini.html]

Note: I have not tested how well the support works with other GTFS feed APIs
than Translink (QR) provides. Feedback is very welcome.

### Config examples for status monitors

Xmobar:

    Run Com "gtfsschedule" ["monitor", "600248"] "gtfs" 600

Poor mans statusbar with `watch`. Use a terminal window and:

    watch -n 60 "gtfsschedule monitor 600248+7"
