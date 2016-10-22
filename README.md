GTFS-Schedule
=============

Shows next departing services from a single stop.

### Motivation

The command is intended to be invoked frequently in a status bar (such as
(Xmobar)[https://github.com/jaor/xmobar]) showing next departing services for
one particular stop.

The command shows remaining minutes in order to leave the current spot (e.g.
office) and to make the service with minimizing the amount to wait for the
service.

### Preparing the data

Run the setup command like:

    gtfsschedule setup
    
The database is created by default in: `$HOME/.local/share/gtfs/`

### Usage

Find out which stop code it is you need to monitor. For example this is
Brisbanes Roma St, platform 8 stop code:
(600029)[https://jp.translink.com.au/plan-your-journey/stops/600029].

Invoke the command to show next departing services from your favorite stop, e.g:

    # Transport leaves in 11 minutes
    gtfsschedule monitor 600248 --walktime 0
    Desitnation 11 min (13:17:00)

    # Transport leaves in 11 minutes, factor in a walk time to the stop/station of 7 minutes
    gtfsschedule monitor 600248 --walktime 7

    # Factor in realtime updates
    gtfsschedule monitor 600248 --walktime 7 -r
