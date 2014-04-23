GTFSBrisbane
============

Shows next departing trains with minimum of network traffic.

### Usage

Like to know the next departing trains from your favorite stop, which is
Roma St station?

    bin/gtfsbrisbane "BRIP CAIP BRSP NAIP CASP BRRW"
    Caboolture via Brisbane City to Ipswich - 11 mins / Brisbane City to Springfield - 26 mins

The script parses the HTML page for your favorite stop, and stash all
remaining, scheduled trains into a shelve in your home directory.
Repeatedly invoking the script will not fetch the website again, until all
scheduled trains are taken out of the shelve and/or are in the past.

Nominate a different stop if you're not departing from Roma St:

    bin/gtfsbrisbane -r --stop 600088 "NAIP"

Reload schedule or purge the current entries:

    bin/gtfsbrisbane -r "BRIP CAIP BRSP NAIP CASP BRRW"

You need 7 minutes to get to the station? Include it in the schedule so
you'll never miss your trian:

    bin/gtfsbrisbane "BRIP CAIP BRSP NAIP CASP BRRW" --delay=7

#### Retrieving stops, stations and train codes

Query the [translink stops and stations website](http://jp.translink.com.au/travel-information/network-information/stops-and-stations).

### Development Notes

This software is under tested and almost grown organically. So don't
expect gold standard coding style.

### Dependencies

* Python 3.4

### Motivation

I'd like to see next departing trains without pegging the translink API
every X seconds/minutes or so. This little script shows the departure
times, but keeping cached versions of the schedule.
