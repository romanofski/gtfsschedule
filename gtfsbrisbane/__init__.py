#!/usr/bin/env python3.3
__name__ = 'gtfsbrisbane'
__version__ = '0.1'
__author__ = 'Róman Joost'
__author_email__ = 'roman@bromeco.de'
__description__ = 'Shows next departing trains'

import urllib
from lxml import html
from xdg.BaseDirectory import xdg_data_home
import os
import shelve
import datetime


APIURL = 'http://jp.translink.com.au/travel-information/network-information/stops-and-stations/stop/600029?RouteCode=&Direction=Upward'
LINES = ['BRIP', 'BRSP']


class Hit:

    def __init__(self, route, direction, scheduled, departs):
        self.route = route
        self._direction = direction
        self.scheduled = scheduled
        self._departs = departs

    @property
    def direction(self):
        clean = self._direction.split()[:-1]
        return ' '.join(clean)

    @property
    def departs(self):
        datestamp = datetime.datetime.today().strftime('%Y %b %a')
        datetimestamp = "{0} {1}".format(datestamp, self.scheduled.upper())
        scheduled = datetime.datetime.strptime(datetimestamp, '%Y %b %a %I.%M%p')
        departs_in = scheduled - datetime.datetime.today()
        return "{0} mins".format(int(departs_in / 60))


class persist:

    def __init__(self, f):
        self.f = f

    def __call__(self):
        result = []
        shelve_dir = os.path.join(xdg_data_home, 'translinkschedule')
        if not os.path.exists(shelve_dir):
            os.makedirs(shelve_dir)
        shelve_path = os.path.join(shelve_dir, 'schedule')

        db = shelve.open(shelve_path)
        hits = []
        try:
            hits = db['hits']
        except KeyError:
            pass
        if not hits:
            hits = self.f()

        # take the first 2 and shelve the rest
        result = hits[:2]
        db['hits'] = hits[2:]
        return result


@persist
def get_next_trains():
    #page = html.parse(urllib.urlopen(APIURL))
    page = html.parse(open('/tmp/600029?RouteCode=&Direction=Upward',
                           'r'))
    result = []
    for row in page.xpath("//div[@id='timetable']/table/tbody/tr"):
        route, direction, scheduled, departs, _ = (
            [x.text_content().strip() for x in row.xpath('td')]
        )
        if route not in LINES:
            continue
        else:
            result.append(Hit(route, direction, scheduled, departs))
    return result


def showtrains():
    trains = get_next_trains()
    if not trains:
        print("No train data available.")
    else:
        data = ['{x.direction} - {x.departs}'.format(x=x) for x in trains[:2]]
        print(' / '.join(data))
