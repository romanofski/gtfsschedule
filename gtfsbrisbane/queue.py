from lxml import html
from xdg.BaseDirectory import xdg_data_home
import datetime
import datetime
import os
import shelve
import urllib


class Entry:
    """A departure entry in the schedule."""

    def __init__(self, route, direction, scheduled, departs):
        self.route = route
        self._direction = direction
        self.scheduled = self._convert_scheduled_time(scheduled)
        self._departs = departs

    @property
    def direction(self):
        clean = self._direction.split()[:-1]
        return ' '.join(clean)

    @property
    def departs(self):
        departs_in = self.scheduled - datetime.datetime.today()
        return "{0} mins".format(int(departs_in.seconds / 60))

    def _convert_scheduled_time(self, scheduled):
        """ Parses scheduled departure time and converts it to a
            datetime object.

            e.g. 3.45pm -> 1/12/2014 15:45
        """
        datestamp = datetime.datetime.today().strftime('%w%m%Y')
        datetimestamp = "{0} {1}".format(datestamp, scheduled.upper())
        return datetime.datetime.strptime(datetimestamp, '%w%m%Y %I.%M%p')


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


class Queue:

    def __init__(self, api, routes):
        self.api = api
        self.routes = routes

    @persist
    def get_next_trains(number=2):
        page = html.parse(self.api)
        result = []
        for row in page.xpath("//div[@id='timetable']/table/tbody/tr"):
            route, direction, scheduled, departs, _ = (
                [x.text_content().strip() for x in row.xpath('td')]
            )
            if route not in self.routes:
                continue
            else:
                result.append(Entry(route, direction, scheduled, departs))
        return result
