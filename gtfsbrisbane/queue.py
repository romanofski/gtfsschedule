from lxml import html
from xdg.BaseDirectory import xdg_data_home
import datetime
import functools
import itertools
import os
import shelve
import types
import urllib


class Entry:
    """A departure entry in the schedule."""

    padding = 0

    def __init__(self, route, direction, scheduled, departs):
        self.route = route
        self._direction = direction
        self._scheduled = scheduled
        self._departs = departs

    @property
    def direction(self):
        clean = self._direction.split()[:-1]
        return ' '.join(clean)

    @property
    def departs(self):
        departs_in = self.scheduled - datetime.datetime.today()
        return "{0} mins".format(int(departs_in.seconds / 60))

    @property
    def scheduled(self):
        """ Parses scheduled departure time and converts it to a
            datetime object.

            e.g. 3.45pm -> 1/12/2014 15:45
        """
        datestamp = datetime.datetime.today().strftime('%x')
        datetimestamp = "{0} {1}".format(datestamp, self._scheduled.upper())
        return (datetime.datetime.strptime(datetimestamp, '%x %I.%M%p') -
                datetime.timedelta(minutes=self.padding))

    def is_valid(self):
        """Returns True if scheduled datetime is still in the future."""
        now = datetime.datetime.today()
        return self.scheduled > now


class persist:
    """
    Small decorator which shelves the schedule entries once retrieved.
    Each script invocation deletes deprecated entries until we run out
    of them. Then it's time to fetch a new schedule.
    """

    storage_directory = 'translinkschedule'

    def __init__(self, f):
        functools.wraps(f)(self)

    def __call__(self, *args, **kwargs):
        result = []
        db = shelve.open(self.shelve_path)
        entries = []
        try:
            entries = db['hits']
        except KeyError:
            pass
        if not entries or kwargs.get('fetch', False):
            if 'hits' in db:
                del db['hits']
            entries = self.__wrapped__(*args, **kwargs)

        self.update_padding(entries, kwargs.get('padding', 0))
        db['hits'] = self.prune_queue(entries)
        db.close()
        return entries

    def __get__(self, instance, cls):
        if instance is None:
            return self
        else:
            return types.MethodType(self, instance)

    def prune_queue(self, entries):
        """Throws away old entries which are past our current datetime."""
        return [x for x in entries if x.is_valid()]

    def update_padding(self, schedule, padding):
        for x in schedule:
            x.padding = padding
        return schedule

    @property
    def base_dir(self):
        return xdg_data_home

    @property
    def shelve_dir(self):
        shelve_dir = os.path.join(self.base_dir, self.storage_directory)
        if not os.path.exists(shelve_dir):
            os.makedirs(shelve_dir)
        return shelve_dir

    @property
    def shelve_path(self):
        return os.path.join(self.shelve_dir, 'schedule')


class Queue:

    def __init__(self, api, routes):
        self.api = api
        self.routes = routes

    @persist
    def get_next_trains(self, fetch=False, padding=0):
        """ Returns the next scheduled trains.

        If `fetch` is true, the train schedule is loaded from the API
        instead of the persistent shelve.
        """
        page = html.parse(self.api)
        result = []
        for row in page.xpath("//div[@id='timetable']/table/tbody/tr"):
            route, direction, scheduled, departs, _ = (
                [x.text_content().strip() for x in row.xpath('td')]
            )
            if route not in self.routes:
                continue
            else:
                result.append(
                    Entry(route, direction, scheduled, departs)
                )
        return result
