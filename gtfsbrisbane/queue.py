from gtfsbrisbane.data_provider import HtmlDataProvider
from gtfsbrisbane.entry import Entry
from lxml import html
from xdg.BaseDirectory import xdg_data_home
import functools
import os
import shelve
import types
import urllib


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

        self.update_padding(entries, kwargs.get('delay', 0))
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

    def update_padding(self, schedule, delay):
        for x in schedule:
            x.delay = delay
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
        self.data_provider = HtmlDataProvider(api)
        self.routes = routes

    @persist
    def get_next_trains(self, fetch=False, delay=0):
        """ Returns the next scheduled trains.

        If `fetch` is true, the train schedule is loaded from the API
        instead of the persistent shelve.
        """
        return self.data_provider.get_schedule(self.routes)
