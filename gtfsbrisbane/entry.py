import datetime


class Entry:
    """A departure entry in the schedule."""

    delay = 0

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
                datetime.timedelta(minutes=self.delay))

    def is_valid(self):
        """Returns True if scheduled datetime is still in the future."""
        now = datetime.datetime.today()
        return self.scheduled > now
