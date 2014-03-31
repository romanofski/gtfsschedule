# Copyright (C) 2014, Roman Joost <roman@bromeco.de>
#
# This library is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library.  If not, see
# <http://www.gnu.org/licenses/>.
import unittest
import datetime
from gtfsbrisbane.queue import Entry


class TestEntry(unittest.TestCase):

    def test_converts_scheduled_time_to_real_datetime(self):
        entry = Entry('ISBN', 'ignore', '3.40pm', '12mins')
        self.assertIsInstance(entry.scheduled, datetime.datetime)
        self.assertEqual(15, entry.scheduled.hour)
        self.assertEqual(40, entry.scheduled.minute)
