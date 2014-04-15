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
from gtfsbrisbane.queue import Entry
from unittest.mock import patch
import datetime
import unittest


class TestEntry(unittest.TestCase):

    def setUp(self):
        self.entry = Entry('ISBN', 'ignore', '3.40pm', '12mins')

    def test_converts_scheduled_time_to_real_datetime(self):
        self.assertIsInstance(self.entry.scheduled, datetime.datetime)
        self.assertEqual(15, self.entry.scheduled.hour)
        self.assertEqual(40, self.entry.scheduled.minute)

    def test_padding_postpones_scheduled_time_by_padding_amount(self):
        self.entry.padding = 7
        self.assertEqual(33, self.entry.scheduled.minute)
