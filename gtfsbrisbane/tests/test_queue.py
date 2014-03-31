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
from gtfsbrisbane.queue import Queue
import datetime
import os.path
import unittest
import functools


class TestEntry(unittest.TestCase):

    def setUp(self):
        self.schedule = os.path.join(
            os.path.dirname(__file__), 'testdata', 'romast.html')

    def test_can_parse_online_schedule(self):
        routes = ['BRSP']
        queue = Queue(self.schedule, routes)
        result = queue.get_next_trains()
        self.assertEqual(2, len(result))
        self.assertEqual(routes[0],
                         functools.reduce(lambda x, y: x.route in routes and x.route, result)
                        )
