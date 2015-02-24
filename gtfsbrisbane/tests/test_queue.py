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
from gtfsbrisbane.entry import Entry
from gtfsbrisbane.queue import persist
from unittest.mock import patch
from unittest.mock import Mock
import datetime
import os.path
import shutil
import tempfile
import unittest


class TestBase(unittest.TestCase):

    def setUp(self):
        self.tempdir = tempfile.mkdtemp()
        self.addCleanup(self.cleanTempDir, self.tempdir)

        patcher = patch.object(persist, 'base_dir')
        self.base_dir_mock = patcher.start()
        self.base_dir_mock.__get__ = Mock(return_value=self.tempdir)
        self.addCleanup(patcher.stop)

        patcher = patch('gtfsbrisbane.entry.datetime')
        self.mock_datetime = patcher.start()
        self.mock_datetime.datetime.today.return_value = datetime.datetime(2014, 4, 2, 16, 40, 0, 0)
        self.mock_datetime.datetime.strptime.side_effect = datetime.datetime.strptime
        self.mock_datetime.timedelta.side_effect = datetime.timedelta
        self.addCleanup(patcher.stop)

    def cleanTempDir(self, tempdir):
        shutil.rmtree(tempdir)


class TestQueue(TestBase):

    def setUp(self):
        super(TestQueue, self).setUp()
        self.schedule = os.path.join(
            os.path.dirname(__file__), 'testdata', 'romast.html')

    def test_can_parse_online_schedule(self):
        routes = ['BRSP']
        queue = Queue(self.schedule, routes)
        result = queue.get_next_trains()
        self.assertEqual(5, len(result))
        self.assertEqual(set(routes), set([x.route for x in result]))

    def test_delay_affects_train_schedule(self):
        routes = ['BRSP']
        queue = Queue(self.schedule, routes)
        #
        # The testdata entries are scheduled with the first train
        # leaving 16:51. The setup time is 16:40, so with a 12minute
        # delay, we can skip at least the first entry.
        #
        result = queue.get_next_trains(delay=12)
        self.assertEqual(4, len([x for x in result if x.is_valid()]))


class TestPersistence(TestBase):

    def test_decorator_creates_shelve_dir(self):
        self.assertFalse(os.listdir(self.tempdir))

        decorator = persist('dummy')
        decorator.shelve_dir
        self.assertEqual([decorator.storage_directory], os.listdir(self.tempdir))

    def test_decorator_retrieves_new_shelved_hits(self):
        def dummy_retrieve(numbers=2):
            return [Entry('ASDF', 'to Hell', '4.27pm')
                    for i in range(1,4)]

        decorator = persist(dummy_retrieve)
        entries = decorator()
        self.assertTrue(entries)
        self.assertIsInstance(entries[0], Entry)

    def test_decorator_prunes_old_entries(self):
        data = [Entry('OLD', 'to Hell', '4.30pm'),
                Entry('VALID', 'to Hell', '4.53pm')]
        decorator = persist('ignored')
        entries = decorator.prune_queue(data)
        self.assertEqual(1, len(entries))
        self.assertEqual('VALID', entries[0].route)
