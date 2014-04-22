from gtfsbrisbane.data_provider import HtmlDataProvider
import unittest
import os.path


class TestHtmlDataProvider(unittest.TestCase):

    def setUp(self):
        self.schedule = os.path.join(
            os.path.dirname(__file__), 'testdata', 'romast.html')

    def test_successfully_yields_all_entries_by_default(self):
        provider = HtmlDataProvider(self.schedule)
        self.assertEqual(14, len(provider.get_schedule()))

    def test_all_entries_can_be_filtered(self):
        provider = HtmlDataProvider(self.schedule)
        self.assertEqual(5, len(provider.get_schedule(['BRSP'])))

    def test_successfully_filters_other_datasources(self):
        schedule = os.path.join(
            os.path.dirname(__file__), 'testdata', 'centralave.html')

        provider = HtmlDataProvider(schedule)
        self.assertEqual(4, len(provider.get_schedule()))
