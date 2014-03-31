#!/usr/bin/env python3.3
__name__ = 'gtfsbrisbane'
__version__ = '0.1'
__author__ = 'Róman Joost'
__author_email__ = 'roman@bromeco.de'
__description__ = 'Shows next departing trains'

import gtfsbrisbane
import gtfsbrisbane.queue


LINES = ['BRIP', 'BRSP']


def showtrains():
    queue = gtfsbrisbane.queue.Queue(
        urllib.urlopen(gtfsbrisbane.config.APIURL), LINES)
    trains = queue.get_next_trains()
    if not trains:
        print("No train data available.")
    else:
        data = ['{x.direction} - {x.departs}'.format(x=x) for x in trains[:2]]
        print(' / '.join(data))
