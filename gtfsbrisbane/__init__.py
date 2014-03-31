#!/usr/bin/env python3.3
__name__ = 'gtfsbrisbane'
__version__ = '0.1'
__author__ = 'Róman Joost'
__author_email__ = 'roman@bromeco.de'
__description__ = 'Shows next departing trains'

from gtfsbrisbane.config import APIURL
import argparse
import gtfsbrisbane.queue


def configure_commandline():
    parser = argparse.ArgumentParser(
        description='{name} {version} -- {description}'.format(
            name=__name__, version=__version__,
            description=__description__))
    parser.add_argument(
        "routes",
        help=("A list of routes which which should be listed."
              " e.g. 'CAIP BRSP BRIP'"),
        type=str
    )
    parser.add_argument(
        "--version",
        action="version",
        version="{name} {version}".format(
            name=__name__, version=__version__)
    )
    return parser.parse_args()


def showtrains():
    arguments = configure_commandline()
    routes = arguments.routes.split()

    queue = gtfsbrisbane.queue.Queue(APIURL, routes)
    trains = queue.get_next_trains()
    if not trains:
        print("No train data available.")
    else:
        data = ['{x.direction} - {x.departs}'.format(x=x) for x in trains[:2]]
        print(' / '.join(data))
