# coding: utf-8
from setuptools import setup, find_packages
from gtfsbrisbane import __author__
from gtfsbrisbane import __author_email__
from gtfsbrisbane import __description__
from gtfsbrisbane import __name__
from gtfsbrisbane import __version__


setup(
    name=__name__,
    version=__version__,
    description=__description__,
    long_description=(
        open("README.md").read() + '\n\n' +
        open("docs/CHANGES.txt").read()
    ),
    classifiers=[
        "Environment :: Console",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU General Public License (GPL)",
        "Programming Language :: Python",
        "Topic :: Internet",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
    keywords='server',
    author=__author__,
    author_email=__author_email__,
    url='',
    license='GPL',
    packages=find_packages(exclude=['ez_setup']),
    include_package_data=True,
    zip_safe=False,
    install_requires=[
        'setuptools',
        'lxml',
        'pyxdg',
    ],
    entry_points={
        'console_scripts': [
            'gtfsbrisbane = gtfsbrisbane:showtrains',
        ]
    }
)
