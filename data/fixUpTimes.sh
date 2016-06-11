#!/bin/bash
#
# Fixup times in order to import them into SQLite as a Time data type. This most
# likely renders the data useless, but something to solve for later.
#
set -xe
OLDIFS=$IFS

IFS=',';
for i in 25,01 26,02 27,03 28,04 29,05 30,06 31,07
do
    set -- $i;
    sed -i "s/,$1:/,$2:/g" stop_times.txt;
done

IFS=$OLDIFS
