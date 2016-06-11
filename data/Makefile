.PHONY: all

%.txt: SEQ_GTFS.zip
	unzip $<
	bash fixUpTimes.sh

gtfs.sqlite: %.txt
	cat importCSVToSqlite.sql | sqlite3 -interactive -csv $@ -bail

all: gtfs.sqlite

clean:
	rm -rf *.txt
