.PHONY: all clean install

%.txt: SEQ_GTFS.zip
	unzip $<
	bash fixUpTimes.sh

gtfs.sqlite: %.txt
	cat importCSVToSqlite.sql | sqlite3 -interactive -csv $@ -bail

install:
	mkdir -p ~/.local/share/gtfs/
	install gtfs.sqlite ~/.local/share/gtfs/gtfs.sqlite

all: gtfs.sqlite

clean:
	rm -rf *.txt
