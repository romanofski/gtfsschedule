man/gtfsschedule.1: man/gtfsschedule.1.adoc
	a2x -v --doctype manpage --format manpage $<

README.html: README.adoc ChangeLog.adoc
	asciidoc $<

src/GTFS/Realtime/Internal/Com/Google/Transit/Realtime.hs: gtfsschedule.proto
	hprotoc --lenses -p GTFS.Realtime.Internal -d src/ -u gtfsschedule.proto

gtfsschedule.proto:
	curl https://developers.google.com/transit/gtfs-realtime/gtfs-realtime.proto -o gtfsschedule.proto

.PHONY: proto
proto: src/GTFS/Realtime/Internal/Com/Google/Transit/Realtime.hs

.PHONY: docs
docs: README.html man/gtfsschedule.1

.PHONY: all
all: docs

.PHONY: clean
clean: README.html man/gtfsschedule.1
	rm -f $^
