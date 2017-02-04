man/gtfsschedule.1: man/gtfsschedule.1.adoc
	a2x -v --doctype manpage --format manpage $<

README.html: README.adoc ChangeLog.adoc
	asciidoc $<

.PHONY: docs
docs: README.html man/gtfsschedule.1

.PHONY: all
all: docs

.PHONY: clean
clean: README.html man/gtfsschedule.1
	rm -f $^
