##########################################################################
# Install executables and configuration files to appropriate directories #
##########################################################################

PREFIX=/usr/local

install:
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib
	cp bin/* $(PREFIX)/bin
	cp lib/* $(PREFIX)/lib
	mkdir -p ~/.FreeArc
	cp -r cfg/* /etc

# Install with individual configuration for every user
local:
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib
	cp bin/* $(PREFIX)/bin
	cp lib/* $(PREFIX)/lib
	mkdir -p ~/.FreeArc
	cp -r cfg/* ~/.FreeArc

uninstall:
	rm -rf ~/.FreeArc
	rm -rf /etc/arc.languages
	rm -f /etc/arc.groups
	rm -f /etc/arc.ini
	rm -f /etc/winarc.history
	rm -f /etc/winarc.ini
	rm -f $(PREFIX)/bin/arc
	rm -f $(PREFIX)/bin/unarc
	rm -f $(PREFIX)/bin/winarc
	rm -f $(PREFIX)/lib/*arc*.sfx
