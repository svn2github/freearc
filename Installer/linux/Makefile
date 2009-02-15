##########################################################################
# Install executables and configuration files to appropriate directories #
##########################################################################

PREFIX=/usr/local

install: uninstall
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/FreeArc
	cp bin/* $(PREFIX)/bin
	cp lib/* $(PREFIX)/lib/FreeArc
	mkdir -p /etc/FreeArc
	cp -r cfg/* /etc/FreeArc

# Install with individual configuration for every user
local: uninstall
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/FreeArc
	cp bin/* $(PREFIX)/bin
	cp lib/* $(PREFIX)/lib/FreeArc
	mkdir -p ~/.FreeArc
	cp -r cfg/* ~/.FreeArc

uninstall:
	rm -rf ~/.FreeArc
	rm -rf /etc/arc.languages
	rm -rf /etc/FreeArc
	rm -f $(PREFIX)/lib/FreeArc
	rm -f /etc/arc.groups
	rm -f /etc/arc.ini
	rm -f /etc/winarc.history
	rm -f /etc/winarc.ini
	rm -f /etc/freearc.history
	rm -f /etc/freearc.ini
	rm -f $(PREFIX)/bin/arc
	rm -f $(PREFIX)/bin/unarc
	rm -f $(PREFIX)/bin/winarc
	rm -f $(PREFIX)/bin/freearc
	rm -f $(PREFIX)/lib/*arc*.sfx
