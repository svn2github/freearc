##########################################################################
# Install executables and configuration files to appropriate directories #
##########################################################################

PREFIX=/usr/local

install: uninstall
	sudo mkdir -p $(PREFIX)/bin
	sudo mkdir -p $(PREFIX)/lib/FreeArc
	sudo cp -r bin/* $(PREFIX)/bin
	sudo cp -r lib/* $(PREFIX)/lib/FreeArc
	mkdir -p /etc/FreeArc
	cp -r cfg/* /etc/FreeArc

# Install with individual configuration for every user
local: uninstall
	sudo mkdir -p $(PREFIX)/bin
	sudo mkdir -p $(PREFIX)/lib/FreeArc
	sudo cp -r bin/* $(PREFIX)/bin
	sudo cp -r lib/* $(PREFIX)/lib/FreeArc
	mkdir -p ~/.FreeArc
	cp -r cfg/* ~/.FreeArc

uninstall:
	sudo rm -rf ~/.FreeArc
	sudo rm -rf /etc/arc.languages
	sudo rm -rf /etc/FreeArc
	sudo rm -rf $(PREFIX)/lib/FreeArc
	sudo rm -f /etc/arc.groups
	sudo rm -f /etc/arc.ini
	sudo rm -f /etc/winarc.history
	sudo rm -f /etc/winarc.ini
	sudo rm -f /etc/freearc.history
	sudo rm -f /etc/freearc.ini
	sudo rm -f $(PREFIX)/bin/arc
	sudo rm -f $(PREFIX)/bin/unarc
	sudo rm -f $(PREFIX)/bin/winarc
	sudo rm -f $(PREFIX)/bin/freearc
	sudo rm -f $(PREFIX)/lib/*arc*.sfx

