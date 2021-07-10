main:
	echo tmp

cosmopolitan:
	if [ -f cosmopolitan.zip ]; then \
		wget https://justine.lol/cosmopolitan/cosmopolitan.zip \
		unzip cosmopolitan.zip \
	fi;

	nim c --nolinking --nimcache:cosmocache cosmopolitan_nim.nim
