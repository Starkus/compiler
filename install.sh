#/bin/bash

if [ "$1" = "-remove" ]
then
	rm /usr/bin/fabric
	rm -r /usr/lib/fabric
	rm -r /usr/include/fabric
	echo "Fabric compiler successfully removed"
else
	mkdir -p /usr/include/fabric/core/
	mkdir -p /usr/lib/fabric
	cp bin/Compiler		/usr/bin/fabric
	cp bin/core.o		/usr/lib/fabric/
	cp bin/core.so		/usr/lib/fabric/
	cp bin/LinuxStart.o	/usr/lib/fabric/
	cp core/*			/usr/include/fabric/core/
	echo "Fabric compiler successfully installed"
fi
