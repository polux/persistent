all: run clean view

compile: map_memory.js

map_memory.js: map_memory.dart
	dart2js -o map_memory.js map_memory.dart
	echo "function dartMainRunner(main, args){" >> map_memory.js
	echo "  main(process.argv.slice(2));" >> map_memory.js
	echo "}" >> map_memory.js

clean:
	rm -f map_memory.js map_memory.js.* map_memory.precompiled.js

run: map_memory.js
	python2 map_memory.py > results.csv.part
	mv results.csv.part results.csv

view:
	awk -f postproces.awk results.csv | column -s, -t | less