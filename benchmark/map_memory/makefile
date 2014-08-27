all: map_memory.js

map_memory.js: map_memory.dart
	dart2js -o map_memory.js map_memory.dart
	echo "function dartMainRunner(main, args){" >> map_memory.js
	echo "  main(process.argv.slice(2));" >> map_memory.js
	echo "}" >> map_memory.js

clean:
	rm -f map_memory.js map_memory.js.* map_memory.precompiled.js

run:
	python2 map_memory.py > results.csv.part
	mv results.csv.part results.csv

view:
	column -t -s, < results.csv | less