all: run


map_memory.js: map_memory.dart
	dart2js -o map_memory.js map_memory.dart
	echo "function dartMainRunner(main, args){" >> map_memory.js
	echo "  main(process.argv.slice(2));" >> map_memory.js
	echo "}" >> map_memory.js


clean:
	rm -f map_memory.js map_memory.js.* map_memory.precompiled.js temp.out


run: map_memory.js

	@echo "OUTPUT IN BYTES PER <char[8],char[8]> ENTRY:"

	@echo -n "DartVM transient: "
	@dart --old_gen_heap_size=1024 map_memory.dart 1000 transient > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "DartVM map: "
	@dart --old_gen_heap_size=1024 map_memory.dart 1000 map > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "DartVM json: "
	@dart --old_gen_heap_size=1024 map_memory.dart 1000 json > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS transient: "
	@ ! node --max-old-space-size=1024 map_memory.js 1000 transient > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS map: "
	@ ! node --max-old-space-size=1024 map_memory.js 1000 map > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS json: "
	@ ! node --max-old-space-size=1024 map_memory.js 1000 json > temp.out 2>/dev/null
	@tail -1 temp.out

