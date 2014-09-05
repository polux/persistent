template_size = 1000

all: run


map_memory.js: map_memory.dart
	dart2js -o map_memory.js map_memory.dart
	echo "function dartMainRunner(main, args){" >> map_memory.js
	echo "  main(process.argv.slice(2));" >> map_memory.js
	echo "}" >> map_memory.js


clean:
	rm -f map_memory.js map_memory.js.* map_memory.precompiled.js temp.out


run: map_memory.js

	@echo "Using flat map of size ${template_size}."
	@echo "Keys and values are Strings of length 8. Output is in bytes per one (key, value) pair." 
	@echo "Benchmark internally counts number of objects that fit into 1GB heap space."
	@echo "- - - - - - - -"

	@echo -n "DartVM persistent: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} persistent > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "DartVM map: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} map > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "DartVM json: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} json > temp.out 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS persistent: "
	@ (! node --max-old-space-size=1024 map_memory.js ${template_size} persistent > temp.out) 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS map: "
	@ (! node --max-old-space-size=1024 map_memory.js ${template_size} map > temp.out) 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS json: "
	@ (! node --max-old-space-size=1024 map_memory.js ${template_size} json > temp.out) 2>/dev/null
	@tail -1 temp.out

