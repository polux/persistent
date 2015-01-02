template_size = 1000

define RUNNER
function dartMainRunner(main, args){
  main(process.argv.slice(2));
}
endef

export RUNNER

all: clean run


build/map_memory.js: map_memory.dart
	mkdir -p build
	dart2js -o build/tmp.js map_memory.dart
	cat ../preamble/d8.js >> build/map_memory.js
	cat build/tmp.js >> build/map_memory.js
	echo "$$RUNNER" >> build/map_memory.js

clean:
	rm -rf build


run: build/map_memory.js

	@echo "Using flat map of size ${template_size}."
	@echo "Keys and values are Strings of length 8. Output is in bytes per one (key, value) pair." 
	@echo "Benchmark internally counts number of objects that fit into 1GB heap space."
	@echo "- - - - - - - -"

	@echo -n "DartVM persistent: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} persistent > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "DartVM map: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} map > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "DartVM json: "
	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} json > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS persistent: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} persistent > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS map: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} map > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS json: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} json > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	rm -f build/temp.out

