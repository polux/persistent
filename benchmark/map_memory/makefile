template_size = 1000

define RUNNER
function dartMainRunner(main, args){
  main(process.argv.slice(2));
}
endef

export RUNNER

define INTRO
- - - - - - - -
Using (flat) map of size ${template_size}. Benchmark internally counts number of objects that fit
into 1GB heap space. Output is in bytes per one key->value storage place. Instances of keys and
values are shared between multiple copies of the structure we create, therefore their size do not
affect the result (much). 
- - - - - - - -
endef

export INTRO


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

	@echo "$$INTRO"

#	@echo -n "DartVM persistent: "
#	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} persistent > build/temp.out 2>/dev/null
#	@tail -1 build/temp.out
#
#	@echo -n "DartVM map: "
#	@dart --old_gen_heap_size=1024 map_memory.dart ${template_size} map > build/temp.out 2>/dev/null
#	@tail -1 build/temp.out

	@echo -n "NodeJS persistent: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} persistent > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS map: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} map > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS map: "
	@ (! node --max-old-space-size=1024 build/map_memory.js ${template_size} hashmap > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	rm -f build/temp.out

