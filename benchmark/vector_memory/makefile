template_size = 1000

all: clean run

define RUNNER
function dartMainRunner(main, args){
  main(process.argv.slice(2));
}
endef

export RUNNER

define INTRO
- - - - - - - -
Using (flat) list of size ${template_size}. Benchmark internally counts number of objects that fit
into 1GB heap space. Output is in bytes per one storage place. Instances of values are shared
between multiple copies of the structure we create, therefore their size do not affect the result
(much).
- - - - - - - -
endef

export INTRO

build/vector_memory.js: vector_memory.dart
	mkdir -p build
	dart2js -o build/tmp.js vector_memory.dart
	cat ../preamble/d8.js >> build/vector_memory.js
	cat build/tmp.js >> build/vector_memory.js
	echo "$$RUNNER" >> build/vector_memory.js


clean:
	rm -rf build


run: build/vector_memory.js

	@echo "$$INTRO"

	@echo -n "DartVM persistent: "
	@dart --old_gen_heap_size=1024 vector_memory.dart ${template_size} persistent > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "DartVM list: "
	@dart --old_gen_heap_size=1024 vector_memory.dart ${template_size} list > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS persistent: "
	@ (! node --max-old-space-size=1024 build/vector_memory.js ${template_size} persistent > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS list: "
	@ (! node --max-old-space-size=1024 build/vector_memory.js ${template_size} list > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

