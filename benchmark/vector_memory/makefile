template_size = 1000

all: run


build/vector_memory.js: vector_memory.dart
	mkdir -p build
	dart2js -o build/vector_memory.js vector_memory.dart
	echo "function dartMainRunner(main, args){" >> build/vector_memory.js
	echo "  main(process.argv.slice(2));" >> build/vector_memory.js
	echo "}" >> build/vector_memory.js


clean:
	rm -rf build


run: build/vector_memory.js

	@echo "Using flat list of size ${template_size}."
	@echo "Values are Strings of length 8. Output is in bytes per one value." 
	@echo "Benchmark internally counts number of objects that fit into 1GB heap space."
	@echo "- - - - - - - -"

	@echo -n "DartVM persistent: "
	@dart --old_gen_heap_size=1024 vector_memory.dart ${template_size} persistent > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "DartVM list: "
	@dart --old_gen_heap_size=1024 vector_memory.dart ${template_size} list > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "DartVM json: "
	@dart --old_gen_heap_size=1024 vector_memory.dart ${template_size} json > build/temp.out 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS persistent: "
	@ (! node --max-old-space-size=1024 build/vector_memory.js ${template_size} persistent > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS list: "
	@ (! node --max-old-space-size=1024 build/vector_memory.js ${template_size} list > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

	@echo -n "NodeJS json: "
	@ (! node --max-old-space-size=1024 build/vector_memory.js ${template_size} json > build/temp.out) 2>/dev/null
	@tail -1 build/temp.out

