all: clean run


build/vector_speed.js: vector_speed.dart
	mkdir -p build
	dart2js -o build/tmp.js vector_speed.dart
	cat ../preamble/d8.js >> build/vector_speed.js
	cat build/tmp.js >> build/vector_speed.js

	dart2js -o build/vector_speed.js vector_speed.dart


clean:
	rm -rf build


run: build/vector_speed.js

	@echo "Performing several get, set, push and pop operations on flat vectors."
	@echo "The number of operations depends on the map size."

	@echo "--------"
	@echo "DartVM:"
	@dart vector_speed.dart

	@echo "--------"
	@echo "NodeJS:"
	@node build/vector_speed.js

