all: clean run


vector_speed.js: vector_speed.dart
	dart2js -o vector_speed.js vector_speed.dart


clean:
	rm -f vector_speed.js vector_speed.js.* vector_speed.precompiled.js temp.out


run: vector_speed.js

	@echo "Performing several get, set, push and pop operations on flat vectors."
	@echo "The number of operations depends on the map size."

	@echo "--------"
	@echo "DartVM:"
	@dart vector_speed.dart

	@echo "--------"
	@echo "NodeJS:"
	@node vector_speed.js

