all: clean run


vector_speed.js: vector_speed.dart
	dart2js -o vector_speed.js vector_speed.dart


clean:
	rm -f vector_speed.js vector_speed.js.* vector_speed.precompiled.js temp.out


run: vector_speed.js

	@echo "Performing several insert, lookup and delete operations on flat maps."
	@echo "The number of operations depends on the map size."

	@echo "--------"
	@echo "DartVM:"
	@dart vector_speed.dart

	@echo "--------"
	@echo "NodeJS:"
	@node vector_speed.js

