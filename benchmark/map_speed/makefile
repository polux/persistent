all: clean run


build/map_speed.js: map_speed.dart
	mkdir -p build
	dart2js -o build/tmp.js map_speed.dart
	cat ../preamble/d8.js >> build/map_speed.js
	cat build/tmp.js >> build/map_speed.js

clean:
	rm -rf build


run: build/map_speed.js

	@echo "Performing several insert, lookup and delete operations on flat maps."
	@echo "The number of operations depends on the map size."

	@echo "--------"
	@echo "DartVM:"
	@dart map_speed.dart

	@echo "--------"
	@echo "NodeJS:"
	@node build/map_speed.js

