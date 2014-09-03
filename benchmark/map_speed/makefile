all: run


map_speed.js: map_speed.dart
	dart2js -o map_speed.js map_speed.dart


clean:
	rm -f map_speed.js map_speed.js.* map_speed.precompiled.js temp.out


run: map_speed.js

	@echo "Performing several insert, lookup and delete operations on flat maps."
	@echo "The number of operations depends on the map size."

	@echo "--------"
	@echo "DartVM:"
	@dart map_speed.dart

	@echo "--------"
	@echo "NodeJS:"
	@node map_speed.js

