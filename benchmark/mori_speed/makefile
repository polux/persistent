all: clean run
    
init:
	npm up

compiled.js: mori_speed.js
	node_modules/closurecompiler/bin/ccjs mori_speed.js > compiled.js

run: compiled.js
	node compiled.js

clean:
	rm -f compiled.js


