template_size = 1000

all: run

run: 

	@echo -n "NodeJS Mori persistent map: "
	@ (! node --max-old-space-size=1024 memory.js ${template_size} persistent_map > temp.out) 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS vanilla JS map: "
	@ (! node --max-old-space-size=1024 memory.js ${template_size} map > temp.out) 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS Mori persistent vector: "
	@ (! node --max-old-space-size=1024 memory.js ${template_size} persistent_vector > temp.out) 2>/dev/null
	@tail -1 temp.out

	@echo -n "NodeJS vanilla JS list: "
	@ (! node --max-old-space-size=1024 memory.js ${template_size} list > temp.out) 2>/dev/null
	@tail -1 temp.out

	rm -f temp.out

