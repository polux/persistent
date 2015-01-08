var Benchmark = require('benchmark');
var m = require('mori');

var samples = [
	{"500":60, "1000":30, "1500":20, "3000":10}
]

var times = 10;

// Read Map

var read_map_suite = new Benchmark.Suite;

for (var sample_id = 0; sample_id < samples.length; sample_id++){

	var sample = samples[sample_id];
	var objects = {}
	
	for(var size_name in sample){
		var size = Number(size_name)
		var count = sample[size_name]

		var prototype = []
		for (var i = 0; i < size; i++){
			prototype.push(String(i*i))
			prototype.push("foo");
		}

		subobjects = [];
		for (var i = 0; i < count; i++){
			subobjects.push(m.hash_map.apply(null, prototype));
		}

		objects[size_name] = subobjects;
	}

	read_map_suite.add('Map Read on '+JSON.stringify(sample), function() {
	  for(var size_name in sample){
	  	var size = Number(size_name)
		
		objects[size_name].forEach(function(map){
			for (var i = size; i >= 0; i--) {
				m.get(map, i*i, "poo");
			}
			for (var i = 0; i <= size; i++) {
				m.get(map, i*i, "poo");
			}
		});

	  }
	}, {"minSamples": times});

}



// Write Map

var write_map_suite = new Benchmark.Suite;

for (var sample_id = 0; sample_id < samples.length; sample_id++){

	var sample = samples[sample_id];

	write_map_suite.add('Map Write on '+JSON.stringify(sample), function() {
	  for(var size_name in sample){
	  	var size = Number(size_name);
	  	var count = sample[size_name];

		for (var j = 0; j < count; j++){
			var map = m.hash_map();
			for(var val in ["foo", "bar", "baz", "woo", "hoo", "goo", "wat"]){
				for (var i = 0; i < size; i++) {
					map = m.assoc(map, i*i, val);
				}
			}
			for (var i = 0; i < size; i++) {
				map = m.dissoc(map, i*i);
			}
		}
	  }
	}, {"minSamples": times});

}




// Run

var logger = function(event) {
	var error = event.target.error,
		time = (1/event.target.hz * 1000000);
		stats = event.target.stats,
		size = stats.sample.length,
		result = event.target.name;

	if (error) {
		result += ': ' + join(error);
	} else {
		result += ': ' + time.toFixed(0) + ' us +/-' +
		stats.rme.toFixed(2) + '% (' + size + ' runs sampled)';
	}
	console.log(result);
}

read_map_suite.on('cycle', logger).run();
write_map_suite.on('cycle', logger).run();
