var m = require('mori');

var template_map = {};
var template_vector = [];

// here we will store the data to prevent it from garbage collection
data = [];

// JSON.stringify & parse is not good way of cloning since it creates copies of the values; we write
// custom (shallow) clone functions instead

function clone_list(template){
    var res = [];
    for (var i=0; i<template.length; i++){
        res.push(template[i]);
    }
    return res;
}

function clone_map(template){
    var res = {};
    for (var key in template){
        res[key]=template[key];
    }
    return res;
}

var creators = {
  "persistent_map": function(){ return m.js_to_clj(template_map);},
  "map": function(){ return clone_map(template_map);},
  "persistent_vector": function(){ return m.js_to_clj(template_vector);},
  "list": function(){ return clone_list(template_vector);},
};

function run(template_size, mode) {

  // create templates for map and list
  for (var i = 0; i < template_size; i++) {
    template_map[""+(10000000+i)] = ""+(20000000+i);
  }

  for (var i = 0; i < template_size; i++) {
    template_vector.push(""+(10000000+i));
  }

  var allocated = 0;
  for(go = true; go; allocated++){
      go = false;
      var a = creators[mode]();
      data.push(a);
      go = true;
      console.log(1073741824.0 / allocated / template_size);
  }
}

function main(args){
  run(parseInt(args[0]), args[1]);
}

main(process.argv.slice(2));


