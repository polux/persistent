var template = {};
var data = Array(100000);

var creators = {

  "persistent": function () { throw "NO WAY !"},

  "transient": function (){ throw "NO WAY !" },
  
  "json": function (){ return JSON.stringify(template) },

  "map": function (){ return JSON.parse(JSON.stringify(template)) }, 
};

function run(template_size, mode) {
  
  for (var i = 0; i < template_size; i++) {
    template[("        " + i).slice(-8)] = ("        " + i).slice(-8);
  }
  
  var allocated = 0;
  for(var go = true; go; allocated++){
    try{
      console.log(allocated);
      go = false;
      var a = creators[mode]();
      data[allocated] = a;
      go = true;
      
    } catch(e) {
      data = null;
      console.error(e)
    }
  }
}

function main(){
  
  run(parseInt(process.argv[2]), process.argv[3]);
}

main()