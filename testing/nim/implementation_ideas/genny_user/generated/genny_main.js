var ffi = require('ffi-napi');
var Struct = require("ref-struct-napi");

var dll = {};

function genny_mainException(message) {
  this.message = message;
  this.name = 'genny_mainException';
}

const Obj = Struct({
  'field':'int64'
})
obj = function(field){
  var v = new Obj();
  v.field = field
  return v;
}
Obj.prototype.isEqual = function(other){
  return self.field == other.field;
};

Obj.prototype.getData = function(){
  result = dll.genny_main_obj_get_data(this)
  return result
}


var dllPath = ""
if(process.platform == "win32") {
  dllPath = __dirname + '/genny_main.dll'
} else if (process.platform == "darwin") {
  dllPath = __dirname + '/libgenny_main.dylib'
} else {
  dllPath = __dirname + '/libgenny_main.so'
}

dll = ffi.Library(dllPath, {
  'genny_main_obj_get_data': ['int64', [Obj]],
});

exports.Obj = Obj;
exports.obj = obj;
