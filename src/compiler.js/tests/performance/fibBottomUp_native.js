var fib = function(n) {
  var fibcalc = function(n,x,y) {
    switch(n) {
      case 0.0: return x;
      default: return fibcalc(n-1.0,y,y+x);
    }
  }
  return fibcalc(n,0.0,1.0);
}

var a = fib(10.0);
var b = fib(50.0);
var c = fib(80.0);