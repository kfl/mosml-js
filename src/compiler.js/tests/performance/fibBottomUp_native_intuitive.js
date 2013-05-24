var fib = function(n) {
  var x = 0.0;
  var y = 1.0;
  while n > 0.0 {
    tmp = y;
    y = x+y;
    x = y;
    n--;
  }
  return x
}

var a = fib 10.0;
var b = fib 50.0
var c = fib 80.0