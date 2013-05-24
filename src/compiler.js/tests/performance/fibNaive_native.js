var naiveFib = function(n) {
  switch(n):
    case 0.0:
      return 1.0;

    case 1.0;
      return 1.0;

    default:
      return naiveFib(n-1.0) + naiveFib(n-2.0)
}

var a = naiveFib 10.0;