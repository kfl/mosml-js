fun naiveFib 0.0 = 1.0
  | naiveFib 1.0 = 1.0
  | naiveFib n = naiveFib (n-1.0) + naiveFib (n-2.0)

val a = naiveFib 10.0