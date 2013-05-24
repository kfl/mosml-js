fun append a 0 list = list
  | append a n list = append a (n-1) (a::list)

val t = append 1 10000000 [];