i := 0
tarai := {x,y,z->
  println(i = 1 + i)
  x <= y ? y :
    tarai(tarai(x-1,y,z),tarai(y-1,z,x),tarai(z-1,x,y))
}

println(tarai(10,5,0))