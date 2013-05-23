i := 0
tarai := {x,y,z->
  x <= y ? y :
    tarai(tarai(x-1,y,z),tarai(y-1,z,x),tarai(z-1,x,y))
}

println(tarai(13,7,0))