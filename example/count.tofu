counter := {->
  x := 0
  {-> x = x + 1}
}

c := counter()
println(c())
println(c())
