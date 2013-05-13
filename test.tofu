counter := {->
  x := 0
  {->x = x + 1;x}
}

c := counter()
println(c())
println(c())
println(c())
println(c())

println(
  alert("Hello, World!")
)