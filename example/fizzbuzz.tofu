for := {i,to,iter->
  i < to ? (
    iter(i)
    for(i+1,to,iter)
  ) : 0
}

for(1,101,{i->
  fizz := i % 3 == 0
  buzz := i % 5 == 0
  println(fizz && buzz ? "FizzBuzz" :
                  fizz ? "Fizz" :
                  buzz ? "Buzz" : i)
})