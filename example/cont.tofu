#Hello
return := {x-> #return
  {k->
    k(x) #ok?
  }
}
bind := {c,f->
  {k->
    c({x->
      f(x)(k)
    })
  }
}
run := {c,x->
  c(x)
}

run(bind(return(2),
 {x->{f->f(x*x)}}),
 {x->println(x)})
