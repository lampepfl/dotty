    type Proc = (() -> Unit)
     def foo(f: {*} Proc): {} Proc =
       def bar[A <: {f} Proc](g: () -> A): () -> {f} Proc =
         g
       val stowaway: () -> {f} Proc =
         bar( () => f )  // error
       () => { stowaway.apply().apply() }