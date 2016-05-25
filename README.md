# Purescript pipes

> Port of the haskell pipes library by Gabriel Gonzalez to purescript:
> https://hackage.haskell.org/package/pipes

This port is "fairly complete", however is **lacking a port of the following**:

* The `ListT` implementation found in pipes.
* The monad morph instances. I think this could be added fairly easily thanks to
  [purescript-mmorph](https://github.com/Thimoteus/purescript-mmorph).
