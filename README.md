# Purescript pipes

> Port of the haskell pipes library by Gabriel Gonzalez to purescript:
> https://hackage.haskell.org/package/pipes

This port is "fairly complete", however is **lacking a port of the following**:

* The `ListT` implementation found in pipes.
* A couple of instances for the `Proxy` type in `Pipes.Internal`:
    * `MonadWriter`
    * `MonadPlus`
    * `MonadAlternative`
