## 2.16.5
* Allowed building with GHC 7.10
* Due to the `Applicative`-`Monad` Proposal in GHC 7.10, the `Monad` and `MonadCatch` instances for `OneT w m` now require a `Monoid w` constraint, since the `Applicative` instance for `OneT w m` also requires `Monoid w`. This also requires the `oneT`, `onetdT`, `onebuT`, and `oneLargestT` functions to have a `Monoid` constraint.
