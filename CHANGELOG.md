## 3.0.0
* Remove KURE's `MonadCatch` in favor of the one from `exceptions`.
 * Redesigned `KureM` to contain both an error message and an exception.
 * Added `StrategyFailure`
 * Added `NodeMismatch` (for congruence combinator failures)
 * Added `ConditionalFailure` (a `String` plus `SomeException`)

## 2.16.12
* Derive `Typeable` instances

## 2.16.5
* Allowed building with `base-4.8.0.0`
* Removed unneeded `Monoid` constraints on `OneT` instances
