## next
* Remove KURE's `MonadCatch` in favor of the one from `exceptions`.
 * Redesigned `KureM` to contain both an error message and an exception.
 * Added `MsgException` (a `String` plus `SomeException`)
 * Added `KureException` for congruence combinator failures
* Derive `Typeable` instances

## 2.16.5
* Allowed building with `base-4.8.0.0`
* Removed unneeded `Monoid` constraints on `OneT` instances
