module Id where
        
newtype Id a = Id a

instance Monad Id where
  return = Id
  (Id a) >>= k = Id $ runId (k a)
  fail = error "Id: failure"

runId :: Id a -> a
runId (Id a) = a