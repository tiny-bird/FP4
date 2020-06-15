{-# LANGUAGE Safe #-}


module Control.Asynchronous
  ( ForeignThreadInterface
  , Fork
  , fork
  , join
  , sync
  )
where


import           Control.Concurrent
  ( ThreadId
  , forkFinally
  )
import           Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  )
import           GHC.Exception
  ( SomeException
  , throw
  )


data Fork a = Fork !ThreadId (IO (Either SomeException a))


class Monad m => ASYNC                  m where
  new :: m (MVar a)
  put ::    MVar a -> a -> m ( )
  get ::    MVar a ->      m  a
class ASYNC m => ForeignThreadInterface m where
  fork ::   m    a   -> m (Fork a)
  join ::   Fork a   -> m       a
  sync :: [ Fork a ] -> m      ( )


instance ASYNC                  IO where
  -- newEmptyMVar: Create an MVar which is initially empty.
  new = newEmptyMVar
  -- putMVar: Put a value into an MVar.
  put = putMVar
  -- readMVar: Atomically read the contents of an MVar. If the MVar is currently
  -- empty, readMVar will wait until it is full. readMVar is guaranteed to
  -- receive the next putMVar.
  get = readMVar
instance ForeignThreadInterface IO where
  fork = \ compute ->
    new                               >>= \ var ->
    forkFinally compute (finally var) >>= \ tid ->
    pure $ Fork tid $ get var
    where
      finally v = \ r -> put v r
  join (Fork _ mvar) =
    mvar >>= \ var ->
    case var of
      Right v -> pure  v
      Left  e -> throw e
  sync =
    mapM_ join