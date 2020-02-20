module Atomically
  ( runAtomically
  , commit
  ) where

-- Collect actions over the Identity monad
type Commit = State (IO ())

-- A simple state monad over the Commit monad
type Atomic s = StateT s Commit

-- | Atomically runs the contents of the TVar in a StateT monad over the Commit
-- monad. The `commit` function collects IO actions that are performed after
-- the state has been written back to the TVar.
runAtomically :: TVar s -> Atomic s () -> IO ()
runAtomically tvar action =
  join $
  atomically $ do
    state <- readTVar tvar
    let ((_, state'), commitAction) =
          runState (runStateT action state) (return ())
    writeTVar tvar state'
    return commitAction

-- Sequences the action with the stoired action and store the result.
commit' :: IO () -> Commit ()
commit' action = do
  stored <- get
  put (stored >> action)

-- | Stores the IO action to be performed after the Atomic action has finished
-- running.
commit :: IO () -> Atomic s ()
commit action = lift $ commit' action
