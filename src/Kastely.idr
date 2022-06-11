module Kastely

import Hardware.MOS6502.Emu
import Control.Monad.Reader
import Control.MonadRec

import JS.Buffer
import JS.Array
import Data.Maybe
import JS.Util

record Memory (m : Type -> Type) (a : Type) where
  constructor MkMemory
  1 runMemory : ReaderT (Array Byte) m a

Functor m => Functor (Memory m) where
  map f = { runMemory $= map f }

Applicative m => Applicative (Memory m) where
  pure x = MkMemory $ pure x
  ff <*> fx = MkMemory $ runMemory ff <*> runMemory fx

Monad m => Monad (Memory m) where
  m >>= k = MkMemory $ runMemory m >>= (runMemory . k)

MonadRec m => MonadRec (Memory m) where
  tailRecM x ini acc f = MkMemory $ tailRecM x ini acc $ \x => \st => runMemory (f x st)

HasIO m => HasIO (Memory m) where
  liftIO act = MkMemory $ liftIO act

HasIO m => MonadMachine (Memory m) where
  readMem addr = do
    mem <- MkMemory ask
    assert_total $ fromMaybe (idris_crash "readMem") <$> readIO mem (cast addr)
  writeMem addr v = do
    mem <- MkMemory ask
    writeIO mem (cast addr) v

%nomangle
public export
initialize : (String -> IO ArrayBuffer) -> IO Nat
initialize loadFile = do
  mem <- arrayDataFrom . cast {to = UInt8Array} =<< loadFile "data/program.dat"

  cpu <- new 0x438b
  let runCPU : ReaderT CPU (Memory IO) a -> IO a
      runCPU = runReaderT mem . runMemory . runReaderT cpu

  -- let loop : Nat -> ReaderT CPU (Memory IO) Nat
  --     loop cnt = getReg pc >>= \pc => case pc of
  --       0x640b => pure cnt
  --       _ => step *> loop (cnt + 1)
  -- let run = runCPU $ loop 0

  let run = runCPU $ tailRecM {rel = dummy} () 0 acc $ \(), cnt =>
          getReg pc >>= \pc => do
            consoleLog $ show pc
            case pc of
              0x640b => pure (Done cnt)
              _ => step *> pure (Cont () () $ cnt + 1)

  run
  where
    dummy : () -> () -> Type
    dummy _ _ = ()

    acc : Accessible dummy ()
    acc = assert_total $ Access $ \(), _ => acc
