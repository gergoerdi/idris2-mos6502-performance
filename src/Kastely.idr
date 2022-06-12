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

single : (HasIO m) => Nat -> ReaderT CPU (Memory m) (Maybe Nat)
single cnt = do
  getReg pc >>= \pc' => do
    -- consoleLog $ show pc'
    case pc' of
      0x640b => do -- Menu
        let cmd = 0x07
        writeMem 0x680d cmd
        setReg regA cmd
        rts
        pure Nothing
      0x4a07 => do -- Check disk
        setReg pc 0x40bb
        pure Nothing
      0xcc03 => do -- Load from disk
        pure (Just cnt)
      0x4679 => do -- Show message from 0xcb4a, length 36
        rts
        pure Nothing
      _ => do
        step
        pure Nothing

loop : (HasIO m) => ReaderT CPU (Memory m) Nat
loop = go 0
  where
    go : Nat -> ReaderT CPU (Memory m) Nat
    go cnt = do
      Nothing <- single cnt
        | Just v => pure v
      go (cnt + 1)

%nomangle
public export
initialize : (String -> IO ArrayBuffer) -> IO Nat
initialize loadFile = do
  mem <- arrayDataFrom . cast {to = UInt8Array} =<< loadFile "data/program.dat"

  cpu <- new 0x438b
  let runCPU : ReaderT CPU (Memory IO) a -> IO a
      runCPU = runReaderT mem . runMemory . runReaderT cpu

  runCPU loop
