module Kastely

import Hardware.MOS6502.Emu
import Kastely.Text

import Data.Maybe
import Data.String

import Data.Array.Fast
import JS.Buffer
import JS.Array
import JS.Util

0 Memory : Type
Memory = Array Byte

toMachine : Memory -> Machine
toMachine mem = MkMachine
  { readMem_  = \addr => readArray mem (cast addr)
  , writeMem_ = \addr => writeArray mem (cast addr)
  }

untilIO : acc -> (acc -> IO (Either acc a)) -> IO a
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO a
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

copyToMemory : Machine => Memory -> Addr -> Addr -> IO ()
copyToMemory from start target = do
  n <- cast <$> sizeIO from
  untilIO 0 $ \i => do
    let src = start + i
        dest = target + i
    if src >= n then pure $ Right () else do
      v <- readArray from (cast src)
      writeMem dest v
      pure $ Left $ i + 1

single : Machine => CPU => (String -> IO ArrayBuffer) -> IO (Maybe ())
single loadFile = do
  getReg pc >>= \pc' => do
    -- consoleLog $ show pc'
    case pc' of
      0x640b => do -- Menu
        consoleLog "Menu"
        let cmd = 0x07
        writeMem 0x680d cmd
        setReg regA cmd
        rts
        pure Nothing
      0x40a7 => do -- Check disk
        consoleLog "Check disk"
        setReg pc 0x40bb
        pure Nothing
      0xcc03 => do -- Load from disk
        x <- getReg regX
        y <- getReg regY
        a <- getReg regA
        let fn = "data/disks/" <+> pack (map toChar [x, y]) <+> ".dat"
        consoleLog $ unwords ["Load from disk", fn]
        buf <- arrayDataFrom . cast {to = UInt8Array} =<< loadFile fn
        addr0 <- toAddr <$> readArray buf 0 <*> readArray buf 1
        copyToMemory buf 2 addr0
        rts
        pure Nothing
      0x4679 => do -- Show message from 0xcb4a, length 36
        consoleLog "Short message"
        rts
        pure $ Just ()
      _ => do
        step
        pure Nothing

%nomangle
public export
initialize : (String -> IO ArrayBuffer) -> IO Nat
initialize loadFile = do
  mem <- arrayDataFrom . cast {to = UInt8Array} =<< loadFile "data/program.dat"

  cpu <- new 0x438b
  let m = toMachine mem

  untilIO 0 $ \cnt => do
    Nothing <- single loadFile
      | Just _ => pure $ Right cnt
    pure $ Left $ cnt + 1
