module Kastely

import Hardware.MOS6502.Emu

import JS.Buffer
import JS.Array
import Data.Maybe
import JS.Util

0 Memory : Type
Memory = Array Byte

unMaybe : Maybe a -> a
unMaybe (Just v) = v
unMaybe Nothing  = assert_total $ idris_crash "unMaybe"

toMachine : Memory -> Machine
toMachine mem =
  MkMachine (\addr => unMaybe <$> readIO mem (cast addr))
            (\addr,v => writeIO mem (cast addr) v)

single : Machine => CPU => Nat-> IO (Maybe Nat)
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

loop : Machine => CPU => IO Nat
loop = fromPrim $ go 0
  where go : Nat -> PrimIO Nat
        go cnt w =
          let MkIORes Nothing w2 := toPrim (single cnt) w
                | MkIORes (Just v) w2 => MkIORes v w2
           in go (cnt + 1) w2

%nomangle
public export
initialize : (String -> IO ArrayBuffer) -> IO Nat
initialize loadFile = do
  mem <- arrayDataFrom . cast {to = UInt8Array} =<< loadFile "data/program.dat"

  cpu <- new 0x438b
  let m = toMachine mem

  loop
