module Main

import Hardware.MOS6502.Emu

import JS.Util

main : IO ()
main = do
  consoleLog "Hello World"
  -- setTimeout (consoleLog "And hello again!") 1000
