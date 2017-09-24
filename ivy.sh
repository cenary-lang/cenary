#!/bin/bash

function compile {
  echo "[.] Compiling..."
  cabal build
  echo "[.] Compilation complete."
}

EXECUTABLE=./dist/build/ivy/ivy

function disasm {
  $EXECUTABLE -m disasm -i stdlib.el
}

function run {
  $EXECUTABLE -m run -i stdlib.el
}

function debug {
  $EXECUTABLE -m debug -i stdlib.el
}

function asm {
  $EXECUTABLE -m asm -i in.evm
}

case $1 in
clear)
  clear
  ;;
compile)
  compile
  ;;
debug)
  debug
  ;;
run)
  run
  ;;
asm)
  asm
  ;;
disasm)
  disasm
  ;;
*)
  compile
  ;;
esac
