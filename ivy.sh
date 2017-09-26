#!/bin/bash

function compile {
  echo "[.] Compiling..."
  stack build
  echo "[.] Compilation complete."
}

EXECUTABLE=./.stack-work/install/x86_64-osx/lts-9.5/8.0.2/bin/ivy

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
