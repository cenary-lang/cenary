#!/bin/bash

function compile {
  echo "[.] Compiling..."
  stack build
  echo "[.] Compilation complete."
}

EXECUTABLE=./.stack-work/install/x86_64-osx/lts-9.5/8.0.2/bin/ivy

function disasm {
  $EXECUTABLE -m disasm -i stdlib.ivy
}

function run {
  $EXECUTABLE -m run -i stdlib.ivy
}

function debug {
  $EXECUTABLE -m debug -i stdlib.ivy
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
