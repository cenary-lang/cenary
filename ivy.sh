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
disasm)
  disasm
  ;;
*)
  compile
  ;;
esac
