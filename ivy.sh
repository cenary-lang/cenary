#!/bin/bash

function compile {
  echo "[.] Compiling..."
  cabal build
  echo "[.] Compilation complete."
}

function run {
  EXECUTABLE=./dist/build/ivy/ivy
  if ! [[ -f $EXECUTABLE ]]; then
    echo "[.] Need to compile before running glow. Compiling..."
    compile
  fi
  echo "[.] Running glow..."
  $EXECUTABLE $1
}

function debug {
  EXECUTABLE=./dist/build/ivy/ivy
  if ! [[ -f $EXECUTABLE ]]; then
    echo "[.] Need to compile before running glow. Compiling..."
    compile
  fi
  echo "[.] Running debug..."
  $EXECUTABLE $1
}

case $1 in
clear)
  clear
  ;;
compile)
  compile
  ;;
debug)
  debug $2
  ;;
run)
  run $2
  ;;
*)
  compile
  ;;
esac
