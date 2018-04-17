#!/bin/bash

function compile {
  if ! [[ "$1" = true ]]; then echo "[.] Compiling..."; fi
  stack build
  if ! [[ "$1" = true ]]; then echo "[.] Compilation complete."; fi
}

EXECUTABLE=./.stack-work/install/x86_64-osx/lts-11.2/8.2.2/bin/ivy

function disasm {
  $EXECUTABLE -m disasm -i stdlib.ivy
}

function ast {
  $EXECUTABLE -m ast -i stdlib.ivy
}

function bytecode {
  $EXECUTABLE -m bytecode -i stdlib.ivy
}

function run {
  $EXECUTABLE -m run -i stdlib.ivy
}

function asm {
  $EXECUTABLE -m asm -i in.evm
}

function deploy {
  $EXECUTABLE -m deploy -i stdlib.ivy
}

function rewind-deploy {
  $EXECUTABLE -m rewind-deploy -i stdlib.ivy
}

function compute {
  compile true
  deploy
  if ! [[ "$?" = 0 ]]; then
    echo "[!] Compilation did not succeed."
    exit 1
  else
    node deployment/deployment.current.js
    rewind-deploy
  fi
}

case $1 in
clear)
  clear
  ;;
compile)
  compile
  ;;
ast)
  ast
  ;;
bytecode)
  bytecode
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
deploy)
  deploy
  ;;
rewind-deploy)
  rewind-deploy
  ;;
compute)
  compute
  ;;
*)
  compile
  ;;
esac
