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
  $EXECUTABLE -m ast -i "${1:-stdlib.ivy}"
}

function bytecode {
  $EXECUTABLE -m bytecode -i "${1:-stdlib.ivy}"
}

function run {
  $EXECUTABLE -m run -i "${1:-stdlib.ivy}"
}

function asm {
  $EXECUTABLE -m asm -i "${1:-in.evm}"
}

function deploy {
  $EXECUTABLE -m deploy -i "${1:-stdlib.ivy}"
}

function rewind-deploy {
  $EXECUTABLE -m rewind-deploy -i "${1:-stdlib.ivy}"
}

function compute {
  DEPLOYMENT_JS=deployment/deployment.current.js
  compile true
  deploy $1
  if ! [[ "$?" = 0 ]]; then
    echo "[!] Compilation did not succeed."
    exit 1
  else
    if [[ $2 ]]; then
      sed -i '' s/@call@/"$2"/ "$DEPLOYMENT_JS"
    fi
    node "$DEPLOYMENT_JS"
    rewind-deploy $1
  fi
}

function blue {
  echo -e "\e[34m$1\033[0m"
}

function green {
  echo -e "\e[32m$1\033[0m"
}

function red {
  echo -e "\e[31m$1\033[0m"
}

# TODO: Tidy up here when you actually learn bash
function test {
  if ! [[ $(ps aux|grep testrpc|wc -l) -gt 1 ]]; then
    start_testrpc
  fi
  ASSERTIONS=( DynArr.ivy "3" "main()"
               Id.ivy "3" "id(3)"
               Fibonacci.ivy "610" "fib(15)"
               Adder.ivy "10" "add(3, 7)"
               Adder.ivy "3" "add(1, 2)"
               Branching.ivy "3" "main(4)"
               Branching.ivy "6" "main(5)"
             )
  for i in {0..20..3}; do
    filename="${ASSERTIONS[$i]}"
    assertion_text="${ASSERTIONS[$i + 1]}"
    function_call="${ASSERTIONS[$i + 2]}"
    blue "Testing file $filename"
    output=$(compute ./test/sources/"$filename" "$function_call" 2>&1)
    if echo $output | grep -q "$assertion_text"; then
      green "OK"
      blue "Output: "
      green "$output"
    else
      red "NOT OK"
      blue "Output: "
      red "$output"
    fi
  done

  if [[ $(ps aux|grep testrpc|wc -l) -gt 1 ]]; then
    kill_testrpc
  fi
}

function start_testrpc {
  testrpc 1>/dev/null &
}

function kill_testrpc {
  ps aux|grep testrpc|grep node|awk '{print $2}'|xargs kill -9
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
test)
  test
  ;;
*)
  compile
  ;;
esac
