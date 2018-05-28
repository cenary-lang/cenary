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
  # if ! [[ $2 ]]; then
  #   # start_testrpc
  # fi
  DEPLOYMENT_JS=deployment/deployment.current.js
  compile true
  deploy $1
  if ! [[ "$?" = 0 ]]; then
    echo "[!] Compilation did not succeed."
    exit 1
  else
    function_name="${2:-main()}"
    echo "Executing: $function_name"
    sed -i '' s/@call@/"$function_name"/ "$DEPLOYMENT_JS"
    node "$DEPLOYMENT_JS"
    rewind-deploy $1
  fi

  # if ! [[ $2 ]]; then
  #   # kill_testrpc
  # fi
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
  start_testrpc
  . ./test/assertions
  for i in $(eval echo "{0.."$(($LEN - 1))"..3}"); do
    filename="${ASSERTIONS[$i]}"
    assertion_text="${ASSERTIONS[$i + 1]}"
    function_call="${ASSERTIONS[$i + 2]}"
    blue "Testing file $filename"
    echo "Test Executing: $function_call"
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
  kill_testrpc
}

function start_testrpc {
  if ! [[ $(ps aux|grep testrpc|wc -l) -gt 1 ]]; then
    testrpc --gasLimit 90000000 --gasPrice 1 1>/dev/null &
  fi
}

function kill_testrpc {
  if [[ $(ps aux|grep testrpc|wc -l) -gt 1 ]]; then
    ps aux|grep testrpc|grep node|awk '{print $2}'|xargs kill -9
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
test)
  test
  ;;
*)
  compile
  ;;
esac
