{ pkgs, deployment-js }:
let
  tests = [
    { name = "DynArr.ivy";
      output = "3";
      funcall = "main()";
    }
    { name = "Id.ivy";
      output = "3";
      funcall = "main()";
    }
    { name = "Fibonacci.ivy";
      output = "610";
      funcall = "fib(15)";
    }
    { name = "Adder.ivy";
      output = "10";
      funcall = "add(3, 7)";
    }
    { name = "Adder.ivy";
      output = "3";
      funcall = "add(1, 2)";
    }
    { name = "Branching.ivy";
      output = "3";
      funcall = "main(4)";
    }
    { name = "Branching.ivy";
      output = "6";
      funcall = "main(5)";
    }
  ];
  colors = {
    black = "0;30";
    blue = "34";
    yellow = "0;33";
    dark_gray = "1;30";
    red = "0;31";
    light_red = "1;31";
    green = "0;32";
    light_green = "1;32";
    brown_orange = "0;33";
    light_blue = "1;34";
    purple = "0;35";
    light_purple = "1;35";
    cyan = "0;36";
    light_cyan = "1;36";
    light_gray = "0;37";
    white = "1;37";
  };
  colored = code: msg: ''
    echo -en "\e[${code}m${msg}\033[0m"
  '';
  blue = colored colors.blue;
  yellow = colored colors.yellow;
  black = colored colors.black;
  toTestScript = { name, output, funcall }: pkgs.writeScript "wow" ''
    echo "[.] Testing file: ${name}"
  '';
  runTests = ivy: pkgs.writeScript "runTests" ''
    for test in ${builtins.concatStringsSep " " (map toTestScript tests)}; do
      $test
    done
  '';
in
  runTests
