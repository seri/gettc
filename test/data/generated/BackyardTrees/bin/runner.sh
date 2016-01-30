# runner.sh mode /path/to/solver /path/to/input/dir /path/to/output/dir
# mode = quiet|verbose|file

mode="$1"
solver="$2"
data_d="$3"
output_d="$4"

command_exists () {
  type "$1" > /dev/null 2>&1 ;
}

timeit () {
  if [ $mode = "quiet" ]; then
    "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
  elif command_exists /usr/bin/time ; then
    /usr/bin/time "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
  elif command_exists time ; then
    time "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
  else
    "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
  fi
}

puts () {
  if [ $mode = "verbose" ]; then
    echo "$1"
  fi
}

write () {
  if [ $mode = "verbose" ]; then
    echo -n "$1"
  fi
}

view_file () {
  filename="$1"
  write "<"
  cat "$filename"
  puts ">"
}

report_case () {
  if [ $mode = "quiet" ]; then
    return
  fi
  write "    Input: "
  view_file "$data_d/$test_case.in"
  write "    Expected: "
  view_file "$data_d/$test_case.out"
  write "    Received: "
  view_file "$output_d/$test_case.out"
}

delegate () {
  if command_exists ruby ; then
    runner_rb="$(dirname $0)/runner.rb"
    ruby "$runner_rb" "$mode" "$solver" "$data_d" "$output_d"
    exit 0
  fi
}

init () {
  export TIME="Time: %es - Memory: %MK"
  export TIMEFORMAT=%3lR

  if [ ! -d "$output_d" ]; then
    mkdir "$output_d"
  fi

  dir=`dirname $0`
  echo "[gettc] Compile checker"
  checker="$dir/../build/check"
  if [ ! -x "$checker" ]; then
    undo=`pwd`
    cd "$dir/../util/check"
    make
    cd "$undo"
  fi
}

main () {
  echo "[gettc] Run test cases"

  cases=0
  fails=0
  errors=0
  failstr=""
  errorstr=""

  for input in "$data_d"/*.in; do
    if [ -f "$input" ]; then
      cases=$(( cases + 1 ))

      test_case=`basename "$input"`
      test_case=${test_case%.in}

      write "Case $test_case ... "
      timeit "$solver" "$data_d/$test_case.in" "$output_d/$test_case.out"
      "$checker" "$data_d/$test_case.out" "$output_d/$test_case.out"
      retcode=$?

      if [ $retcode -eq 0 ]; then
        puts "Passed"

      elif [ $retcode -eq 1 ]; then
        fails=$(( fails + 1 ))
        failstr="$failstr $test_case"
        puts "Failed"
        report_case "$test_case"

      else
        errors=$(( errors + 1 ))
        errorstr="$errorstr $test_case"
        puts "Error"
        report_case "$test_case"
      fi
    fi
  done

  echo "[gettc] Summary"
  echo "$cases cases checked, $fails failures, $errors errors"
  if [ $fails -gt 0 ]; then
    echo "Failures:${failstr}"
  fi
  if [ $errors -gt 0 ]; then
    echo "Errors:${errorstr}"
  fi
}

delegate
init
main
