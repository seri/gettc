Gettc's Ruby plugin to generate Ruby solutions for TopCoder problems.

## User requirements

You need `gettc` installed and your Ruby version must be at least 1.9.3 to run
the generated solution.

## User guide

If you want `gettc` to generate solutions with two-spaces indentation instead
of four-spaces, you can just edit the `*.rb` files in 
`~/.gettc/template/solve/ruby`.

## Known issues

If you're on Windows and you get an error like `cannot execute solver`, you can
fix by deleting or commenting the line `delegate` in `runner.sh` (line 132). To
make the change permanent, apply the fix in `~/.gettc/template/bin/runner.sh`.

If you have a nicer solution, please send me an email.

## Development status

* `rake test:ruby` Passed
* `rake test:engine` Passed
* `rake generate` OK
* `rake run` OK

## Change log

### gettc-1.9

* Born
