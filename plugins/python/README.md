Gettc's Python plugin to generate Python solutions for TopCoder problems.

## User requirements

Python 3 is required to run the generated solutions.

## Development status

* `rake test:python` Passed
* `rake test:engine` Passed
* `rake generate` OK
* `rake run` OK

## Known issues

If you're on Windows and you get an error like `cannot execute checker`, you can
fix by deleting or commenting the line `delegate` in `runner.sh` (line 132). To
make the change permanent, apply the fix in `~/.gettc/template/bin/runner.sh`.

If you have a nicer solution, please send me an email.

## Change log

### gettc-1.10

* Minor refactoring

### gettc-1.8.1

* Fix Character parsing (allowing a Character to be either quoted or unquoted)

### gettc-1.7.2

* Fix default target in template Makefile.

### gettc-1.7.1

* Remove mkmf in engine.rb

### gettc-1.7

* Auto detect path to python3 so things are smoother under systems with both
python2 and python 3.
* Minor engine.rb refactoring.

### gettc-1.6.2

* A more portable shebang in template.

### gettc-1.6

* Born
