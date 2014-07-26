Gettc's Python plugin to generate Python solutions for TopCoder problems.

## User requirements

Python 3 is required to run the generated solutions. If your system contains 
multiple pythons, make sure that Python 3 is the default. That is, `python --version`
should returns 3.x.y. 

If you must keep Python 2 as default, change `/usr/bin/env python` to
`/usr/bin/env python3` in `~/.gettc/template/solve/python/{name}Runner.py`.

## Development status

* `rake test:python` Passed
* `rake test:engine` Passed
* `rake generate` OK
* `rake run` OK

## Change log

### gettc-1.6.2

* A more portable shebang in template.

### gettc-1.6

* Born
