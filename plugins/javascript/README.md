Gettc's Javascript plugin to generate Javascript solutions for TopCoder problems.

## User requirements

You need Node.js. I recommend a version of at least v.0.12.2.
You also need the following NPM packages:

* `npm install long -g` Because many TopCoder problems deal with 64bit integers.

## Known issues

If you're on Windows and you get an error like `cannot execute solver`, you can
fix by deleting or commenting the line `delegate` in `runner.sh` (line 132). To
make the change permanent, apply the fix in `~/.gettc/template/bin/runner.sh`.

If you have a nicer solution, please send me an email.

## Development status

* `rake test:js` Passed
* `rake test:engine` Passed
* `rake generate` OK
* `rake run` OK

## Development guide

* `npm install nodeunit -g`

## Change log

### gettc-1.10

* Born
