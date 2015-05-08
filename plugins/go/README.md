Gettc's Go plugin to generate Go solutions for TopCoder problems.

## User requirements

Go version >=1.1 

## Developement status

* `rake test:go` Passed
* `rake generate` OK
* `rake run` OK

## Change log

### gettc-1.8.1

* Fix Character parsing (allowing a Character to be either quoted or unquoted)

### gettc-1.7.2

* Important bugfix in Makefile: on Windows, GOPATH tends to be quite picky. 

### gettc-1.7

* Born
