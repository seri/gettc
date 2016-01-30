Gettc's Java plugin to generate Java solutions for TopCoder problems.

## User requirements

Besides JDK, [Apache Ant](http://ant.apache.org/) is required.

## Developement status

* `ant test` Passed
* `ant dist` OK
* `rake test:engine` Passed
* `rake generate` OK
* `rake run` OK

## Developement guide

The following guide is for internal developement only.

These days the Java community has apparently moved away from Ant. Maven or
Gradle is the way to go now. Eventually we will have to migrate but before
then, follow these instructions to run stuffs:

1. Download [junit-4.10.jar](http://search.maven.org/#search|gav|1|g:"junit" AND a:"junit).
Mind the version number. Last time I checked, things didn't work for junit 4.11.
2. Download [hamcrest-core-1.3.jar](http://search.maven.org/#search|gav|1|g:"org.hamcrest" AND a:"hamcrest-core").
3. Search for `ant-junit4-1.9.6.jar`
4. Put these jars into Ant lib, which should be `usr/share/ant/lib` on Linux.

## Change log

### gettc-1.8.1

* Fix Character parsing (allowing a Character to be either quoted or unquoted)

### gettc-1.7

* Remove target `test` in template Makefile.
* Minor engine.rb refactoring.

### gettc-1.6

* Fix string parsing with a quote in between.
* Fix negative numbers parsing.
