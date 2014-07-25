Gettc's Java plugin to generate Java solutions for TopCoder problems.

# Current status:
  
    - `ant test` Passed
    - `rake test:engine` Passed
    - `ant dist` OK
    - `rake generate` OK
    - `rake run` OK

# Change log

## gettc-1.6

These days the Java community has apparently moved away from Ant. Maven or Gradle is the way to go now. Eventually we will have to migrate but before then, follow these instructions to run stuffs:

    - Download [junit-4.10.jar](http://search.maven.org/#search|gav|1|g:"junit" AND a:"junit). Mind the version number. Last time I checked, things didn't work for junit 4.11.
    - Download [hamcrest-core-1.3.jar](http://search.maven.org/#search|gav|1|g:"org.hamcrest" AND a:"hamcrest-core").
    - Put both the jars into Ant lib, which should be `usr/share/ant/lib` on Linux.