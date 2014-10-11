This document is useful to both developers and users who may be curious to know 
what has changed after an update. By the way, you can update just by
`[sudo] gem install gettc` or `[sudo] gem update` (which will update all gems). 

Updating is usually backward-compatible, which means that the problems generated
by an older version should continue to work after an update.

As of version 1.6 and onward, gettc will replace the contents in `$GETTC_HOME`
(defaults to `~/.gettc`) if they are outdated. If you are a power user and you 
make heavy customisation in your `$GETTC_HOME`, make sure to back up before
updating.

## 1.7

### 1.7.4
Released October 11th, 2014
* Refactor the versioning a bit.

### 1.7.3
Released October 11th, 2014
* Fix the time command in `runner.sh` once again.

### 1.7.2
Released August 25th, 2014
* Bugfix for Go plugin on Windows.

### 1.7.1
Released August 16th, 2014
* Minor engine.rb change in Python plugin.

### 1.7.0
Released August 16th, 2014
* Add Golang support.
* Minor template changes.

## 1.6

### 1.6.2
Released July 26th, 2014
* Add a convenient command: `gettc reset` to force a replacement of `$GETTC_HOME`.
* Change shebang in Python plugin to be a bit more portable.
* Minor documentation changes.

### 1.6.1
Released July 25th, 2014
* Minor style changes.

### 1.6.0
Released July 25th, 2014
* Add Python plugin.
* Fixed string parsing when there is a quote character in between.
* Fixed ambiguity of the name of solution method by using namespace whenever 
that makes sense.
* Change runner.sh to be more portable w/r/t the time command (issue reported by Mukesh).
* Now gettc will respect `$GETTC_HOME` if it is set (default is still `~/.gettc`)
* Gettc will replace `$GETTC_HOME` if it is of an older version.
* Java plugin bug fixed: previously, the parser misbehaved with negative numbers.
* Heavy internal refactoring especially the structures of plugins.
* Remove unit test files in template.

## 1.5
Released February 20th, 2014
* Add support for proxy.

## 1.4
Released December 24th, 2013
* Important change to `download.rb` due to changes TopCoder authentication (issue 
reported by romand).

## 1.3

### 1.3.1
Released July 6th, 2013
* Normalize line endings.

### 1.3.0
Released July 3rd, 2013
* Switch from bluecloth to the more stable rdiscount.
* Push gettc to rubygems.org for easier installation.
* Documentation is published to project's website.

## 1.2

### 1.2.2
Released June 30th, 2013
* Remove unneeded file `account.rb`.

### 1.2.1
Released June 28th, 2013
* Finally get rid of the deprecated iconv.

### 1.2.0
Released April 3rd, 2013
* runner.sh now report test cases whose outputs cannot be parsed.
* Remove `rake install`.

## 1.1
Released December 28th, 2011
* Add boolean as a valid TopCoder type (issue reported by bokertov).
* Write a Raketask to automate re-installation, simply run `rake` now.

## 1.0

### 1.0.2
Released December 24th, 2011
* Fix some bugs in the parser (issue reported by bokertov).

### 1.0.1
Released October 24th, 2011
* Change Haskell template to work with GHC7.2.1 (issue reported by Muskesh).

### 1.0.0
Released August 12th, 2011
* Important: Change download.rb code due to TopCoder's changes.
* No longer need to export anything in bashrc.
* Add Java support.
* Put language supports into plugins, Ruby code into core.
* Refactor the types and signatures API.
