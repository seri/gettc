# Gettc CHANGELOG

## 1.6
Unreleased
* Add support for Python.
* Change runner.sh to be more portable w/r/t the time command (issue reported by Mukesh).
* Main script `gettc` now knows if your `.gettc` is too old and offers replacing it.

## 1.5
Released February 20th, 2014
* Add support for proxy.

## 1.4
Released December 24th, 2013
* Important change to download.rb due to changes TopCoder authentication (issue reported by romand).

## 1.3.1
Released July 6th, 2013
* Normalize line endings.

## 1.3
Released July 3rd, 2013
* Switch from `bluecloth` to the more stable `rdiscount`.
* Push `gettc` to `rubygems.org` for easier installation.
* Documentation is published to project's website.

## 1.2.2
Released June 30th, 2013
* Remove unneeded file `account.rb`.

## 1.2.1
Released June 28th, 2013
* Finally get rid of the deprecated `iconv`.

## 1.2
Released April 3rd, 2013
* `runner.sh` now report test cases whose outputs cannot be parsed.
* Remove `rake install`.

## 1.1
Released December 28th, 2011
* Add boolean as a valid TopCoder type (issue reported by bokertov).
* Write a Raketask to automate re-installation, simply run `rake` now.

## 1.0.2
Released December 24th, 2011
* Fix some bugs in the parser (issue reported by bokertov).

## 1.0.1
Released October 24th, 2011
* Change Haskell template to work with GHC7.2.1 (issue reported by Muskesh).

## 1.0.0
Released August 12th, 2011
* Important: Change download.rb code due to TopCoder's changes.
* No longer need to export anything in `bashrc`.
* Add Java support.
* Put language supports into plugins, Ruby code into core.
* Refactor the types and signatures API.