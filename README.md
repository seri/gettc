Project page: [http://seri.github.io/gettc/](http://seri.github.io/gettc/)

# At a glance

    $ [sudo] gem install gettc
    $ gettc 11127

Note that 11127 is the ID that TopCoder gives to the problem named `DigitHoles`. You can find the ID for any problem if you look at the URL for [that problem's statement](http://community.topcoder.com/stat?c=problem_statement&pm=11127) (you need to have a TopCoder account). Output:

    You have given ID = 11127
    Downloading problem to raw HTML ... Done
    Parsing problem from raw HTML ... Done
    Generating problem diectory for DigitHoles ... Done

Now:

    $ cd DigitHoles/solve/cpp
    $ make demo

Output:

    Check 0 ... 0m0.328s
    Failed
        Input: <42>
        Expected: <1>
        Received: <0>
    Check 1 ... 0m0.015s
    Failed
        Input: <669>
        Expected: <3>
        Received: <0>
    Check 2 ... 0m0.016s
    Failed
        Input: <688>
        Expected: <5>
        Received: <0>
    Check 3 ... 0m0.015s
    Passed
    Check 4 ... 0m0.016s
    Failed
        Input: <456>
        Expected: <2>
        Received: <0>
    Check 5 ... 0m0.016s
    Failed
        Input: <789>
        Expected: <3>
        Received: <0>
    6 cases checked, 5 failed, 0 errored
    Failed cases: 0 1 2 4 5

5/6 test cases failed. That sucks. Now edit the file `DigitHoles.cpp` with the following content:

    int numHoles(int number) {
        static int holes[] = {1, 0, 0, 0, 1, 0, 1, 0, 2, 1};
        int ret = 0;
        while (number > 0) {
            ret += holes[number % 10];
            number /= 10;
        }
        return ret;
    }

And then try again:

    $ make demo

You should see:

    Check 0 ... 0m0.141s
    Passed
    Check 1 ... 0m0.015s
    Passed
    Check 2 ... 0m0.016s
    Passed
    Check 3 ... 0m0.015s
    Passed
    Check 4 ... 0m0.016s
    Passed
    Check 5 ... 0m0.015s
    Passed
    6 cases checked, 0 failed, 0 errored

Good. We have passed all the example tests. Why not challenge the system tests while we are it?

    $ make sys

Output:

    131 cases checked, 0 failed, 0 errored

Congratulations! You have solved a TopCoder problem like a boss!


# Introduction

Download a [TopCoder](http://topcoder.com/tc) problem, parse the examples and system tests, then finally generate a basic template for C++, Haskell, and Java. You write the function definition and the generated template will take care of running it against input and output files.

TopCoder is a heaven for programmers. Solving algorithmic problems is a great way to embrace the passion for programming. There are problems for all levels. A strong academic background is not required to enjoy it. If you like Project Euler, you will probably love TopCoder.

However, you normally have to paste the solution into TopCoder's online arena where it will be checked for correctness. Even then the online arena only supports C++, Java, and C#.

# Installation

The following packages are hard dependencies:

- [Ruby](http://www.ruby-lang.org/en/downloads/): The [Ruby installer](http://rubyinstaller.org/) is recommend for Windows users. 
- [RubyGems](http://rubygems.org/pages/download): Many Ruby installations already bundle RubyGems.
- The standard GCC toolset: Most Unix systems have it bundled. Windows users may use [MinGW](http://www.mingw.org).

With those in place, we are aready to go:

    $ [sudo] gem install hpricot rdiscount gettc

Once that is done, you should be able to run gettc on the command line. Now there are a couple things you need to get depending on which language you plan to use to solve problems.

## For C++

You are already ready to solve problems using C++. If you want to use `make test`, you just need to get [Boost](http://www.boost.org/).

## For Haskell

Besides GHC, [Cabal](http://www.haskell.org/cabal/download.html) is required. But it could have been bundled by your Haskell installer. Now:

    $ [sudo] cabal update
    $ [sudo] cabal install parsec

HUnit is required if you wish to apply TDD:

    $ [sudo] cabal install HUnit

If you don't write tests, there is no need to install HUnit.

## For Java

Besides JDK, [Apache Ant](http://ant.apache.org/) is required. This should come as no surprise to most Java programmers.

In Java, you use *ant* instead of *make*. So:

    $ ant demo

Will run against the examples. And:

    $ ant sys

Will run against the system tests.

[JUnit](https://github.com/KentBeck/junit/downloads) is required if you wish to run unit tests. Don't use the beta versions. Download one of the stable jar archives and put the jar into Ant's lib dir. 

# Further information

## FAQ

### I only use Java

    $ cd ~/.gettc/template/solve
    $ rm -rf haskell cpp

If you want them back, copy from the project's Gem directory.

### I want to see input/output for the failed cases in system tests

    $ make sysv

### I never write unit tests

    $ cd ~/.gettc/template/solve/
    $ rm {cpp,haskell,java}/*Test.*

### The output of make is too verbose
   
Use *make --quiet* instead. Here is what I do in my bashrc:

    alias mk=`make --quiet`

### But I use C&#35;

You may email me to request support for your favourite language. I don't promise it will get delivered, but I'll see what I can do. 

I would appreciate if you roll your own, too. First refer to gettc/plugins to see what I did for the supported languages. There are a few things you need to do:

- Write a generic parser that parses the generated input files into variables.
- Write an engine in Ruby that acts a helper to generate code.
- Write the template files in ERuby.

As what happens in gettc's generator is:

- It will automatically require ~/.gettc/include/*/engine.rb.
- It will walk through ~/.gettc/template, read each file as an ERuby template, and copy to the target directory.

Once you have done the hard work, please make a fork or something for other people to use.

### How do I uninstall?

    $ sudo gem uninstall gettc

Besides getting rid of gettc, you may also want to do this to get a clean upgrade:

    $ rm -rf ~/.gettc


#### Wrong username or password when trying to download

- You should provide your own username/password in ~/.gettc/config.yml
- If this doesn't work, try grabbing the latest version
- If that still doesn't work, make an issue or email me

## Tips

- You may `rm -rf build` after you're done solving to save some disk space.

## Known Issues

### Ambiguous function names

Sometimes the solution method has the same name with a standard library function, such as *filter*. In this case, you'll have to manually change the function name to something else in the runner and solution files.

### String parsing error

 TopCoder allows a string to be like "This is" one string", while gettc gets confused with the quote character in between. If your solution fails only under this situation, it's probably correct.

## Future plans

- Support for more interesting languages (Golang is on the way)
