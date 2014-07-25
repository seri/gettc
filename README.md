This document is for new users. Existing users can find details about updating by reading the [change log](https://github.com/seri/gettc/blob/master/CHANGELOG.md).

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

    $ [sudo] gem install gettc

Once that is done, you should be able to run gettc on the command line. After a solution directory is generated, the standard procedure is:

    $ cd ProblemName/solve/your_language
    $ make

Now there are a couple things you need to get depending on your desired language.

## For C++

You are already ready to solve problems using C++.

## For Haskell

Besides GHC, [Cabal](http://www.haskell.org/cabal/download.html) is required. But it could have been bundled by your Haskell installer. Now:

    $ [sudo] cabal update
    $ [sudo] cabal install parsec

## For Java

Besides JDK, [Apache Ant](http://ant.apache.org/) is required. 

## For Python

Python 3 is required. If your system contains multiple pythons, make sure that Python 3 is the default. If you must keep Python 2 as default, manually tweak around the files in `~/.gettc/template/solve/python`.

## Other languages

At the moment, gettc supports C++, Haskell and Java out of the box. Other languages are provided via plugins.

# Tips

- Provide your own username/password in `~/.gettc/config.yml` if download fails.
- Use `make sysv` to display failed cases when challenging the system tests.
- You may `rm -rf build` after you're done solving to save some disk space.
- You can play with the contents of the directory `~/.gettc` to, say, remove things you don't want to be generated. If you mess up, you can safely delete the whole directory `~/.gettc`. The next time gettc runs, it will notice that there is no `~/.gettc` and regenerate that.
- You can bring the solutions generated by gettc to another computer to run. Such system doesn't need to have gettc, or even ruby, but it will need the standard gcc toolchain, and of course the compiler for your desired language (if it isn't C++). You will also need to copy '~/.gettc' to that system.