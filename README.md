Download a [TopCoder](http://topcoder.com/tc) problem, parse the examples and
system tests, then finally generate a naive solution for the following languages:

- C++
- Haskell
- Java
- Python 3
- Go
- Ruby
- Javascript

And support for more languages is just around the corner. 
    
> You can email me to request support for your favourite language. I will
> prioritize development on the most requested ones.

You write the function definition and the generated solution will take care of
running it against the downloaded input and output files.

TopCoder is a heaven for programmers. Solving algorithmic problems is a great
way to embrace the passion for programming. There are problems for all levels. 
A strong academic background is not required to enjoy it. If you like Project
Euler, you will probably love TopCoder. 

However, the TopCoder online arena is quite inconvenient and supports only a 
few languages. Gettc's goal is to make the practice of solving algorithmic
problems convenient and fun, and in your desired language.

# At a glance

    $ [sudo] gem install gettc
    $ gettc 11127

Note that 11127 is the ID that TopCoder gives to the problem named `DigitHoles`. You
can find the ID for any problem if you look at the URL for 
[that problem's statement](http://community.topcoder.com/stat?c=problem_statement&pm=11127) 
(you need to have a TopCoder account). Output:

    You have given ID = 11127
    Downloading problem to raw HTML ... Done
    Parsing problem from raw HTML ... Done
    Generating problem directory for DigitHoles ... Done

Now:

    $ cd DigitHoles/solve/cpp
    $ make demo

Output:

    [gettc] Compile solver                                                                   
    [gettc] Compile checker                                                                  
    [gettc] Run test cases                                                                   
    Case 0 ... 2ms Failed                                                                    
        Input: <42>                                                                          
        Expected: <1>                                                                        
        Received: <0>                                                                        
    Case 1 ... 1ms Failed                                                                    
        Input: <669>                                                                         
        Expected: <3>                                                                        
        Received: <0>                                                                        
    Case 2 ... 1ms Failed                                                                    
        Input: <688>
        Expected: <5>
        Received: <0>
    Case 3 ... 1ms Passed
    Case 4 ... 1ms Failed
        Input: <456>
        Expected: <2>
        Received: <0>
    Case 5 ... 1ms Failed
        Input: <789>
        Expected: <3>
        Received: <0>
    [gettc] Summary
    6 cases checked, 5 failures, 0 errors
    Failures: 0, 1, 2, 4, 5
        Total time taken: 7 ms
        Average time taken: 1 ms
        Slowest running case: 2 ms (case 0)

As you can see, the generated solution actually managed to solve 1 test case. 
Gettc is pretty smart after all. Anyway, you still need to do the hard work.
Open the file `DigitHoles.cpp` in your favourite editor and enter the following
content:

    class DigitHoles {
    public:
        int numHoles(int number) {
            static int holes[] = {1, 0, 0, 0, 1, 0, 1, 0, 2, 1};
            int ret = 0;
            while (number > 0) {
                ret += holes[number % 10];
                number /= 10;
            }
            return ret;
        }
    };

And then try again:

    $ make demo

You should see:

    [gettc] Compile solver
    [gettc] Compile checker
    [gettc] Run test cases
    Case 0 ... 2ms Passed
    Case 1 ... 1ms Passed
    Case 2 ... 1ms Passed
    Case 3 ... 1ms Passed
    Case 4 ... 1ms Passed
    Case 5 ... 2ms Passed
    [gettc] Summary
    6 cases checked, 0 failures, 0 errors
        Total time taken: 8 ms
        Average time taken: 1 ms
        Slowest running case: 2 ms (case 0)

Good. We have passed all the example tests. Why not challenge the system tests
while we are it?

    $ make sys

Output:

    [gettc] Compile solver
    [gettc] Compile checker
    [gettc] Run test cases
    [gettc] Summary
    131 cases checked, 0 failures, 0 errors
        Total time taken: 233 ms
        Average time taken: 1 ms
        Slowest running case: 7 ms (case 2)

Congratulations! You have solved a TopCoder problem like a boss!

# Installation

Gettc works on most operating systems, including Linux, Windows, and Mac OS.

The following packages are hard dependencies:

- [Ruby](http://www.ruby-lang.org/en/downloads/): The
[Ruby installer](http://rubyinstaller.org/) is recommend for Windows users. 
- [RubyGems](http://rubygems.org/pages/download): Many Ruby installations
already bundle RubyGems.
- The standard GCC toolset: Specifically, you should be able to run g++ and make
from the command line. Windows users may use [MinGW](http://www.mingw.org) or
Cygwin.

> If you have problems installing on Windows, this may help:
> https://www.quora.com/How-do-I-install-gettc-for-downloading-TopCoder-problems/answer/Rajneesh-Chauhan

With those in place, we are aready to go:

    $ [sudo] gem install gettc

Once that is done, you should be able to run gettc on the command line. The
standard procedure is:

    $ gettc problem_id
    $ cd ProblemName/solve/your_language
    $ make

Now there are a couple things you need to get depending on your desired language.

- For C++: You are already ready to solve problems [using C++](plugins/cpp/README.md).
- For Haskell: [Come here](plugins/haskell/README.md)
- For Java: [Come here](plugins/java/README.md)
- For Python: [Come here](plugins/python/README.md)
- For Go: [Come here](plugins/go/README.md)
- For Ruby: [Come here](plugins/ruby/README.md)
- For Javascript: [Come here](plugins/javascript/README.md)

# Known issues

- I remember having encountered a problem where gettc failed to download the system
tests while it is in fact possible to get the system tests. However, I forgot the 
problem ID. If you meet such problems, please file an issue so I can fix this.
- There are a few problems that allow multiple answers (such as
[CorruptedMessage](http://community.topcoder.com/stat?c=problem_statement&pm=13748&rd=16416)).
Gettc cannot deal with this because it can only extract the single answer that
TopCoder shows. If you meet such problems, a good guess is to always return the
smallest possible value.

# Tips

- It's good practice to provide your own username/password in `~/.gettc/config.yml`.
- You may `rm -rf build` after you're done solving to save some disk space.
- You can play with the contents of the directory `~/.gettc` to, say, remove things
you don't want to be generated. If you mess up, run `gettc reset`.
- You can bring the solutions generated by gettc to another computer to run.
Such system doesn't need to have gettc, or even ruby, but it will need the 
standard gcc toolchain, and of course the compiler for your desired language
(if it isn't C++). You will also need to copy `~/.gettc` to that system.

> This document is for new users. Existing users can find details about updating by
> reading the [change log](https://github.com/seri/gettc/blob/master/CHANGELOG.md).
