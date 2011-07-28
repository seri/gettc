# Purpose

Download a TopCoder problem, parse the problem statement and system tests (if available) into input and output files, then finally generate a skeleton solution.

Although I am not actively participating in TopCoder, I love solving their problems to kickstart a working day. I love it so much that I have been automating the whole process so that I just have to sit down, drink my coffee, and write the solution without doing any unnecessary work. I want to write my code in my favourite editor, inspect my code against the examples given in the problem statement, and finally test it against the system tests, all locally. 

# Installation

You'll need Ruby and RubyGems pre-installed.

    $ cd /path/to/gettc
    $ gem install pkg/gettc-0.1.0.gem

`gettc` is expected to work out of the box on Windows and Linux.

# Usage

Suppose you are surfing TopCoder. As it seems like awesome day, you fancy solving a Div1 1k problem at: [http://www.topcoder.com/stat?c=problem_statement&pm=11290&rd=14537](http://www.topcoder.com/stat?c=problem_statement&pm=11290&rd=14537). From the URL, you see that the ID for this problem is `11290`. So you fire up your favourite shell (or just `cmd` on Windows) and do this:

    $ gettc 11290

The output is expected to be like this:

     You have given ID = 11290
     Downloading problem to raw HTML ... Done
     Parsing problem from raw HTML ... Done
     Generating problem diectory for PickAndDelete ... Done

And a `PickAndDelete` directory has been created with all the tests and solution skeletons populated for you.

    $ cd PickAndDelete/solve/cpp
    $ make

That will run your solution against the examples given in problem statement. The first time you do this, it's likely that the code won't compile. You will have to put this in your `.bashrc`:

    $ export CPLUS_INCLUDE_PATH=/path/to/gettc/include/cpp
    $ export HASKELL_INCLUDE_PATH=/path/to/gettc/include/haskell

If you want to challenge the code against system tests:

    $ make sys

The output will look something like:

    57 cases checked, 57 failed

Of course most of the cases will fail because you haven't written anything yet. Please write a solution in `solve.hh`. Don't cheat!

# Support for other languages

`gettc` provides sensible skeleton solution for C++ and Haskell by default, but it's quite easy to write a template for your favourite language. After the first run, `gettc` creates a `.gettc` directory in your `$HOME`. Within this directory, you can change your username/password in TopCoder by editing `config.yml`, and you can do anything with the `template` directory. The template files are written in `erb`.
