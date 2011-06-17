export TIME="Time: %es - Memory: %MK"
export TIMEFORMAT=%3lR

for fi in input/*
do 
    base=`basename $fi`
    echo -n "Test $base ... "
    if [ -x /usr/bin/time ]
    then
        /usr/bin/time $1/solve $fi $1/base
    else
        time $1/solve $fi $1/$base
    fi
    ./test $1/$base output/$base
    if [ $? -ne 0 ]
    then
        echo 'Failed'
        echo 'INPUT'
        echo -n '<'
        cat $fi
        echo '>'
        echo 'OUTPUT'
        echo -n '<'
        cat $1/$base
        echo '>'
        echo 'RESULT'
        echo -n '<'
        cat output/$base
        echo '>'
        echo
    else
        echo 'Passed'
    fi
done
