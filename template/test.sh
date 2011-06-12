for fi in input/*
do 
    base=`basename $fi`
    echo -n "Test $base ... "
    $1/<%= @name %> $fi $1/$base
    diff $1/$base output/$base &> /dev/null
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
