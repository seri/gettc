def benchmark message
    print "#{message} .. "
    before = Time.now
    yield
    elapsed = (Time.now - before).round(2)
    puts "#{elapsed}s"
end