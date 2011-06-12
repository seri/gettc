default: cc

cc: <%= @name %>.cc
	g++ <%= @name %>.cc -o out.cc/<%= @name %> -std=gnu++0x
	sh test.sh out.cc

hs: <%= @name %>.hs
	ghc <%= @name %>.hs -o out.hs/<%= @name %>
	sh test.sh out.hs
	rm <%= @name %>.{hi,o}

clean:
	rm -f out.cc/* out.hs/*
