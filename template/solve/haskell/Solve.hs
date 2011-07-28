module Solve where 
<% require 'topcoder/langs/haskell'; haskell = TopCoder::Langs::Haskell.new func, vars %>
<%= func.name %> :: <%= haskell.var_types.join ' -> ' %> -> <%= haskell.class.type_to_s func.type %>
<%= func.name %> <%= haskell.var_names.join ' ' %> = <%= haskell.class.dumb_value func.type %>
