module Gettc
  class Account
    attr_accessor :username, :password

    def initialize(username, password)
      @username = username
      @password = password
    end

    def to_s
      "#{@username}|#{@password}"
    end
  end
end
