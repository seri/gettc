module Gettc
  class Account
    attr_accessor :username, :password, :token

    def initialize(username, password, token = nil)
      @username = username
      @password = password
      @token = token
    end

    def to_s
      "#{@username}|#{@password}"
    end
  end
end
