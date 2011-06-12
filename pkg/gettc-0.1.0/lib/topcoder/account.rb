module TopCoder 
    class Account
        attr_accessor :username, :password
        def initialize username, password
            @username = username
            @password = password
        end
        def to_s
            return "#{@username}|#{@password}"
        end
    end
end
