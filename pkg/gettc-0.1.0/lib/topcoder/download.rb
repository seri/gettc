require 'net/https'
require 'cgi'
require 'uri' 

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
    class DownloadError < StandardError
    end
    class LoginFailed < DownloadError
        attr_accessor :account, :cookie
        def initialize account, cookie, msg = 'Wrong username or password'
            @account = account
            @cookie = cookie
            super "#{msg}\nAccount: #{@account}\nCookie: #{@cookie}\n"
        end
    end
    class IDNotAvailable < DownloadError
        attr_accessor :id
        def initialize id, msg = 'ID not available'
            @id = id
            super "#{msg} (#{id})"
        end
    end
    class Downloader
        def initialize account
            @account = account
            @raw = get_cookie
        end 
        def get_cookie
            uri = URI.parse 'https://www.topcoder.com/tc?&module=Login'

            req = Net::HTTP::Post.new uri.request_uri
            req.set_form_data ({'username' => @account.username,
                                'password' => @account.password,
                                'rem' => 'on' })

            https = Net::HTTP.new uri.host, uri.port
            https.use_ssl = true
            https.verify_mode = OpenSSL::SSL::VERIFY_NONE

            res = https.request req
            raw = res['set-cookie']

            cookie = CGI::Cookie.parse raw
            if cookie['main_user_id_1'].empty? or cookie['main_tcsso_1'].empty? then
                raise LoginFailed.new @account, cookie
            end

            return raw
        end 
        def download url
            if not url.start_with? 'http' then
                url = 'http://www.topcoder.com' + url
            end 
            uri = URI.parse url 
            req = Net::HTTP::Get.new uri.request_uri
            req['cookie'] = @raw

            http = Net::HTTP.new uri.host, uri.port
            res = http.request req

            if not res.is_a? Net::HTTPSuccess then
                raise DownloadError.new res.class.to_s
            end 
            return res.body
        end
        def download_problem id
            url = "http://www.topcoder.com/stat?c=problem_statement&pm=#{id}"
            body = download url
            if body.match('<h3>Problem Statement</h3>').nil? then
                raise IDNotAvailable.new id
            end 
            return body
        end
    end
end
