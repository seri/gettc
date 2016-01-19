require "net/https"
require "cgi"
require "uri" 
require "ostruct"

module Gettc
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
    class ProxyError < DownloadError
        attr_accessor :proxy
        def initialize proxy, msg = 'Proxy error'
            @proxy = proxy
            super "#{msg}: http_proxy = #{proxy}"
        end
    end
    class Downloader
        ROOT = "https://community.topcoder.com/"
        LIMIT = 10
        def initialize account
            @account = account
            @proxy = get_proxy
            @raw = get_cookie
        end
        def download url
            uri = url
            unless uri.is_a? URI then
                uri = url.start_with?("http") ? URI.parse(url) : URI.join(ROOT, url) 
            end 

            connect uri do |http|
                LIMIT.times do
                    req = Net::HTTP::Get.new uri.request_uri
                    req["cookie"] = @raw
                    res = http.request req   
                    
                    return res.body if res.is_a? Net::HTTPSuccess
                    unless res.is_a? Net::HTTPMovedPermanently then
                        raise DownloadError.new res.class.to_s 
                    end
                    uri = URI.parse res["location"]
                end

                raise DownloadError.new "Tried #{LIMIT} times without success"
            end 
        end
        def download_problem id
            url = "/stat?c=problem_statement&pm=#{id}"
            body = download url
            if body.match("<h3>Problem Statement</h3>").nil? then
                raise IDNotAvailable.new id
            end 
            return body
        end
    private
        def get_proxy
            uri = URI.parse ENV["http_proxy"]
            proxy = OpenStruct.new
            proxy.host, proxy.port = uri.host, uri.port
            if uri.userinfo then
                proxy.user, proxy.pass = uri.userinfo.split /:/
            else
                proxy.user, proxy.pass = nil, nil
            end
            return proxy
        rescue URI::InvalidURIError
            return nil
        end
        def connect uri
            if @proxy.nil? then
                Net::HTTP.start(uri.host, uri.port, :use_ssl => uri.scheme == 'https') do |http|
                    yield http
                end
            else
                Net::HTTP.start(uri.host, uri.port, @proxy.host, @proxy.port,
                                @proxy.user, @proxy.pass, :use_ssl => uri.scheme == 'https') do |http|
                begin
                    yield http
                rescue Errno::ECONNRESET
                    raise ProxyError.new @proxy
                end
                end
            end 
        end
        def get_cookie
            uri = URI("https://community.topcoder.com/tc")

            req = Net::HTTP::Post.new uri.request_uri
            req.set_form_data({"module" => "Login",
                               "username" => @account.username,
                               "password" => @account.password})

            res = connect uri do |http|
                http.request req
            end
            raw = res["set-cookie"]

            cookie = CGI::Cookie.parse raw
            if cookie["tcsso"].empty? then
                raise LoginFailed.new @account, cookie, res.body
            end

            return raw
        end 
    end
end
