require "net/https"
require "cgi"
require "uri"
require "ostruct"

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

  class DownloadError < StandardError
  end

  class LoginFailed < DownloadError
    attr_accessor :account, :cookie

    def initialize(account, cookie, msg = "Wrong username or password")
      @account = account
      @cookie = cookie
      super "#{msg}\nAccount: #{@account}\nCookie: #{@cookie}\n"
    end
  end

  class IDNotAvailable < DownloadError
    attr_accessor :id

    def initialize(id, msg = "ID not available")
      @id = id
      super "#{msg} (#{id})"
    end
  end

  class ProxyError < DownloadError
    attr_accessor :proxy

    def initialize(proxy, msg = "Proxy error")
      @proxy = proxy
      super "#{msg}: http_proxy = #{proxy}"
    end
  end

  class Downloader
    ROOT = "http://community.topcoder.com"
    LIMIT = 10

    def initialize(account)
      @account = account
      @proxy = get_proxy
      @raw = get_cookie
    end

    def download(url)
      uri = url
      unless uri.is_a?(URI)
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

        raise DownloadError.new("Tried #{LIMIT} times without success")
      end
    end

    def download_problem(id)
      body = download("/stat?c=problem_statement&pm=#{id}")
      raise IDNotAvailable.new(id) unless body.match("<h3>Problem Statement</h3>")
      body
    end

    private

    def get_proxy
      uri = URI.parse(ENV["http_proxy"])
      proxy = OpenStruct.new
      proxy.host, proxy.port = uri.host, uri.port
      proxy.user, proxy.pass = uri.userinfo ? uri.userinfo.split(/:/) : nil, nil
      proxy
    rescue URI::InvalidURIError
      nil
    end

    def connect(uri)
      if @proxy.nil?
        Net::HTTP.start(uri.host, uri.port) { |http| yield http }
      else
        Net::HTTP.start(uri.host, uri.port,
                        @proxy.host, @proxy.port,
                        @proxy.user, @proxy.pass) do |http|
          begin
            yield http
          rescue Errno::ECONNRESET
            raise ProxyError.new @proxy
          end
        end
      end
    end

    def get_cookie
      uri = URI.join(ROOT, "tc?&module=Login")

      req = Net::HTTP::Post.new(uri.request_uri)
      req.set_form_data({
        "username" => @account.username,
         "password" => @account.password,
         "rem" => "on"
      })

      res = connect(uri) { |http| http.request(req) }
      raw = res["set-cookie"]

      cookie = CGI::Cookie.parse(raw)
      raise LoginFailed.new(@account, cookie) if cookie["tcsso"].empty?

      raw
    end
  end
end
