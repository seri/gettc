require "net/https"
require "cgi"
require "uri"
require "ostruct"
require "json"

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

  class AuthTokenError < DownloadError
    attr_accessor :account, :server_response

    def initialize(account, server_response, message = "Failed to acquire an OAuth token")
      @account = account
      @server_response = server_response
      super "#{message}\nAccount: #{@account}\nServer Response:\n#{JSON.pretty_generate(server_response)}"
    end
  end

  class CookieError < DownloadError
    attr_accessor :cookie

    def initialize(cookie, message = "Cookie is in bad format")
      @cookie = cookie
      super "#{message}\nCookie: #{@cookie}"
    end
  end

  class IDNotAvailable < DownloadError
    attr_accessor :id

    def initialize(id, message = "ID not available")
      @id = id
      super "#{message} (#{id})"
    end
  end

  class ProxyError < DownloadError
    attr_accessor :proxy

    def initialize(proxy, message = "Proxy error")
      @proxy = proxy
      super "#{message}: http_proxy = #{proxy}"
    end
  end

  class Downloader
    ROOT = "https://community.topcoder.com"
    LIMIT = 10

    def initialize(account)
      @account = account
      @proxy = get_proxy
      @cookie = get_cookie
    end

    def download(url)
      uri = url
      unless uri.is_a?(URI)
        uri = url.start_with?("http") ? URI.parse(url) : URI.join(ROOT, url)
      end

      connect(uri) do |http|
        LIMIT.times do
          request = Net::HTTP::Get.new(uri.request_uri)
          request["cookie"] = @cookie

          response = http.request(request)
          return response.body if response.is_a?(Net::HTTPSuccess)

          unless response.is_a?(Net::HTTPMovedPermanently) then
            raise DownloadError.new(response.class.to_s)
          end

          uri = URI.parse(response["location"])
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
      uri = URI.parse(uri) unless uri.is_a?(URI)
      options = { use_ssl: uri.scheme == 'https' }

      if @proxy.nil?
        Net::HTTP.start(uri.host, uri.port, options) { |http| yield http }
      else
        Net::HTTP.start(uri.host, uri.port,
                        @proxy.host, @proxy.port,
                        @proxy.user, @proxy.pass,
                        options) do |http|
          begin
            yield http
          rescue Errno::ECONNRESET
            raise ProxyError.new(@proxy)
          end
        end
      end
    end

    def post_json(uri, params)
      uri = URI.parse(uri) unless uri.is_a?(URI)
      connect(uri) do |http|
        request = Net::HTTP::Post.new(uri.request_uri)
        request["Content-Type"] = "application/json"
        request.body = params.to_json
        http.request(request)
      end
    end

    def get_cookie
      jwt_token_response = JSON(post_json("http://api.topcoder.com/v2/auth", {
        username: @account.username,
        password: @account.password
      }).body)
      jwt_token = jwt_token_response["token"]
      raise AuthTokenError.new(@account, jwt_token_response, "Failed to acquire a JWT token") unless jwt_token

      refresh_token_response = JSON(post_json("http://api.topcoder.com/v2/reauth", {
        token: jwt_token
      }).body)
      refresh_token = refresh_token_response["token"]
      raise AuthTokenError.new(@account, refresh_token_response, "Failed to acquire a Refresh token") unless refresh_token

      response = post_json("https://api.topcoder.com/v3/authorizations", {
        param: {
          externalToken: refresh_token
        }
      })
      raw_cookie = response["set-cookie"]

      unless CGI::Cookie.parse(raw_cookie).has_key?("tcsso")
        raise DownloadError.new(raw_cookie, "Server refused to send a tcsso cookie")
      end
      raw_cookie
    end
  end
end
