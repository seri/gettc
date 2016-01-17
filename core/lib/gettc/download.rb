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
    ROOT = "https://community.topcoder.com"
    LIMIT = 10

    def initialize(account)
      @account = account
      @proxy = get_proxy
      @sso_token = get_sso_token
    end

    def download(url)
      uri = url
      unless uri.is_a?(URI)
        uri = url.start_with?("http") ? URI.parse(url) : URI.join(ROOT, url)
      end

      connect(uri) do |http|
        LIMIT.times do
          request = Net::HTTP::Get.new(uri.request_uri)
          request["cookie"] = "tcsso=#{@sso_token}"

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

    def get_sso_token
      jwt_token = JSON(post_json("http://api.topcoder.com/v2/auth", {
        username: @account.username,
        password: @account.password
      }).body)["token"]

      refresh_token = JSON(post_json("http://api.topcoder.com/v2/reauth", {
        token: jwt_token
      }).body)["token"]

      response = post_json("https://api.topcoder.com/v3/authorizations", {
        param: {
          externalToken: refresh_token
        }
      })

      cookie = CGI::Cookie.parse(response["set-cookie"])
      raise LoginFailed.new(@account, cookie) unless cookie.has_key?("tcsso")

      cookie["tcsso"].to_s.split(";").first.split("=")[1].gsub("%7C", "|")
    end
  end
end
