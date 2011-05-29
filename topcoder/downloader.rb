require 'net/https' 
require 'uri' 

module TopCoder 
    class Downloader
        def initialize username, password
            @username = username
            @password = password
        end 
        def get_cookie
            uri = URI.parse 'https://www.topcoder.com/tc?&module=Login'

            req = Net::HTTP::Post.new uri.request_uri
            req.set_form_data ({'username' => @username,
                                'password' => @password,
                                'rem' => 'on' })

            https = Net::HTTP.new uri.host, uri.port
            https.use_ssl = true
            https.verify_mode = OpenSSL::SSL::VERIFY_NONE

            res = https.request req
            return res['set-cookie']
        end 
        def download id
            url = 'http://www.topcoder.com/stat?c=problem_statement&pm=' + id.to_s
            uri = URI.parse url

            @cookie = get_cookie if @cookie.nil?
            req = Net::HTTP::Get.new uri.request_uri
            req['cookie'] = @cookie

            http = Net::HTTP.new uri.host, uri.port
            res = http.request req
            return res.body
        end
    end
end
