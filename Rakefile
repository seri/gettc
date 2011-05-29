ids = [10330, 10329, 10324, 10505, 10297, 9995, 10685, 10690, 10686]

task :download do 
    require 'yaml' 
    cfg = YAML::load open('config.yml')

    require 'topcoder/downloader.rb'
    robot = TopCoder::Downloader.new cfg['user'], cfg['pass']

    ok = true
    ids.each_index do |i|
        id = ids[i]

        html = robot.download id 
        open "files/#{id}.htm", 'w' do |f| 
            f.write html 
        end 

        res = html.match '<h3>Problem Statement</h3>'
        if res.nil? then
            puts "Failed at the #{i + 1}nd time, with id = #{id}"
            if html.include? '<b>Forgot your password?</b>' then
                puts 'Possible cause: wrong username or password, edit config.yml to fix'
            end 
            ok = false
            break
        end
    end 
    puts 'Everything cool' if ok
end

task :parse do
    require 'topcoder/parse.rb' 

    id = 10686
    filename = "files/#{id}.htm"

    if not File.exists? filename then
        puts "Download problem #{id} first"
    else
        html = open filename, 'r' do |f| f.read end 
        begin 
            prob = TopCoder::parse html 
            puts "PROBLEM #{id}\n\n"
            puts prob
        rescue Exception => err
            puts "Error occurred while parsing problem #{id}"
            puts err.backtrace.join "\n"
        end
    end 
end
