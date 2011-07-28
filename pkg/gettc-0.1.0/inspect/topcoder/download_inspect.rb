require 'topcoder/download' 
include TopCoder

def filename dir_name, prob_name 
    return File.join File.dirname(__FILE__), 
                     "../../files/#{dir_name}/#{prob_name}.htm"
end

def download_all id, name, detail_url = nil, solution_url = nil
    print "Downloading #{name} ... "
    File.open filename('stats', name), 'w' do |f|
        f.write $robot.download_problem id
    end 
    if not detail_url.nil? then
        File.open filename('details', name), 'w' do |f|
            f.write $robot.download detail_url
        end
    end
    if not solution_url.nil? then
        File.open filename('solutions', name), 'w' do |f|
            f.write $robot.download solution_url
        end 
    end 
    puts "Done"
end

def main
    $robot = Downloader.new Account.new 'gettc', 'algorithm'
    download_all 10297, 'CirclesCountry', 
                 '/tc?module=ProblemDetail&rd=13751&pm=10297',
                 '/stat?c=problem_solution&cr=22504795&rd=13751&pm=10297'
    download_all 10329, 'PageNumbers',
                '/tc?module=ProblemDetail&rd=13757&pm=10329',
                '/stat?c=problem_solution&cr=16063200&rd=13757&pm=10329'
    download_all 10685, 'TheTournamentDivOne', 
                '/tc?module=ProblemDetail&rd=13907&pm=10685'
    download_all 11120, 'FunnyGames',
                '/tc?module=ProblemDetail&rd=14285&pm=11120'
    download_all 10737, 'BuildingRoads',
                 '/tc?module=ProblemDetail&rd=14153&pm=10737'
end
main
