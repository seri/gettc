require "test/unit"
require "yaml"

require "gettc/account"
include Gettc

module TestHelper
  class Internal
    def self.setup_fixtures
      fixtures_data = YAML::load(File.read(File.join(script_dir, "fixtures.yml")))
      account_data = fixtures_data["account"]

      $problems = fixtures_data["problems"]
      $problems.each do |hash|
        message = "Test failed for #{hash['name']}"
        if description = hash["description"]
          message << " (#{description})"
        end
        hash["message"] = message
      end

      $account = Account.new(account_data["username"], account_data["password"])
    end

    def self.script_dir
      File.dirname(__FILE__)
    end

    def self.data_dir
      File.join(script_dir, "data")
    end

    def self.data_file_for(data_type, basename)
      File.join(data_dir, data_type.to_s, "#{basename}.html")
    end
  end
end

def write_problem(data_type, problem_name, file_content)
  File.write(TestHelper::Internal.data_file_for(data_type, problem_name), file_content)
end

def read_problem(data_type, problem_name)
  filename = TestHelper::Internal.data_file_for(data_type, problem_name)
  File.exists?(filename) ? File.read(filename) : ""
end

TestHelper::Internal.setup_fixtures
