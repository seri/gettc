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

    def self.data_file_for(data_type, basename, extension)
      File.join(data_dir, data_type.to_s, "#{basename}.#{extension}")
    end
  end
end

def write_problem_html(data_type, problem_name, html)
  File.write(TestHelper::Internal.data_file_for(data_type, problem_name, "html"), html)
end

def read_problem_html(data_type, problem_name)
  filename = TestHelper::Internal.data_file_for(data_type, problem_name, "html")
  File.exists?(filename) ? File.read(filename) : ""
end

def write_problem_yaml(problem)
  File.write(TestHelper::Internal.data_file_for(:parsed, problem.name, "yaml"), problem.to_yaml)
end

def read_problem_yaml(problem_name)
  filename = TestHelper::Internal.data_file_for(:parsed, problem_name, "yaml")
  File.exists?(filename) ? YAML.load(File.read(filename)) : nil
end

TestHelper::Internal.setup_fixtures
