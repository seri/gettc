require "test/unit"
require "yaml"

require "gettc/account"
include Gettc

def setup_fixtures
  fixtures_data = YAML::load(File.read("fixtures.yml"))
  account_data = fixtures_data["account"]

  $problems = fixtures_data["problems"]
  $account = Account.new(account_data["username"], account_data["password"])
end

def write_problem_statement(problem_name, problem_statement_html)
  write_file
end

setup_fixtures
