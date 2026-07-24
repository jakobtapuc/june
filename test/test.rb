require_relative "./tester.rb"

Dir.glob("tests/**/*.june").each do |file|
  name = file.delete_prefix("tests/").delete_suffix(".june")
  june_test name
end
