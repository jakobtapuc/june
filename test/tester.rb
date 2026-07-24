require "open3"

class JuneTest
  def initialize(name)
    @name = name
  end

  def run
    source = "./tests/#{@name}.june"
    expected = "./tests/#{@name}.out"

    stdout, stderr, status =
      Open3.capture3("../build/june", source)

    unless status.success?
      fail_test("Program failed:\n#{stderr}")

      return
    end

    expected_output = File.read(expected)

    if stdout == expected_output
      puts "PASSED: #{@name}"
    else
      fail_test(<<~MSG)
        Output mismatch

        Expected:
        #{expected_output}

        Actual:
        #{stdout}
      MSG
    end
  end

  private

  def fail_test(message)
    puts "FAILED: #{@name}"
    puts message

    exit 1
  end
end

def june_test(name)
  JuneTest.new(name).run
end
