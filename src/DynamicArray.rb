#!/bin/ruby

require 'json'
require 'stringio'

#
# Complete the 'dynamicArray' function below.
#
# The function is expected to return an INTEGER_ARRAY.
# The function accepts following parameters:
#  1. INTEGER n
#  2. 2D_INTEGER_ARRAY queries
#

def dynamicArray(n, queries)
      # Write your code here
  last_answer = 0
  seq = Array.new(n) { [] }
  result = []

  queries.each do |q|
    tpe = q[0]; x = q[1]; y = q[2]

    i = (x ^ last_answer) % n
    if tpe == 1
      seq[i] << y
    else
      inner_seq = seq[i]
      last_answer = inner_seq[y % inner_seq.size]
      result << last_answer
    end
  end

  result
end

fptr = File.open(ENV['OUTPUT_PATH'], 'w')

first_multiple_input = gets.rstrip.split

n = first_multiple_input[0].to_i

q = first_multiple_input[1].to_i

queries = Array.new(q)

q.times do |i|
    queries[i] = gets.rstrip.split.map(&:to_i)
end

result = dynamicArray n, queries

fptr.write result.join "\n"
fptr.write "\n"

fptr.close()

