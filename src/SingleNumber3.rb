# @param {Integer[]} nums
# @return {Integer[]}
def single_number(nums)
  frequencies = {}
  nums.each do |i|
    if frequencies[i]
      frequencies.delete i
    else
      frequencies[i] = 1
    end
  end

  frequencies.keys
end

# Linear time & constant space
# Linear time rules out sorting then checking
#
# Nothing is known about the range of values, only that they are integers. They may be disjoint, etc...
# A big key is that the ordering is unimportant. Which
#
# Building a frequency table would solve in linear time. Just two passes over array.
#   The first pass to construct the table, the second to find the elements only set to 1
#
# But, that uses linear memory as well. In fact, anything that counts or stores all of the inputs
# will use linear memory.
#
# How to compress the input?
# Let's imagine that the input is compressed. How can I find the two numbers that appear once?
