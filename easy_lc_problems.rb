# Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
# You may assume that each input would have exactly one solution, and you may not use the same element twice.
# You can return the answer in any order.
def two_sum(nums, target)
  a = 0
  z = nums.length - 1
  while true do
    val = nums[a] + numx[z]
    return [a, z] if val == target

    z-= 1 if val > target
    a+= 1 if val < target
  end
end
