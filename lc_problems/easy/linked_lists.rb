# Definition for singly-linked list.
# class ListNode
#     attr_accessor :val, :next
#     def initialize(val = 0, _next = nil)
#         @val = val
#         @next = _next
#     end
# end
# @param {ListNode} head
# @return {Boolean}
def is_palindrome(head)
  slow = head
  fast = head
  while (!fast.nil?) do
    # reached the end already?
    break if fast.next.nil?

    # advance pointers
    slow = slow.next
    fast = fast.next.next
  end

  # slow is now the midpoint
  # reverse from slow to the end
  xs = slow
  acc = nil
  while(!xs.nil?) do
    elem = xs.next

    xs.next = acc
    acc = xs
    xs = elem
  end

  while(!acc.nil?) do
    return false if head.val != acc.val

    head = head.next
    acc = acc.next
  end

  return true
end

def reverse_list(head)
  # a left-fold with cons reverses a list. Apply the same logic here

  xs = head
  acc = nil
  while(!xs.nil?) do
    # I was unsure about whether these were weak references where the value would
    x = xs.next
    xs.next = acc
    acc = xs
    xs = x
  end

  acc
end
