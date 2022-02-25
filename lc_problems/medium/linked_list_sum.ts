 class ListNode {
     val: number
     next: ListNode | null
     constructor(val?: number, next?: ListNode | null) {
         this.val = (val===undefined ? 0 : val)
         this.next = (next===undefined ? null : next)
     }
 }


function addTwoNumbers(l1: ListNode | null, l2: ListNode | null): ListNode | null {
  let a = l1
  let b = l2
  if (a == null) {return b}
  if (b == null) {return a}

  // loop through both inputs until the shorter is exhausted
  let carry = 0
  const stack = []
  do {
    let [x, c] = handleCarry(a.val + b.val + carry)

    stack.push(x)
    a = a.next
    b = b.next
    carry = c
  } while(a != null && b != null)

  // process the remainder in one list
  if (a != null || b != null) {
    let remainder = a == null ? b : a;
    do {
      let [x, c] = handleCarry(remainder.val + carry)

      stack.push(x)
      remainder = remainder.next
      carry = c
    } while(remainder != null)
  }

  // handle a trailing carry
  if (carry == 1) { stack.push(carry) }

  let ls = null
  while(stack.length > 0) {
    const head = new ListNode(stack.pop(), ls)
    ls = head
  }

  return ls
};

function handleCarry(x: number): [number, number] {
  let carry = 0
  if(x > 9) {
    carry = 1
    x = x % 10
  } else {
    carry = 0
  }

  return [x, carry]
}
