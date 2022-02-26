function lengthOfLongestSubstring(s: string): number {
  let maxLength = 0

  for(var i=0; i < s.length; i++){
    let seen = new Set()
    let len = 0
    for(var j=i; j < s.length; j++){
      if(seen.has(s[j])) { break }

      seen.add(s[j])
      len++
    }
    if(len > maxLength) {
      maxLength = len
    }
  }

  return maxLength
};
