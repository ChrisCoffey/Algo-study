#!/usr/bin/ruby

require 'json'
require 'stringio'

# Complete the minimumBribes function below.
def minimumBribes(q)
    count = 0
    q.each_index do |i|
        ix = i+1
        v = q[i]
        return puts "Too chaotic" if v > (ix+2)

        # V can move backwards infinitely as others pass it, but only ever forwards twice
        # So check the distance between where v is (ix) and how far it could have been.

        start = [v-1, 0].max
        (start..i).each do |n|
            count += 1 if q[n] > v
        end
    end
    count
end

t = gets.to_i

t.times do |t_itr|
    q = gets.rstrip.split(' ').map(&:to_i)

    minimumBribes q
end
