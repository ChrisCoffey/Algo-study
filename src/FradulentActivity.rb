def activityNotifications(expenditure, d)
    count = 0
    counts = initial_counts(expenditure, d)
    (d..(expenditure.length-1)).each do |i|
        m = median(counts, d)
        count += 1 if m*2 <= expenditure[i]
        # bookkeeping for the seen elements
        counts[expenditure[i]] += 1

        d_idx = expenditure[i-d]
        counts[d_idx] -= 1

    end

    count
end

def initial_counts(expenditure, d)
    counts = Array.new(201, 0)
    (1..d-1).each do |i|
        counts[expenditure[i]] += 1
    end
    counts
end

def median(counts, d)
    seen = middle = 0

    counts.each_with_index do |count, i|
        seen += count
        middle = i if seen >= d/2 unless middle > 0

        return d.odd? ? i : ((middle + i) / 2.0) if seen > d/2
    end
end
