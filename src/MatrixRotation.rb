## #!/bin/ruby

require 'stringio'

# Complete the matrixRotation function below.
def matrixRotation(matrix, r)
  resultMatrix = Array.new(m) { |_| Array.new size: n }

  (0..m).each do |i|
    (0..n).each do |j|
      y,x = computeOffset i, j, r
      resultMatrix[y][x] = matrix[i][j]
    end
  end

  printMatrix resultMatrix
end

def computeOffset(x,y, r)
  lay = layer(x,y)

  #
  #    ul-----------ur
  #    |            |
  #    |            |
  #    |            |
  #    ll-----------lr
  #
  ul_x,ul_y = [lay,lay]
  ll_x,ll_y = [lay, m-lay]
  lr_x,lr_y = [n-lay, m-lay]
  ur_x,ur_y = [n-lay, lay]

  perimeter = 2*( (m-lay) + (n-lay) )
  rotationDistance = r % perimeter

  # determine where the point is
  # Move it around the perimeter until distance = 0, using "jumps" rather than decrementing everything by 1

  # return the new point
end

def layer(x,y)
  xMidpoint = n/2
  yMidpoint = m/2

  yLayer = yMidpoint - (yMidpoint - y).abs
  xLayer = xMidpoint - (xMidpoint - x).abs

  [yLayer, xLayer].min
end

def printMatrix(matrix)
  matrix.each do |row|
    puts row.join(" ")
  end
end

mnr = gets.rstrip.split

m = mnr[0].to_i

n = mnr[1].to_i

r = mnr[2].to_i


matrix = Array.new(m)

m.times do |i|
    matrix[i] = gets.rstrip.split.map(&:to_i)
end

matrixRotation matrix, r

