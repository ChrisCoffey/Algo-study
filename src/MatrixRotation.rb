## #!/bin/ruby

require 'stringio'

mnr = gets.rstrip.split

$m = mnr[0].to_i

$n = mnr[1].to_i

r = mnr[2].to_i


matrix = Array.new($m)

$m.times do |i|
    matrix[i] = gets.rstrip.split.map(&:to_i)
end

# Complete the matrixRotation function below.
def matrixRotation(matrix, r)
  resultMatrix = Array.new($m) { |_| Array.new($n) }

  (0..$m-1).each do |i|
    (0..$n-1).each do |j|
      x,y = computeOffset i, j, r
      resultMatrix[y][x] = matrix[i][j]
    end
  end

  printMatrix resultMatrix
end

def computeOffset(x,y, r)
  lay = layer(x,y)

  #   0,0 is the upper left of layer 1
  #
  #    ul-----------ur
  #    |            |
  #    |            |
  #    |            |
  #    ll-----------lr
  #
  ul_x,ul_y = [lay,lay]
  ll_x,ll_y = [lay, $m-lay-1]
  lr_x,lr_y = [$n-lay-1, $m-lay-1]
  ur_x,ur_y = [$n-lay-1, lay]

  perimeter = 2*( ($m-lay) + ($n-lay) )
  rotationDistance = r % perimeter

  puts [x,y,r,rotationDistance, lay].inspect
  # determine where the point is, then move it counter-clockwise until either
  #   a) r == 0. This means the new location is on this row
  #   b) it reaches a boundary and needs to move in the next direction. In this case
  #      recursively call computeOffset with the new x,y, & remaining r

  # top row
  if y == ul_y
    distanceFromUpperLeft = x - ul_x
    distanceToMove = distanceFromUpperLeft > rotationDistance ? rotationDistance : (rotationDistance - distanceFromUpperLeft)
    if distanceToMove > distanceFromUpperLeft
      computeOffset ul_x, (y+1), (distanceToMove - 1)
    else
      [x - distanceToMove, y]
    end
  # bottom
  elsif y == ll_y
    distanceFromLowerRight = lr_x - x
    distanceToMove = distanceFromLowerRight > rotationDistance ? rotationDistance : (rotationDistance - distanceFromLowerRight)
    if distanceToMove > distanceFromLowerRight
      computeOffset lr_x, (y-1), (distanceToMove -1)
    else
      [x + distanceToMove, y]
    end
  # left side
  elsif x == ll_x && y != ul_y && y != ll_y
    distanceFromLowerLeft = ll_y - y
    distanceToMove = distanceFromLowerLeft > rotationDistance ? rotationDistance : (rotationDistance - distanceFromLowerLeft)
    if distanceToMove > distanceFromLowerLeft
      computeOffset (x+1), ll_y, (distanceToMove -1)
    else
      [x, y + distanceToMove]
    end
  # right side
  elsif x == ur_x && y != lr_y && y != ur_y
    distanceFromUpperRight = y - ur_y
    distanceToMove = distanceFromUpperRight > rotationDistance ? rotationDistance : (rotationDistance - distanceFromUpperRight)
    if distanceToMove > distanceFromUpperRight
      computeOffset (x-1), ur_y, (distanceToMove-1)
    else
      [x, y - distanceToMove]
    end
  else
    puts "Failure"
  end

  # how to handle corners?
  # corners count as part of the horizontal
  # Move it around the perimeter until distance = 0, using "jumps" rather than decrementing everything by 1

  # return the new point
end


def layer(x,y)
  rhs,lhs = [0, $n-1]
  top,bottom = [0, $m-1]
  xMidpoint = $n/2
  yMidpoint = $m/2

  xLayer = x > xMidpoint ? lhs - x : x
  yLayer = y > yMidpoint ? bottom - y : y

  [yLayer, xLayer].min
end

def printMatrix(matrix)
  puts "\n------------------\n"
  matrix.each do |row|
    puts row.inspect
    #puts row.join(" ")
  end
end


matrixRotation matrix, r

