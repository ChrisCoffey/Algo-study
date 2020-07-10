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

  (0..$m-1).each do |row|
    (0..$n-1).each do |col|
      x,y = computeOffset col, row, r
      puts [matrix[row][col], row, col, y, x].inspect
      resultMatrix[y][x] = matrix[row][col]
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

  performRotation x, y, rotationDistance, {
    ul: {x: ul_x, y: ul_y},
    ll: {x: ll_x, y: ll_y},
    lr: {x: lr_x, y: lr_y},
    ur: {x: ur_x, y: ur_y}
    }
end

  # determine where the point is, then move it counter-clockwise until either
  #   a) r == 0. This means the new location is on this row
  #   b) it reaches a boundary and needs to move in the next direction. In this case
  #      recursively call computeOffset with the new x,y, & remaining r

  # how to handle corners?
  # corners count as part of the horizontal
  # Move it around the perimeter until distance = 0, using "jumps" rather than decrementing everything by 1
def performRotation(x, y, rotationDistance, coordinates)
  puts [x,y,rotationDistance].inspect
  # top row
  if y == coordinates[:ul][:y]
    distanceFromUpperLeft = x - coordinates[:ul][:x]
    distanceToMove = distanceFromUpperLeft >= rotationDistance ? rotationDistance : (rotationDistance - distanceFromUpperLeft)

    # Move to the next edge
    if distanceToMove > distanceFromUpperLeft # if its greater, rotate through
      performRotation coordinates[:ul][:x], (y+1), (distanceToMove -1), coordinates
    elsif distanceToMove < distanceFromUpperLeft
      return [x - distanceToMove, y]
    else # make it the corner
      performRotation coordinates[:ul][:x], y, distanceToMove, coordinates
    end
  # bottom
  elsif y == coordinates[:ll][:y]
    distanceFromLowerRight = coordinates[:lr][:x] - x
    distanceToMove = distanceFromLowerRight >= rotationDistance ? rotationDistance : (rotationDistance - distanceFromLowerRight)

    if distanceToMove > distanceFromLowerRight # if its greater, rotate through
      performRotation coordinates[:lr][:x], (y-1), (distanceToMove -1), coordinates
    elsif distanceToMove < distanceFromLowerRight # if its less, rotate less
      return [x + distanceToMove, y]
    else # otherwise, make it the corner
      performRotation coordinates[:lr][:x], y, distanceToMove, coordinates
    end
  # left side
  elsif x == coordinates[:ll][:x] && y != coordinates[:ul][:y] && y != coordinates[:ll][:y]
    distanceFromLowerLeft = coordinates[:ll][:y] - y
    distanceToMove = distanceFromLowerLeft >= rotationDistance ? rotationDistance : (rotationDistance - distanceFromLowerLeft)

    if distanceToMove > distanceFromLowerLeft
      performRotation (x+1), coordinates[:ll][:y], (distanceToMove -1), coordinates
    elsif distanceToMove == distanceFromLowerLeft
      performRotation x, coordinates[:ll][:y], distanceToMove, coordinates
    else
      return [x, y + distanceToMove]
    end
  # right side
  elsif x == coordinates[:ur][:x] && y != coordinates[:lr][:y] && y != coordinates[:ur][:y]
    distanceFromUpperRight = y - coordinates[:ur][:y]
    distanceToMove = distanceFromUpperRight >= rotationDistance ? rotationDistance : (rotationDistance - distanceFromUpperRight)

    if distanceToMove > distanceFromUpperRight
      performRotation (x-1), coordinates[:ur][:y], (distanceToMove -1), coordinates
    elsif distanceToMove < distanceFromUpperRight
      performRotation x, coordinates[:ur][:y], distanceToMove, coordinates
    else
      return [x, y - distanceToMove]
    end
  else
    puts "Failure"
  end
end


def layer(x,y)
  rhs,lhs = [0, $n-1]
  top,bottom = [0, $m-1]
  xMidpoint = ($n-1)/2
  yMidpoint = ($m-1)/2

  xLayer = x > xMidpoint ? lhs - x : x
  yLayer = y > yMidpoint ? bottom - y : y

  [yLayer, xLayer].min
end

def printMatrix(matrix)
  matrix.each do |row|
    puts row.join(" ")
  end
end


matrixRotation matrix, r

