library(magrittr)
library(dplyr)

readMap <- function(map.data) {
  if(length(map.data) == 1)
    map.data <- strsplit(map.data, "\n")[[1]]
  sapply(map.data, function(x) strsplit(x, "")[[1]] == "#") %>% t()
}

getJunctions <- function(src, dst) {
  if(isTRUE(all.equal(src, dst))){
    return(list(x = numeric(), y = numeric()))
  }
  x <- c(src[1], dst[1])# %>% sort()
  y <- c(src[2], dst[2])# %>% sort()
  # message("x = ", x, " y = ", y)
  step.x <- (x[2] - x[1]) / abs(y[2] - y[1])
  # message("step.x = ", step.x)
  if(is.finite(step.x)){
    # message("step.x = ", step.x)
    if(step.x != 0)
      all.x.steps <- seq(x[1], x[2], by = step.x)
    else
      all.x.steps <- rep(x[1], abs(y[2] - y[1]) + 1)
    match.x <- (all.x.steps %% 1) == 0
    # message("match.x = ", paste(match.x, collapse = ","))
    # message("steps.x = ", paste(all.x.steps, collapse = ","))
    # message("steps.y = ", paste(y[1]:y[2], collapse = ","))
    x.rv <- all.x.steps[match.x]
    y.rv <- (y[1]:y[2])[match.x]
  } else {
    step.y <- (y[2] - y[1]) / abs(x[2] - x[1])
    if(step.y != 0)
      all.y.steps <- seq(y[1], y[2], by = step.y)
    else
      all.y.steps <- rep(y[1], abs(x[2] - x[1]) + 1)
    match.y <- (all.y.steps %% 1) == 0
    # message("match.y = ", paste(match.y, collapse = ","))
    # message("steps.y = ", paste(all.y.steps, collapse = ","))
    y.rv <- all.y.steps[match.y]
    x.rv <- (x[1]:x[2])[match.y]
  }
  # message("x = ", paste(x.rv, collapse = ","), "; y = ", paste(y.rv, collapse = ","))
  # x.rv <- setdiff((x[1]:x[2])[match.y], x)
  # y.rv <- setdiff((y[1]:y[2])[match.x], y)
  # if(x[1] == x[2])
  #   x.rv <- c(x.rv, x[1])
  # if(y[1] == y[2])
  #   y.rv <- c(y.rv, y[1])
  rv <- matrix(c(x.rv, y.rv), ncol = 2)
  if(length(dim(rv))){
    if(all(rv[1, ] == src) || all(rv[1, ] == dst))
      rv <- rv[-1, ]
  }
  rv <- matrix(rv, ncol = 2)
  if(length(dim(rv))){
    last.r <- dim(rv)[1]
    if(all(rv[last.r, ] == src) || all(rv[last.r, ] == dst))
      rv <- rv[-last.r, ]
  }
  # message(list(rv))
  matrix(rv, ncol = 2)
}

getJunctions(c(3, 6), c(3, 3))
getJunctions(c(3, 7), c(5, 3))
getJunctions(c(9, 6), c(3, 3))

test.map.1 <- readMap(c(
  ".#..#"
  , "....."
  , "#####"
  , "....#"
  , "...##"
))

getElements <- function(map.arr, elements) {
  if(!length(dim(elements)))
    return(logical())
  apply(elements, 1, function(el){
    map.arr[el[2], el[1]]
  })
}

checkVisibility <- function(map.arr, src, dst) {
  blockers <- getJunctions(src, dst)
  rv <- !any(getElements(map.arr, blockers))
  # message("visibility = ", rv)
  rv
}

checkVisibility(test.map.1, c(3,2), c(3,5))

getIndices <- function(idx, dims) {
  i1 <- (idx - 1) %/% dims[2] + 1
  i2 <- idx %% dims[2]
  c(i1, ifelse(i2, i2, dims[2]))
}

lapply(4:8, getIndices, dims = c(4,6))

countVisible <- function(map.arr, src){
  is.visible <- sapply(1:dim(map.arr)[1], function(y){
    sapply(1:dim(map.arr)[2], function(x){
      if(!map.arr[y, x]){
        # message("No asteroid at ", y, ",", x)
        return(NA)
      }
      v <- checkVisibility(map.arr, src, c(x, y))
      # message("asteroid at ", y, ",", x, ", visibility = ", v, " from ", paste(src, collapse = ","))
      v
    })
  })
  # message("Is visible = ", paste(is.visible, collapse = ","))
  sum(is.visible, na.rm = T) - 1
}

countVisible(test.map.1, c(5, 4))

getMaxVisible <- function(map.arr) {
  dims <- dim(map.arr)
  max.dims <- numeric()
  max.cnt <- 0
  visible.cnt <- sapply(1:dim(map.arr)[1], function(y){
    sapply(1:dim(map.arr)[2], function(x){
      if(!map.arr[y, x])
        return (NA)
      cnt <- countVisible(map.arr, c(x, y))
      if(cnt > max.cnt) {
        max.cnt <<- cnt 
        max.dims <<- c(x, y) - 1
      }
      cnt
    })
  })
  print(visible.cnt %>% t())
  message("Max observerd = ", max.cnt)
  max.dims
}

getMaxVisible(test.map.1)

test.map.2 <- readMap(c(
  "......#.#."
  , "#..#.#...."
  , "..#######."
  , ".#.#.###.."
  , ".#..#....."
  , "..#....#.#"
  , "#..#....#."
  , ".##.#..###"
  , "##...#..#."
  , ".#....####"
))

getMaxVisible(test.map.2)

test.map.3 <- readMap("#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

getMaxVisible(test.map.3)

test.map.4 <- readMap(".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

getMaxVisible(test.map.4)

test.map.5 <- readMap(".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

getMaxVisible(test.map.5)

real.map <- readMap(".#......##.#..#.......#####...#..
...#.....##......###....#.##.....
..#...#....#....#............###.
.....#......#.##......#.#..###.#.
#.#..........##.#.#...#.##.#.#.#.
..#.##.#...#.......#..##.......##
..#....#.....#..##.#..####.#.....
#.............#..#.........#.#...
........#.##..#..#..#.#.....#.#..
.........#...#..##......###.....#
##.#.###..#..#.#.....#.........#.
.#.###.##..##......#####..#..##..
.........#.......#.#......#......
..#...#...#...#.#....###.#.......
#..#.#....#...#.......#..#.#.##..
#.....##...#.###..#..#......#..##
...........#...#......#..#....#..
#.#.#......#....#..#.....##....##
..###...#.#.##..#...#.....#...#.#
.......#..##.#..#.............##.
..###........##.#................
###.#..#...#......###.#........#.
.......#....#.#.#..#..#....#..#..
.#...#..#...#......#....#.#..#...
#.#.........#.....#....#.#.#.....
.#....#......##.##....#........#.
....#..#..#...#..##.#.#......#.#.
..###.##.#.....#....#.#......#...
#.##...#............#..#.....#..#
.#....##....##...#......#........
...#...##...#.......#....##.#....
.#....#.#...#.#...##....#..##.#.#
.#.#....##.......#.....##.##.#.##")

getMaxVisible(real.map)

norm_vec <- function(x) sqrt(sum(x^2))

sign0 <- function(x) {
  if(x == 0)
    return(1)
  sign(x)
}

getAngle <- function(src, dst) {
  c1 <- (src[1] - dst[1])
  c2 <- (src[2] - dst[2])
  # message("c1 = ", c1, "; c2 = ", c2)
  h <- sqrt(c1 ** 2 + c2 ** 2)
  # message("h = ", h)
  sine <- c1 / h
  # message(sine)
  # message(asin(sine))
  # message(asin(sine))
  # message("signs = ", sign0(c1), ", ", sign0(c2))
  angle <- case_when(
    sign0(c1) == 1 && sign0(c2) == 1 ~ 2 * pi - asin(sine)
    , sign0(c1) != 1 && sign0(c2) == 1 ~ - asin(sine)
    , sign0(c1) != 1 && sign0(c2) != 1 ~ pi / 2 - asin(sine)
    , sign0(c1) == 1 && sign0(c2) != 1 ~ pi + asin(sine)
    , TRUE ~ asin(sine)
  )
  ifelse(angle == 2 * pi, 0, angle)
}

getAngle(c(2,1), c(2,0))
getAngle(c(2,1), c(3,0))
getAngle(c(2,1), c(3,1))
getAngle(c(2,1), c(3,2))
getAngle(c(2,1), c(2,2))
getAngle(c(2,1), c(1,2))
getAngle(c(2,1), c(1,1))
getAngle(c(2,1), c(1,0))
getAngle(c(2,1), c(2,0))


getAngle(c(4,4), c(4,3))

setAngles <- function(map.arr, src) {
  dims <- dim(map.arr)
  angles <- sapply(1:dims[2], function(x){
    sapply(1:dims[1], function(y){
      getAngle(src, c(x, y))
    })
  }) %>% t() %>% as.numeric()
  distances <- sapply(1:dims[2], function(x){
    sapply(1:dims[1], function(y){
      sum(abs(src - c(x, y)))
    })
  }) %>% t() %>% as.numeric()
  df <- data.frame(
    distances = distances
    , angles = angles
    , index = 1:(dims[1] * dims[2])
  ) %>% arrange(angles, distances)
  df %>% pull(angles) %>% setNames(df$index)
}

setAngles(test.map.1, c(4,3))
setAngles(test.map.a, c(9,4))
setAngles(test.map.5, c(12, 14))


setVisibility <- function(map.arr, src) {
  dims <- dim(map.arr)
  sapply(1:dims[2], function(x){
    sapply(1:dims[1], function(y){
      if(all(src == c(x, y)))
        return(NaN)
      if(!map.arr[y, x])
        return(NA)
      checkVisibility(map.arr, src, c(x, y))
    })
  }) %>% t() %>% as.logical() %>% setNames(1:(dims[1] * dims[2]))
}

setVisibility(test.map.1, c(4,3))
setVisibility(test.map.a, c(9,4))

eliminationNumber <- function(map.arr, src) {
  a.map <- setAngles(map.arr, src) %>% sort()
  v.map <- setVisibility(map.arr, src)
  order <- numeric()
  iters <- 1
  while(sum(map.arr, na.rm = T) > 1) {
    message("asteroids remain (1) = ", sum(map.arr, na.rm = T))
    message("asteroids visible (1) = ", sum(v.map, na.rm = T))
    print(v.map)
    print(names(v.map))
    print(names(a.map))
    print(map.arr)
    sapply(names(a.map), function(pos) {
      # print(pos)
      if(!is.na(v.map[pos])) {
        if(v.map[pos]) {
          message("remove ", pos, " : ", paste(getIndices(as.numeric(pos), dim(map.arr)), collapse = ","))
          indices <- getIndices(as.numeric(pos), dim(map.arr))
          map.arr[indices[1], indices[2]] <<- FALSE
          order <<- c(order, as.numeric(pos))
        }
      }
    })
    v.map <- setVisibility(map.arr, src)
    message("asteroids remain (2) = ", sum(map.arr, na.rm = T))
    iters <- iters + 1
    if (iters > 30)
      break
  }
  order
}

eliminationNumber(test.map.a, c(9, 4))
eliminationNumber(test.map.2, c(4, 3))
els.real <- eliminationNumber(real.map, c(29, 24))
getIndices(els.real[200], dim(real.map)) - 1

els.5 <- eliminationNumber(test.map.5, c(12, 14))
lapply(c(1,2,3,10,20,50,100,199,200,201,299), function(idx) getIndices(els.5[idx], dim(test.map.5)) - 1)

test.map.a <- readMap(".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##")

angles.a <- setAngles(test.map.a, c(4, 9))

getIndices(5, c(5, 17))
