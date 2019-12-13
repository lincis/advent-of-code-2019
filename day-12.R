library(magrittr)
library(dplyr)
library(pracma)
library(parallel)

getCoord <- function(txt, coord) {
  sub(paste0(".*", coord, "=(-*\\d+).*"), "\\1", txt) %>% as.numeric()
}

getCoord("<x=2, y=-10, z=-7>", "x")
getCoord("<x=2, y=-10, z=-7>", "y")
getCoord("<x=2, y=-10, z=-7>", "z")

readState <- function(txt) {
  lapply(strsplit(txt, "\n")[[1]], function(r){
    data.frame(
      x = getCoord(r, "x")
      , y = getCoord(r, "y")
      , z = getCoord(r, "z")
    )
  }) %>% bind_rows() %>%
    mutate(
      v_x = 0, v_y = 0, v_z = 0
      , pot.e = abs(x) + abs(y) + abs(z)
      , kin.e = abs(v_x) + abs(v_y) + abs(v_z)
      , total.e = pot.e * kin.e
    )
}

test.1 <- readState("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>")

tickTime <- function(sattelite.state) {
  for(idx in 1:nrow(sattelite.state)) {
    sub.state <- sattelite.state %>% slice(setdiff(1:nrow(sattelite.state), idx))
    for(coord in c("x", "y", "z")) {
      v.diff <- sum(
        case_when(
          sub.state[[coord]] > sattelite.state[idx, coord] ~ 1
          , sub.state[[coord]] < sattelite.state[idx, coord] ~ -1
          , TRUE ~ 0
        )
      )
      # message("idx = ", idx, " coord = ", coord, " velocity diff = ", v.diff)
      sattelite.state[idx, paste0("v_", coord)] <- sattelite.state[idx, paste0("v_", coord)] + v.diff
    }
  }
  sattelite.state %>%
    mutate(
      x = x + v_x
      , y = y + v_y
      , z = z + v_z
      , pot.e = abs(x) + abs(y) + abs(z)
      , kin.e = abs(v_x) + abs(v_y) + abs(v_z)
      , total.e = pot.e * kin.e
    )
}

tickTime(test.1)

doTicks <- function(sattellite.state, n){
  for(i in 1:n) {
    sattellite.state <- tickTime(sattellite.state)
  }
  sattellite.state
}

test.1.10 <- doTicks(test.1, 10)
sum(test.1.10$total.e)

task.state <- readState("<x=-13, y=14, z=-7>
<x=-18, y=9, z=0>
<x=0, y=-3, z=-3>
<x=-15, y=3, z=-13>")

task.state.1000 <- doTicks(task.state, 1000)
sum(task.state.1000$total.e)

tickCoord <- function(coord.vels, n = 1) {
  for (iterator in 1:n){
    sapply(1:(dim(coord.vels)[1]), function(row.num){
      coord.vels[row.num, 2] <<- coord.vels[row.num, 2] + sum(case_when(
        coord.vels[-row.num, 1] > coord.vels[row.num, 1] ~ 1
        , coord.vels[-row.num, 1] < coord.vels[row.num, 1] ~ -1
        , TRUE ~ 0
      ))
    })
    coord.vels[, 1] <- coord.vels[, 1] + coord.vels[, 2]
  }
  coord.vels
}

findPeriod <- function(cords) {
  cords.init <- cords
  period <- 1
  cords <- tickCoord(cords)
  while(!all(cords == cords.init)){
    cords <- tickCoord(cords)
    period <- period + 1
  }
  period
}

findPeriod(
  as.matrix(test.1 %>% select(x, v_x))
)

findRepeat <- function(sattellite.state) {
  period <- mclapply(c("x", "y", "z"), function(coord){
    m <- as.matrix(sattellite.state[, c(coord, paste0("v_", coord))])
    findPeriod(m)
  }, mc.cores = 3)
  message("Periods = ", paste(period, collapse = ":"))
  Lcm(Lcm(period[[1]], period[[2]]), period[[3]])
}

findRepeat(test.1)
findRepeat(task.state)

library(bit64)
Lcm(as.integer64(102356), Lcm(as.integer64(231614), as.integer64(193052)))
