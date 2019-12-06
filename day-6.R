library(igraph)

readOrbits <- function(path) {
  all.lines <- readLines(path)
  parts <- strsplit(all.lines, ")") %>% unlist()
  graph(parts)
}

g <- readOrbits(here::here("day-6-data/test.dat"))

plot(g)

gsize(g)

neighbors(g, "COM")

ego_size(g, 100000L, mode = "in", mindist = 1) %>% sum()

g.test <- readOrbits(here::here("day-6-data/input.dat"))

plot(g.test)

gsize(g.test)

neighbors(g.test, "COM")

ego_size(g.test, 100000L, mode = "out", mindist = 1) %>% sum()


#### PART TWO

g2 <- readOrbits(here::here("day-6-data/test2.dat"))
plot(g2)
distances(g2, "YOU", "SAN")

distances(g.test, "YOU", "SAN")
