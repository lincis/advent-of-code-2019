library(dplyr)

getLayers <- function(x, w, h) {
  digits <- x %>% as.character() %>% strsplit("") %>% .[[1]] %>% as.numeric()
  n.pixels <- w * h
  layers = length(digits) / n.pixels
  array(digits, c(w, h, layers))
}

l <- getLayers(c(123456789012), 3, 2)

apply(l, 3, function(x) sum(x == 0)) %>% which.min()

d.8.in <- readLines("day-8.dat")

l.task <- getLayers(
  d.8.in
  , 25, 6
)

min.0.layer <- apply(l.task, 3, function(x) sum(x == 0)) %>% which.min()
sum(l.task[,,min.0.layer] == 1) * sum(l.task[,,min.0.layer] == 2)

getPixels <- function(stack.pixels) {
  stack.pixels[stack.pixels != 2] %>% head(1)
}

final.pixels <- apply(l.task, c(1,2), getPixels)
final.pixels %>% apply(2, function(x) paste(x, collapse = "")) %>% paste(collapse = "\n") %>% stringr::str_replace_all("0", " ") %>% message()

l.test <- getLayers("0222112222120000", 2, 2)
apply(l.test, c(1,2), getPixels)

l.test[,,1] %>% as.numeric()
