library(dplyr)

operators <- list(
  plus = `+`, times = `*`
)

computer <- function(inputs, offset = 0) {
  if (length(inputs) - offset < 4) stop("Too few inputs")
  instructions <- inputs[(offset + 1) : (offset + 4)]
  if (instructions[1] == 99) {
    # message("Instruction is 99, quitting")
    return(inputs)
  }
  # message("Instructions: ", paste(instructions, collapse = ", "))
  operator <- operators[[instructions[1]]]
  operator.name <- names(operators)[[instructions[1]]]
  arg1 <- inputs[instructions[2] + 1]
  arg2 <- inputs[instructions[3] + 1]
  # message("Do : ", arg1, " ", operator.name, " ", arg2)
  result <- operator(arg1, arg2)
  # message("Result: ", result)
  inputs[instructions[4] + 1] <- result
  if(length(inputs) - offset - 4 >= 4)
    return(computer(inputs, offset + 4))
  inputs
}

computer(c(1,0,0,0,99))
computer(c(2,3,0,3,99))
computer(c(2,4,4,5,99,0))
computer(c(1,1,1,4,99,5,6,0,99))

all.inputs <- c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,6,27,1,6,27,31,1,13,31,35,1,13,35,39,1,39,13,43,2,43,9,47,2,6,47,51,1,51,9,55,1,55,9,59,1,59,6,63,1,9,63,67,2,67,10,71,2,71,13,75,1,10,75,79,2,10,79,83,1,83,6,87,2,87,10,91,1,91,6,95,1,95,13,99,1,99,13,103,2,103,9,107,2,107,10,111,1,5,111,115,2,115,9,119,1,5,119,123,1,123,9,127,1,127,2,131,1,5,131,0,99,2,0,14,0)

result <- computer(all.inputs)

paste(result, collapse = ",")

lookup <- lapply(1:99, function(noun){
  lapply(1:99, function(verb){
    current.inputs <- all.inputs
    current.inputs[2] <- noun
    current.inputs[3] <- verb
    computer(current.inputs)[1]
  })
})

lookup.v <- unlist(lookup)

lookup.df <- data.frame(
  result = lookup.v
  , noun = sapply(1:99, function(noun) sapply(1:99, function(verb) noun)) %>% as.numeric()
  , verb = 1:99
)

lookup.df %>% filter(result == 19690720)
