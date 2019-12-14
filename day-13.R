library(dplyr)
library(bit64)
# as.integer64 <- as.integer64


EXIT_OK <- 0
NEED_INPUT <- 1
NEED_ITER <- 2

readArgument <- function(arg, mode, base, program){
  if(mode == 1) {
    rv <- as.integer64(arg)
  }
  else {
    if(mode == 0)
      index <- as.integer(arg) + 1L
    else
      index <- as.integer(arg) + 1L + base
    if (index > length(program))
      rv <- as.integer64(0L)
    else
      rv <- program[index]
    # message("index = ", index, " value = ", rv)
  }
  # message("readArgument(", arg, ",", mode, ",", base, ") = ", rv)
  rv
}

doDraw <- function(image.data, intcode.output) {
  # message("draw from ", paste(intcode.output, collapse = ","))
  image.data[paste(intcode.output[1:2], collapse = ":")] <- intcode.output[3]
  image.data
}

calcInput <- function(image.data) {
  # message("calcInput in")
  pos.ball <- strsplit(names(image.data)[image.data == 4], ":")[[1]] %>% as.numeric()
  pos.paddle <- strsplit(names(image.data)[image.data == 3], ":")[[1]] %>% as.numeric()
  if(all(pos.paddle == pos.ball))
    message("Ball hits paddle")
  input <- case_when(
    pos.ball[1] == pos.paddle[1] ~ 0
    , pos.ball[1] < pos.paddle[1] ~ -1
    , pos.ball[1] > pos.paddle[1] ~ 1
  )
  # message("Ball at ", paste(pos.ball, collapse = ","), "; paddle at ", paste(pos.paddle, collapse = ","), "; move = ", input)
  input
}

computerDay13 <- function(program, offset = 0, output.result = NULL, base = 0, image.data = numeric(), score = NA) {
  operators <- list(
    "1" = list(fn = function(x) x[1] + x[2], args = 3, write.to.input = TRUE, do.jump = FALSE)
    , "2" = list(fn = function(x) x[1] * x[2], args = 3, write.to.input = TRUE, do.jump = FALSE)
    , "3" = list(fn = function(x){ x }, args = 1, input.args = TRUE, write.to.input = TRUE, do.jump = FALSE)
    , "4" = list(fn = function(x) {
      # message("Output = ", x)
      x
    }, do.output = TRUE, args = 1, write.to.input = FALSE, do.jump = FALSE)
    , "5" = list(fn = function(x){
      if(as.logical(x[1])) return(x[2])
      return(NA)
    }, args = 2, write.to.input = FALSE, do.jump = TRUE)
    , "6" = list(fn = function(x){
      if(!as.logical(x[1])) return(x[2])
      return(NA)
    }, args = 2, write.to.input = FALSE, do.jump = TRUE)
    , "7" = list(fn = function(x){ as.numeric(x[1] < x[2]) }, args = 3, write.to.input = TRUE, do.jump = FALSE)
    , "8" = list(fn = function(x){ as.numeric(x[1] == x[2]) }, args = 3, write.to.input = TRUE, do.jump = FALSE)
    , "9" = list(fn = function(x){ x }, args = 1, write.to.input = FALSE, do.jump = FALSE, change.base = TRUE)
  )
  
  # message("Offset = ", offset, "; base = ", base, " op.def = ", head(program, offset + 1))
  op.index.mode <- strsplit(as.character(program[offset + 1]), "")[[1]]
  # message("op.index.mode = ", paste(op.index.mode, collapse = ","))
  op.index <- paste(tail(op.index.mode, 2), collapse = "") %>% as.numeric() %>% as.character()
  if(op.index == "99") {
    message("Index is 99, quitting")
    return(list(program = program, status = EXIT_OK, output = output.result, offset = 0, image.data = image.data))
  }
  op.spec <- operators[[op.index]]
  op.modes <- numeric()
  if(length(op.index.mode) > 2)
    op.modes <- op.index.mode[1:(length(op.index.mode) - 2)] %>% rev %>% as.numeric()
  # message("args = ", op.spec$args, "; len(op.modes) = ", length(op.modes))
  op.modes <- c(op.modes, rep(0, op.spec$args - length(op.modes)))
  op.args <- as.numeric(program[(offset + 2) : (offset + 1 + op.spec$args)])
  # message(
  #   "op.index = ", op.index, "; op.spec = ", list(op.spec)
  #   , "; op.args = ", paste(op.args, collapse = ",")
  #   , "; op.modes = ", paste(op.modes, collapse = ",")
  # )
  all.args <- integer64()
  num.fn.args <- ifelse(op.spec$write.to.input, op.spec$args - 1, op.spec$args)
  # message("num args = ", num.fn.args)
  if(num.fn.args > 0) {
    for(i in 1:num.fn.args) {
      all.args <- c(all.args, readArgument(op.args[i], op.modes[i], base, program))
    }
  }
  if(!is.null(op.spec$input.args)){
    if(op.spec$input.args){
      # if(length(input) == 0) {
      #   message("Input args required, but not present, quitting")
      #   return(list(status = NEED_INPUT, output = output.result, program = program, input = input, offset = offset))
      # }
      # input <- image.data$data[[paste(image.data$pos, collapse = ":")]]
      # if(is.null(input)) {
      #   if(length(output.result) == 0){
      #     input <- 1
      #     message("Use first input ", input)
      #   }
      #   else
      #     input <- 0
      #   # message("Use input ", input)
      # }
      input <- calcInput(image.data)
      all.args <- c(all.args, as.integer64(input))
    }
  }
  # message(
  #   "all.args = ", paste(all.args, collapse = ","), " op.args = ", paste(op.args, collapse = ",")
  #   , " op.modes = ", paste(op.modes, collapse = ","), " offset = ", offset, " base = ", base
  #   , " op.index.mode = ", op.index.mode
  # )
  result <- op.spec$fn(all.args)
  # message("Result: ", list(op.spec$fn), "(", paste(all.args, collapse = ","), ") = ", as.numeric(result))
  if(op.spec$write.to.input){
    if(length(program) < tail(op.args, 1) + 1)
      program <- c(program, rep(as.integer64(0), tail(op.args, 1) + 1 - length(program)))
    index <- tail(op.args, 1)
    if(tail(op.modes, 1) == 2)
      index <- index + base
    # message("Write ", result, " to program[", index + 1, "]")
    program[index + 1] <- as.integer64(result)
    # message(program[index + 1])
    # message(paste(program, collapse = ","))
  }
  if(op.spec$do.jump && !is.na(result))
    new.offset <- as.integer(result)
  else
    new.offset <- offset + op.spec$args + 1
  if(!is.null(op.spec$do.output)) {
    # message("Output 1")
    if(op.spec$do.output){
      # message("Output 2")
      output.result <- c(output.result, as.integer(result))
    }
    if((length(output.result) %% 3) == 0) {
      # message("output (", length(output.result), ") = ", paste(tail(output.result, 3), collapse = ","))
      if(all(tail(output.result, 3)[1:2] == as.integer(c(-1, 0)))) {
        score <- tail(output.result, 1)
        message("Update score to ", score)
      } else {
        image.data <- doDraw(image.data, tail(output.result, 3))
      }
    }
  }
  if(!is.null(op.spec$change.base)){
    if(op.spec$change.base){
      base <- base + as.integer(result)
      # message("New base = ", base)
    }
  }
  # message("New offset = ", new.offset)
  # return(list(status = NEED_INPUT, output = output.result, program = program, input = input, offset = offset))
  # if(all(program < 1e16))
  # computerDay13(program, new.offset, output.result, base, image.data)
  return(list(status = NEED_ITER, output = output.result, program = program, offset = new.offset, base = base, image.data = image.data, score = score))
}

day.13.program[1] <- as.integer64(2L)
r.13 <- doCompute13(day.13.program)
sum(r.13$image.data == 2)
sum(r.13$image.data == 0)
sum(r.13$image.data == 1)
sum(r.13$image.data == 3)
sum(r.13$image.data == 4)

names(r.13$image.data[r.13$image.data == 4])

doCompute13 <- function(program) {
  r <- computerDay13(program)
  iters <- 1
  while(r$status == NEED_ITER) {
    r <- computerDay13(
      program = r$program, offset = r$offset, output.result = r$output, r$base, r$image.data, r$score
    )
    if((iters %% 1000) == 0){
      message("iteration ", iters, " n of blocks = ", sum(r$image.data == 2))
    }
    iters <- iters + 1
    if(iters > 1e8) {
      message("too many iters")
      break
    }
  }
  r
}

day.13.program <- strsplit(readLines("datasets/day-13.dat"),",")[[1]] %>% as.integer64()

