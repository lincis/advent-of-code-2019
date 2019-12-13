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

changeCoords <- function(command, coords) {
  sine <- ifelse(command == 1, -1, 1)
  # message("coords from ", command, " on ", paste(coords, collapse = ","), " = ", paste(matrix(c(0, -sine, sine, 0), ncol = 2, byrow = T) %*% coords, collapse = ","))
  matrix(c(0, -sine, sine, 0), ncol = 2, byrow = T) %*% coords
}

changeCoords(0, c(0,1))

# doDraw <- function(command, direction = c(1, 0), coords = c(0, 0)) {
#   new.direction <- changeCoords(command[2], direction)
#   list(
#     color = ifelse(command[1] == 1, "#", ".")
#     , coords = coords
#     , new.coords = new.direction + coords
#     , new.direction = new.direction
#   )
# }

doDraw <- function(image.data, intcode.output) {
  image.data$data[[paste(image.data$pos, collapse = ":")]] <- intcode.output[1]
  image.data$direction <- changeCoords(intcode.output[2], image.data$direction)
  # message("Paint ", paste(image.data$pos, collapse = ","), " ", intcode.output[1])
  image.data$pos <- image.data$pos + image.data$direction
  # message("Move to ", paste(image.data$pos, collapse = ","))
  image.data
}

computerDay11 <- function(program, offset = 0, output.result = NULL, base = 0, image.data = list(pos = c(1,1), direction = c(0, 1), data = list())) {
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
      input <- image.data$data[[paste(image.data$pos, collapse = ":")]]
      if(is.null(input)) {
        if(length(output.result) == 0){
          input <- 1
          message("Use first input ", input)
        }
        else
          input <- 0
        # message("Use input ", input)
      }
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
    # message("output (", length(output.result), ") = ", paste(output.result, collapse = ","))
    if((length(output.result) %% 2) == 0) {
      image.data <- doDraw(image.data, tail(output.result, 2))
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
  # computerDay11(program, new.offset, output.result, base, image.data)
  return(list(status = NEED_ITER, output = output.result, program = program, offset = new.offset, base = base, image.data = image.data))
}

doCompute <- function(program) {
  r <- computerDay11(program)
  iters <- 1
  while(r$status == NEED_ITER) {
    r <- computerDay11(
      program = r$program, offset = r$offset, output.result = r$output, r$base, r$image.data
    )
    if((iters %% 1000) == 0)
      message("iteration ", iters)
    iters <- iters + 1
    if(iters > 1e8) {
      message("too many iters")
      break
    }
  }
  r
}

day.11.program <- as.integer64(c(3,8,1005,8,330,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,29,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,51,1,1103,2,10,1006,0,94,1006,0,11,1,1106,13,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,87,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,109,2,1105,5,10,2,103,16,10,1,1103,12,10,2,105,2,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,146,1006,0,49,2,1,12,10,2,1006,6,10,1,1101,4,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,183,1,6,9,10,1006,0,32,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,213,2,1101,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,239,1006,0,47,1006,0,4,2,6,0,10,1006,0,58,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,274,2,1005,14,10,1006,0,17,1,104,20,10,1006,0,28,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,309,101,1,9,9,1007,9,928,10,1005,10,15,99,109,652,104,0,104,1,21101,0,937263411860,1,21102,347,1,0,1105,1,451,21101,932440724376,0,1,21102,1,358,0,1105,1,451,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,29015167015,1,21101,0,405,0,1106,0,451,21102,1,3422723163,1,21101,0,416,0,1106,0,451,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,868389376360,1,21101,0,439,0,1105,1,451,21102,825544712960,1,1,21102,1,450,0,1106,0,451,99,109,2,21201,-1,0,1,21101,0,40,2,21102,482,1,3,21102,1,472,0,1106,0,515,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,477,478,493,4,0,1001,477,1,477,108,4,477,10,1006,10,509,1101,0,0,477,109,-2,2106,0,0,0,109,4,2101,0,-1,514,1207,-3,0,10,1006,10,532,21102,1,0,-3,22101,0,-3,1,22102,1,-2,2,21102,1,1,3,21101,551,0,0,1106,0,556,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,579,2207,-4,-2,10,1006,10,579,22102,1,-4,-4,1106,0,647,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,598,0,1106,0,556,22101,0,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,617,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,639,21201,-1,0,1,21102,639,1,0,105,1,514,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0))

rrr <- doCompute(day.11.program)

computerDay11(day.11.program)

fullDrawing <- function(image.data){
  coords <- sapply(names(image.data), function(x){
    as.numeric(strsplit(x, ":")[[1]])
  }) %>% matrix(nrow = 2)
  print(coords)
  dims <- apply(coords, 1, function(x){
    max(x) - min(x) + 1
  })
  print(dims)
  coords[1, ] <- coords[1, ] - min(coords[1, ]) + 1
  coords[2, ] <- coords[2, ] - min(coords[2, ]) + 1
  print(coords)
  drawing <- matrix(" ", nrow = dims[2], ncol = dims[1])
  # print(dim(drawing))
  for(idx in 1:length(image.data)){
    # print(idx)
    # print(coords[1,idx])
    # print(coords[2,idx])
    # print(image.data[[idx]])
    # print(dims[2] - coords[2,idx] + 1)
    # print(dims[1] - coords[1,idx] + 1)
    drawing[coords[2,idx], coords[1,idx]] <- ifelse(image.data[[idx]] == 1, "#", " ")
  }
  print(drawing)
  drawing
}

drawing <- fullDrawing(rrr$image.data$data)

cat(
  paste(
    rev(apply(
      drawing %>% t(), 2, function(x) paste(x, collapse = "")
    )), collapse = "\n"
  )
)
