EXIT_OK <- 0
NEED_INPUT <- 1

operators <- list(
  "1" = list(fn = `+`, args = 3, write.to.input = TRUE, do.jump = FALSE)
  , "2" = list(fn = `*`, args = 3, write.to.input = TRUE, do.jump = FALSE)
  , "3" = list(fn = function(x){ x }, args = 1, input.args = TRUE, write.to.input = TRUE, do.jump = FALSE)
  , "4" = list(fn = function(x) {message("Output = ", x); x}, do.output = TRUE, args = 1, write.to.input = FALSE, do.jump = FALSE)
  , "5" = list(fn = function(x, y){
    if(x != 0) return(y)
    return(NA)
  }, args = 2, write.to.input = FALSE, do.jump = TRUE)
  , "6" = list(fn = function(x, y){
    if(x == 0) return(y)
    return(NA)
  }, args = 2, write.to.input = FALSE, do.jump = TRUE)
  , "7" = list(fn = function(x, y){ as.numeric(x < y) }, args = 3, write.to.input = TRUE, do.jump = FALSE)
  , "8" = list(fn = function(x, y){ as.numeric(x == y) }, args = 3, write.to.input = TRUE, do.jump = FALSE)
)

computerDay7(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), input = 1)
computerDay7(c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), input = 0)

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}

singleAmp <-function(program, inputs){
  result <- list(output = 0)
  for(i in 1:length(inputs)) {
    result <- computerDay7(program, input = c(inputs[i], result$output))
  }
  message("Final output = ", result$output)
  result$output
}

amplifiers <- function(program, n = 5) {
  all.combs <- perm(1:n) - 1
  results <- apply(all.combs, 1, function(inputs){
    singleAmp(program, inputs)
  })
  list(all.combs[which.max(results),]) %>% setNames(max(results))
}

amplifiers(c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0))
amplifiers(c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0))
amplifiers(c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
            1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0))
amplifiers(c(3,8,1001,8,10,8,105,1,0,0,21,34,51,76,101,114,195,276,357,438,99999,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,101,4,9,9,102,4,9,9,1001,9,5,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,102,5,9,9,1001,9,2,9,1002,9,2,9,4,9,99,3,9,1001,9,3,9,102,2,9,9,101,4,9,9,102,3,9,9,101,2,9,9,4,9,99,3,9,102,2,9,9,101,4,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99))

computerDay7 <- function(program, offset = 0, input = c(0), output.result = NULL) {
  # message("Offset = ", offset, "; Input = ", paste(program, collapse = ","))
  op.index.mode <- strsplit(as.character(program[offset + 1]), "")[[1]]
  # message("op.index.mode = ", paste(op.index.mode, collapse = ","))
  op.index <- paste(tail(op.index.mode, 2), collapse = "") %>% as.numeric() %>% as.character()
  if(op.index == "99") {
    message("Index is 99, quitting")
    return(list(status = EXIT_OK, output = output.result, program = program, input = input, offset = 0))
  }
  op.spec <- operators[[op.index]]
  op.modes <- numeric()
  if(length(op.index.mode) > 2)
    op.modes <- op.index.mode[1:(length(op.index.mode) - 2)] %>% rev %>% as.numeric()
  op.modes <- c(op.modes, rep(0, op.spec$args - length(op.modes)))
  op.args <- as.numeric(program[(offset + 2) : (offset + 1 + op.spec$args)])
  # message(
  #   "op.index = ", op.index, "; op.spec = ", list(op.spec)
  #   , "; op.args = ", paste(op.args, collapse = ",")
  #   , "; op.modes = ", paste(op.modes, collapse = ",")
  # )
  all.args <- c()
  num.fn.args <- ifelse(op.spec$write.to.input, op.spec$args - 1, op.spec$args)
  # message("num args = ", num.fn.args)
  if(num.fn.args > 0) {
    for(i in 1:num.fn.args) {
      # message(op.args[i], " : ", class(op.args[i]))
      all.args <- c(all.args, as.numeric(ifelse(op.modes[i] == 0, program[op.args[i] + 1], op.args[i])))
      # message("all.args = ", all.args)
    }
  }
  if(!is.null(op.spec$input.args)){
    if(op.spec$input.args){
      if(length(input) == 0) {
        message("Input args required, but not present, quitting")
        return(list(status = NEED_INPUT, output = output.result, program = program, input = input, offset = offset))
      }
      all.args <- c(all.args, input[1])
      input <- tail(input, length(input) - 1)
    }
  }
  # message("all.args = ", paste(all.args, collapse = ","))
  result <- do.call(op.spec$fn, as.list(all.args))
  # message("Result: ", list(op.spec$fn), "(", paste(all.args, collapse = ","), ") = ", result)
  if(op.spec$write.to.input){
    program[tail(op.args, 1) + 1] <- result
    # message(paste(program, collapse = ","))
  }
  if(op.spec$do.jump && !is.na(result))
    new.offset <- result
  else
    new.offset <- offset + op.spec$args + 1
  if(!is.null(op.spec$do.output)) {
    # message("Output 1")
    if(op.spec$do.output){
      # message("Output 2")
      output.result <- result
    }
  }
  # message("New offset = ", new.offset)
  computerDay7(program, new.offset, input, output.result)
}

computerDay7(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5), input = c(9, 0))

computerWithState <- function(program, input) {
  list(
    state = NEED_INPUT
    , program = program
    , input = input
    , output = NULL
    , offset = 0
  )
}


feedbackAmp <- function(program, n = 5) {
  all.combs <- perm(1:n) + 4
  results <- apply(all.combs, 1, function(inputs){
    # print(inputs)
    computers = list()
    for(i in 1:n) {
      computers[[i]] <- computerWithState(program, inputs[i])
    }
    computers[[n]]$output = 0
    all.states <- sapply(computers, function(x) x$state)
    iter = 1
    while(!any(all.states == EXIT_OK)) {
      # print(computers)
      # print(all.states)
      # message("Iter ", iter)
      for(i in 1:n) {
        prev.i <- ifelse(i == 1, 5, i - 1)
        # message("i = ", i, " prev = ", prev.i)
        computers[[i]] <- computerDay7(
          computers[[i]]$program, input = c(computers[[i]]$input, computers[[prev.i]]$output), offset = computers[[i]]$offset
        )
      }
      iter <- iter + 1
      all.states <- sapply(computers, function(x) x$status)
      if(iter > 50)
        break
    }
    computers[[n]]$output
  })
  list(all.combs[which.max(results),]) %>% setNames(max(results))
}

feedbackAmp(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
            27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5))

feedbackAmp(c(3,8,1001,8,10,8,105,1,0,0,21,34,51,76,101,114,195,276,357,438,99999,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,101,4,9,9,102,4,9,9,1001,9,5,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,102,5,9,9,1001,9,2,9,1002,9,2,9,4,9,99,3,9,1001,9,3,9,102,2,9,9,101,4,9,9,102,3,9,9,101,2,9,9,4,9,99,3,9,102,2,9,9,101,4,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99))
