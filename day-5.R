# --- Day 5: Sunny with a Chance of Asteroids ---
#   
#   You're starting to sweat as the ship makes its way toward Mercury. The Elves suggest that you get the air conditioner working by upgrading your ship computer to support the Thermal Environment Supervision Terminal.
# 
# The Thermal Environment Supervision Terminal (TEST) starts by running a diagnostic program (your puzzle input). The TEST diagnostic program will run on your existing Intcode computer after a few modifications:
# 
# First, you'll need to add two new instructions:
#   
#   Opcode 3 takes a single integer as input and saves it to the address given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
# Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
# 
# Programs that use these instructions will come with documentation that explains what should be connected to the input and output. The program 3,0,4,0,99 outputs whatever it gets as input, then halts.
# 
# Second, you'll need to add support for parameter modes:
# 
# Each parameter of an instruction is handled based on its parameter mode. Right now, your ship computer already understands parameter mode 0, position mode, which causes the parameter to be interpreted as a position - if the parameter is 50, its value is the value stored at address 50 in memory. Until now, all parameters have been in position mode.
# 
# Now, your ship computer will also need to handle parameters in mode 1, immediate mode. In immediate mode, a parameter is interpreted as a value - if the parameter is 50, its value is simply 50.
# 
# Parameter modes are stored in the same value as the instruction's opcode. The opcode is a two-digit number based only on the ones and tens digit of the value, that is, the opcode is the rightmost two digits of the first value in an instruction. Parameter modes are single digits, one per parameter, read right-to-left from the opcode: the first parameter's mode is in the hundreds digit, the second parameter's mode is in the thousands digit, the third parameter's mode is in the ten-thousands digit, and so on. Any missing modes are 0.
# 
# For example, consider the program 1002,4,3,4,33.
# 
# The first instruction, 1002,4,3,4, is a multiply instruction - the rightmost two digits of the first value, 02, indicate opcode 2, multiplication. Then, going right to left, the parameter modes are 0 (hundreds digit), 1 (thousands digit), and 0 (ten-thousands digit, not present and therefore zero):
# 
# ABCDE
#  1002
# 
# DE - two-digit opcode,      02 == opcode 2
#  C - mode of 1st parameter,  0 == position mode
#  B - mode of 2nd parameter,  1 == immediate mode
#  A - mode of 3rd parameter,  0 == position mode,
#                                   omitted due to being a leading zero
# 
# This instruction multiplies its first two parameters. The first parameter, 4 in position mode, works like it did before - its value is the value stored at address 4 (33). The second parameter, 3 in immediate mode, simply has value 3. The result of this operation, 33 * 3 = 99, is written according to the third parameter, 4 in position mode, which also works like it did before - 99 is written to address 4.
# 
# Parameters that an instruction writes to will never be in immediate mode.
# 
# Finally, some notes:
# 
#     It is important to remember that the instruction pointer should increase by the number of values in the instruction after the instruction finishes. Because of the new instructions, this amount is no longer always 4.
#     Integers can be negative: 1101,100,-1,4,0 is a valid program (find 100 + -1, store the result in position 4).
# 
# The TEST diagnostic program will start by requesting from the user the ID of the system to test by running an input instruction - provide it 1, the ID for the ship's air conditioner unit.
# 
# It will then perform a series of diagnostic tests confirming that various parts of the Intcode computer, like parameter modes, function correctly. For each test, it will run an output instruction indicating how far the result of the test was from the expected value, where 0 means the test was successful. Non-zero outputs mean that a function is not working correctly; check the instructions that were run before the output instruction to see which one failed.
# 
# Finally, the program will output a diagnostic code and immediately halt. This final output isn't an error; an output followed immediately by a halt means the program finished. If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.
# 
# After providing 1 to the only input instruction and passing all the tests, what diagnostic code does the program produce?

operators <- list(
  "1" = list(fn = `+`, args = 3, write.to.input = TRUE, do.jump = FALSE)
  , "2" = list(fn = `*`, args = 3, write.to.input = TRUE, do.jump = FALSE)
  , "3" = list(fn = function(){ readline("Please provide input: ") }, args = 1, write.to.input = TRUE, do.jump = FALSE)
  , "4" = list(fn = function(x) {message("Output = ", x); x}, args = 1, write.to.input = FALSE, do.jump = FALSE)
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

computerDay5 <- function(inputs, offset = 0) {
  message("Offset = ", offset, "; Input = ", paste(inputs, collapse = ","))
  op.index.mode <- strsplit(as.character(inputs[offset + 1]), "")[[1]]
  message("op.index.mode = ", paste(op.index.mode, collapse = ","))
  op.index <- paste(tail(op.index.mode, 2), collapse = "") %>% as.numeric() %>% as.character()
  if(op.index == "99") {
    message("Index is 99, quitting")
    return(inputs)
  }
  op.spec <- operators[[op.index]]
  op.modes <- numeric()
  if(length(op.index.mode) > 2)
    op.modes <- op.index.mode[1:(length(op.index.mode) - 2)] %>% rev %>% as.numeric()
  op.modes <- c(op.modes, rep(0, op.spec$args - length(op.modes)))
  op.args <- as.numeric(inputs[(offset + 2) : (offset + 1 + op.spec$args)])
  message(
    "op.index = ", op.index, "; op.spec = ", list(op.spec)
    , "; op.args = ", paste(op.args, collapse = ",")
    , "; op.modes = ", paste(op.modes, collapse = ",")
  )
  all.args <- c()
  num.fn.args <- ifelse(op.spec$write.to.input, op.spec$args - 1, op.spec$args)
  if(num.fn.args > 0) {
    for(i in 1:num.fn.args) {
      # message(op.args[i], " : ", class(op.args[i]))
      all.args <- c(all.args, as.numeric(ifelse(op.modes[i] == 0, inputs[op.args[i] + 1], op.args[i])))
    }
  }
  message("all.args = ", paste(all.args, collapse = ","))
  result <- do.call(op.spec$fn, as.list(all.args))
  message("Result: ", list(op.spec$fn), "(", paste(all.args, collapse = ","), ") = ", result)
  if(op.spec$write.to.input){
    inputs[tail(op.args, 1) + 1] <- result
    message(inputs)
  }
  if(op.spec$do.jump && !is.na(result))
    new.offset <- result
  else
    new.offset <- offset + op.spec$args + 1
  message("New offset = ", new.offset)
  computerDay5(inputs, new.offset)
}

computerDay5(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9))
computerDay5(c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1))

computerDay5(c(3,225,1,225,6,6,1100,1,238,225,104,0,1101,78,5,225,1,166,139,224,101,-74,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1002,136,18,224,101,-918,224,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1001,83,84,224,1001,224,-139,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,55,20,225,1101,53,94,225,2,217,87,224,1001,224,-2120,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,102,37,14,224,101,-185,224,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,1101,8,51,225,1102,46,15,225,1102,88,87,224,1001,224,-7656,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,29,28,225,1101,58,43,224,1001,224,-101,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1101,93,54,225,101,40,191,224,1001,224,-133,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,40,79,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,344,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,389,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,434,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,464,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,569,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,584,1001,223,1,223,7,677,677,224,1002,223,2,223,1005,224,599,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,629,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,644,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,659,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226))

computerDay5(c(0101,0,0,0,99))

computerDay5(c(1,0,0,0,99))
computerDay5(c(2,3,0,3,99))
computerDay5(c(2,4,4,5,99,0))
computerDay5(c(1,1,1,4,99,5,6,0,99))
computerDay5(c(1002,4,3,4,33))
computerDay5(c(1101,100,-1,4,0))

computerDay5(c(3,9,8,9,10,9,4,9,99,-1,8))
computerDay5(c(3,9,7,9,10,9,4,9,99,-1,8))
computerDay5(c(3,3,1108,-1,8,3,4,3,99))
