library(magrittr)
library(dplyr)
library(matlib)
library(bit64)

A <- matrix(c(1, 2, -1, 2), 2, 2)
b <- c(2,1)
showEqn(A, b)

colnames(A) < c("x", "y")
rownames(A) < c("x", "y")

Solve(A, b, fractions = TRUE)

getQuantities <- function(x) {
  parts <- strsplit(trimws(x, "both"), " ")[[1]]
  c(as.integer(parts[1])) %>% setNames(parts[2])
}

getQuantities(c(" 10 ORE   "))

splitReaction <- function(reaction) {
  in.out <- strsplit(reaction, "=>")[[1]]
  in.parts <- strsplit(in.out[1], ",")[[1]]
  list(
    "in" = lapply(in.parts, getQuantities) %>% unlist()
    , "out" = getQuantities(in.out[2])
  )
}

splitReaction("7 A, 1 E => 1 FUEL")

readReactions <- function(def) {
  lapply(strsplit(def, "\n")[[1]], splitReaction)
}

test.r.1 <- readReactions("10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

makeEqMatrix <- function(reactions) {
  vars <- c("ORE", "FUEL")
  for(r in reactions) {
    for(chem in c(names(r$`in`), names(r$out))) {
      if(!chem %in% vars)
        vars <- c(vars, chem)
    }
  }
  eq.matrix <- matrix(0L, ncol = length(vars), nrow = length(reactions))
  for(row.num in 1:length(reactions)) {
    r <- reactions[[row.num]]
    for(chem in names(r$`in`)) {
      pos <- which(vars == chem)
      eq.matrix[row.num, pos] <- r$`in`[chem] %>% as.integer()
    }
    for(chem in names(r$out)) {
      pos <- which(vars == chem)
      eq.matrix[row.num, pos] <- - r$out[chem] %>% as.integer()
    }
  }
  colnames(eq.matrix) <- vars
  # message("Full eq.matrix")
  # print(eq.matrix)
  eq.matrix
}

eq <- makeEqMatrix(test.r.1)

elimination <- function(eq) {
  row.n.fuel <- which(apply(eq, 1, function(x) x[2] != 0))
  row.fuel <- eq[row.n.fuel, ]
  prod.eq <- eq[-row.n.fuel, ]
  iters <- 1
  while(any(tail(row.fuel, -2) > 0)) {
    iters <- iters + 1
    e.col <- (which(tail(row.fuel, -2) > 0))[1] + 2
    e.row <- which(apply(prod.eq, 1, function(x) x[e.col] < 0))
    mul <- ceiling(row.fuel[e.col] / abs(prod.eq[e.row, e.col]))
    # message("iter ", iters, " mod = ", paste(row.fuel, ","))
    row.fuel <- row.fuel + prod.eq[e.row, ] * mul
    # message("iter ", iters, " row.fuel = ", paste(row.fuel, ","))
    if(iters > 10000){
      message("quit, reached iters = ", iters)
      break
    }
  }
  row.fuel
}

elimination(eq)

getOreCnt <- function(raw.reactions) {
  readReactions(raw.reactions) %>%
    makeEqMatrix() %>%
    elimination()
}

getOreCnt("9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

getOreCnt("157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

getOreCnt("2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")

getOreCnt("171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX")

getOreCnt("1 HKCVW, 2 DFCT => 5 ZJZRN
8 TCPN, 7 XHTJF, 3 DFCT => 8 ZKCXK
1 ZJZRN, 4 NZVL, 1 NJFXK, 7 RHJCQ, 32 MCQS, 1 XFNPT => 5 ZWQX
10 DRWB, 16 JBHKV => 6 TCPN
3 MBFK => 7 DRWB
9 RHJCQ => 6 MBMKZ
1 BVFPF => 2 KRTGD
1 QNXC, 7 BKNQT, 1 XFNPT => 4 VNFJQ
2 TCPN, 1 WFSV => 2 TGJP
35 DFCT => 2 RHJCQ
1 SKBV, 7 CTRH => 8 QGDSV
8 VSRMJ, 1 BVFPF => 4 CTRH
1 WMCD => 3 FPZLF
13 CVJQG, 8 DXBZJ => 9 QBDQ
1 XSRWM => 5 GDJGV
132 ORE => 3 MBFK
2 BQGP => 9 LZKJZ
5 GZLHP => 7 WFSV
2 RXSZS, 10 MBFK, 1 BPNVK => 2 GZLHP
13 BZFH => 8 XSRWM
3 QLSVN => 3 SKBV
8 QBDQ => 4 VSRMJ
1 RXSZS => 9 CVJQG
3 MBFK => 3 BVFPF
7 GZLHP, 4 MBFK, 5 CVJQG => 8 XHTJF
1 GZLHP => 2 DFCT
4 SZDWB, 4 RHJCQ, 1 WMCD => 3 RGZDK
2 BRXLV => 8 DXBZJ
192 ORE => 7 RXSZS
1 PRMR, 6 DFCT => 5 SZDWB
104 ORE => 9 BPNVK
6 VLJWQ, 8 ZKCXK, 6 BKNQT, 26 JRXQ, 7 FPZLF, 6 HKCVW, 18 KRTGD => 4 RBFX
7 XFNPT, 1 GDJGV => 2 HJDB
15 SKBV, 8 DRWB, 12 RXSZS => 3 GHQPH
1 BZFH => 5 GCBR
1 TGJP, 6 SKBV => 1 BZFH
4 KRTGD, 1 ZJHKP, 1 LZKJZ, 1 VNFJQ, 6 QBDQ, 1 PRMR, 1 NJFXK, 1 HJDB => 8 TFQH
10 BVFPF, 1 RGZDK => 8 QNXC
1 XHTJF => 5 JRXQ
3 XKTMK, 4 QGDSV => 3 ZJHKP
2 BZFH => 7 PRMR
1 BPNVK, 1 RXSZS => 5 JBHKV
10 XHTJF => 9 BKNQT
1 JBHKV, 2 XHTJF => 8 QLSVN
24 VNFJQ, 42 TFQH, 39 RBFX, 1 ZWQX, 7 VBHVQ, 26 DRWB, 21 NJFXK => 1 FUEL
26 WBKQ, 14 XHTJF => 5 BQGP
5 WBKQ, 7 MBMKZ => 3 LQGC
6 LQGC => 5 NZVL
13 KRTGD, 5 GHQPH => 9 VLJWQ
117 ORE => 4 BRXLV
3 XKTMK, 1 PRMR => 2 MCQS
3 DRWB, 7 BVFPF, 4 TCPN => 7 NJFXK
10 VHFCR, 13 JZQJ => 5 XKTMK
17 CVJQG, 4 GCBR => 9 HKCVW
22 DFCT, 17 TGJP => 2 WBKQ
2 JZQJ, 12 XFNPT, 1 BQGP => 2 VBHVQ
12 HKCVW => 1 JZQJ
1 XSRWM => 3 WMCD
12 BZFH, 14 SKBV, 1 CTRH => 4 XFNPT
7 ZKCXK => 6 VHFCR")

elimination2 <- function(eq, n.ore = 1000000000000) {
  row.n.fuel <- which(apply(eq, 1, function(x) x[2] != 0))
  row.fuel <- elimination(eq)
  mul <- n.ore / row.fuel["ORE"]
  eq[row.n.fuel, ] <- eq[row.n.fuel, ] * mul
  row.fuel2 <- elimination(eq)
  iters <- 1
  while(round(row.fuel2["FUEL"]) != round(row.fuel["FUEL"])) {
    row.fuel <- row.fuel2
    mul <- n.ore / row.fuel["ORE"]
    eq[row.n.fuel, ] <- eq[row.n.fuel, ] * mul
    row.fuel2 <- elimination(eq)
    iters <- iters + 1
    if(iters > 100) {
      message("quite, to many iters = ", iters)
      break
    }
  }
  as.integer64(c(round(row.fuel2["FUEL"]), round(row.fuel2["ORE"])))
}

getOreCnt2 <- function(raw.reactions) {
  readReactions(raw.reactions) %>%
    makeEqMatrix() %>%
    elimination2()
}

getOreCnt2("157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

getOreCnt2("2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")

getOreCnt2("171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX")

getOreCnt2("1 HKCVW, 2 DFCT => 5 ZJZRN
8 TCPN, 7 XHTJF, 3 DFCT => 8 ZKCXK
1 ZJZRN, 4 NZVL, 1 NJFXK, 7 RHJCQ, 32 MCQS, 1 XFNPT => 5 ZWQX
10 DRWB, 16 JBHKV => 6 TCPN
3 MBFK => 7 DRWB
9 RHJCQ => 6 MBMKZ
1 BVFPF => 2 KRTGD
1 QNXC, 7 BKNQT, 1 XFNPT => 4 VNFJQ
2 TCPN, 1 WFSV => 2 TGJP
35 DFCT => 2 RHJCQ
1 SKBV, 7 CTRH => 8 QGDSV
8 VSRMJ, 1 BVFPF => 4 CTRH
1 WMCD => 3 FPZLF
13 CVJQG, 8 DXBZJ => 9 QBDQ
1 XSRWM => 5 GDJGV
132 ORE => 3 MBFK
2 BQGP => 9 LZKJZ
5 GZLHP => 7 WFSV
2 RXSZS, 10 MBFK, 1 BPNVK => 2 GZLHP
13 BZFH => 8 XSRWM
3 QLSVN => 3 SKBV
8 QBDQ => 4 VSRMJ
1 RXSZS => 9 CVJQG
3 MBFK => 3 BVFPF
7 GZLHP, 4 MBFK, 5 CVJQG => 8 XHTJF
1 GZLHP => 2 DFCT
4 SZDWB, 4 RHJCQ, 1 WMCD => 3 RGZDK
2 BRXLV => 8 DXBZJ
192 ORE => 7 RXSZS
1 PRMR, 6 DFCT => 5 SZDWB
104 ORE => 9 BPNVK
6 VLJWQ, 8 ZKCXK, 6 BKNQT, 26 JRXQ, 7 FPZLF, 6 HKCVW, 18 KRTGD => 4 RBFX
7 XFNPT, 1 GDJGV => 2 HJDB
15 SKBV, 8 DRWB, 12 RXSZS => 3 GHQPH
1 BZFH => 5 GCBR
1 TGJP, 6 SKBV => 1 BZFH
4 KRTGD, 1 ZJHKP, 1 LZKJZ, 1 VNFJQ, 6 QBDQ, 1 PRMR, 1 NJFXK, 1 HJDB => 8 TFQH
10 BVFPF, 1 RGZDK => 8 QNXC
1 XHTJF => 5 JRXQ
3 XKTMK, 4 QGDSV => 3 ZJHKP
2 BZFH => 7 PRMR
1 BPNVK, 1 RXSZS => 5 JBHKV
10 XHTJF => 9 BKNQT
1 JBHKV, 2 XHTJF => 8 QLSVN
24 VNFJQ, 42 TFQH, 39 RBFX, 1 ZWQX, 7 VBHVQ, 26 DRWB, 21 NJFXK => 1 FUEL
26 WBKQ, 14 XHTJF => 5 BQGP
5 WBKQ, 7 MBMKZ => 3 LQGC
6 LQGC => 5 NZVL
13 KRTGD, 5 GHQPH => 9 VLJWQ
117 ORE => 4 BRXLV
3 XKTMK, 1 PRMR => 2 MCQS
3 DRWB, 7 BVFPF, 4 TCPN => 7 NJFXK
10 VHFCR, 13 JZQJ => 5 XKTMK
17 CVJQG, 4 GCBR => 9 HKCVW
22 DFCT, 17 TGJP => 2 WBKQ
2 JZQJ, 12 XFNPT, 1 BQGP => 2 VBHVQ
12 HKCVW => 1 JZQJ
1 XSRWM => 3 WMCD
12 BZFH, 14 SKBV, 1 CTRH => 4 XFNPT
7 ZKCXK => 6 VHFCR")
