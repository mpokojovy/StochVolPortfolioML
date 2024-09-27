## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

# Set path!
PATH = NULL

if (is.null(PATH)) {
  stop("PATH needs to be specified.")
}

setwd(PATH)

if (!dir.exists("output")) {
  dir.create("output")
}

## Load libraries
source("generics.R")
source("lib/SGD.R")
source("lib/market_lib.R")
source("lib/neural_lib.R")
source("lib/portfolio_lib.R")

tic = proc.time()

## Do vanilla GBM run
nstep = c(100L, 100L, 500L)
batch = c(10L, 10L, 50L)
learning_rate = c(0.1, 0.05, 0.01)

eta.grid  = rev(1.0/seq(from = 1.0/4.0, to = 1.0/1.0, length.out = 7L))
seed.grid = 1:length(eta.grid)

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  set.seed(seed.grid[i.eta])

  my.utility = function(W) terminal.utility(W, utility = function(W) isoelastic.utility(W, eta))
  attr(my.utility, "eta") = eta

  cat("Run: Vanilla GBM for SP500, isoelastic utility with power eta =", zapsmall(eta, 4), "\n")
  source("runs/GBM.run.R")
  cat("\n")

  file.name = paste("output/Portfolio.GBM.eta=", zapsmall(eta, 4), ".RData", sep = "")
  save(file = file.name, list = "optimal.portfolio")
}

cat("\n")

## Do Heston for run for SP500 and VIX
nstep = c(1500L, 500L, 500L)
batch = c(10L, 10L, 50L)
learning_rate = c(0.05, 0.01, 0.01)

eta.grid  = 1:4
seed.grid = 10 + (1:4)

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  set.seed(seed.grid[i.eta])

  my.utility = function(W) terminal.utility(W, utility = function(W) isoelastic.utility(W, eta))
  attr(my.utility, "eta") = eta

  cat("Run: Heston for SP500 and VIX, isoelastic utility with power eta =", zapsmall(eta, 4), "\n")
  source("runs/Heston01.run.R")
  cat("\n")

  file.name = paste("output/Portfolio.Heston01.eta=", zapsmall(eta, 4), ".RData", sep = "")
  save(file = file.name, list = "optimal.portfolio")
}

## Do Heston for run for USO and OVX
nstep = c(1500L, 500L, 500L)
batch = c(10L, 10L, 50L)
learning_rate = c(0.05, 0.01, 0.01)

eta.grid  = 1:4
seed.grid = 14 + (1:4)

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  set.seed(seed.grid[i.eta])

  my.utility = function(W) terminal.utility(W, utility = function(W) isoelastic.utility(W, eta))
  attr(my.utility, "eta") = eta

  cat("Run: Heston for USO and OVX, isoelastic utility with power eta =", zapsmall(eta, 4), "\n")
  source("runs/Heston02.run.R")
  cat("\n")

  file.name = paste("output/Portfolio.Heston02.eta=", zapsmall(eta, 4), ".RData", sep = "")
  save(file = file.name, list = "optimal.portfolio")
}

print(proc.time() - tic)
