## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

# Set path!
PATH = NULL

if (is.null(PATH)) {
  stop("PATH needs to be specified.")
}

setwd(PATH)

## Load libraries
source("generics.R")
source("lib/SGD.R")
source("lib/market_lib.R")
source("lib/neural_lib.R")
source("lib/portfolio_lib.R")

## Run test examples

eta = 1.0
my.utility = function(W) terminal.utility(W, utility = function(W) isoelastic.utility(W, eta))
attr(my.utility, "eta") = eta

flag = "Heston02" # "GBM", "Heston01" or "Heston02"

if (!is.element(tolower(flag), c("heston01", "heston02"))) {
  ## Vanilla GBM
  set.seed(1L)

  nstep = c(100L, 100L, 100L)
  batch = c(10L, 10L, 50L)
  learning_rate = c(0.1, 0.05, 0.01)

  tic = proc.time()
  cat("Test: Vanilla GBM, isoelastic utility with power eta =", eta, "\n")
  source("runs/GBM.run.R")
  print(proc.time() - tic)

  ## Evaluate and compare expected utility
  myopic.portfolio = optimal.portfolio
  myopic.portfolio@strategy = optimal.portfolio@myopic.strategy

  NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = 10000L)
  myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = 10000L)

  cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
  cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")

  cat("\n\n")
} else if (tolower(flag) == "heston01") {
  # Heston for SP500 and VIX
  set.seed(11L)
  
  nstep = c(1500L, 500L, 100L)
  batch = c(10L, 10L, 50L)
  learning_rate = c(0.05, 0.01, 0.01)
  
  tic = proc.time()
  cat("Test: Heston for SP500 and VIX, isoelastic utility with power eta =", eta, "\n")
  source("runs/Heston01.run.R")
  print(proc.time() - tic)
  
  # Evaluate and compare expected utility
  myopic.portfolio = optimal.portfolio
  myopic.portfolio@strategy = optimal.portfolio@myopic.strategy
  
  NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = 10000)
  myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = 10000)
  
  cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
  cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")
  
  plot(optimal.portfolio, which = 1)
  plot(optimal.portfolio, which = 2)
  plot(optimal.portfolio, which = 3)
  plot(optimal.portfolio, which = 4)
  plot(optimal.portfolio, which = 5)
} else {
  # Heston for USO and OVX
  set.seed(15L)
  
  nstep = c(1500L, 500L, 100L)
  batch = c(10L, 10L, 50L)
  learning_rate = c(0.05, 0.01, 0.01)
  
  tic = proc.time()
  cat("Test: Heston for USO and OVX, isoelastic utility with power eta =", eta, "\n")
  source("runs/Heston02.run.R")
  print(proc.time() - tic)
  
  # Evaluate and compare expected utility
  myopic.portfolio = optimal.portfolio
  myopic.portfolio@strategy = optimal.portfolio@myopic.strategy
  
  NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = 10000)
  myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = 10000)
  
  cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
  cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")
  
  plot(optimal.portfolio, which = 1)
  plot(optimal.portfolio, which = 2)
  plot(optimal.portfolio, which = 3)
  plot(optimal.portfolio, which = 4)
  plot(optimal.portfolio, which = 5)
}
