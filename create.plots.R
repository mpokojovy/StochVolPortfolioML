## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

if (!require("neuralnet")) {
  install.packages("neuralnet")
}

# Set path!
PATH = NULL

if (is.null(PATH)) {
  stop("PATH needs to be specified.")
}

setwd(PATH)

if (!dir.exists("plots")) {
  dir.create("plots")
}

## Load libraries
source("generics.R")
source("lib/SGD.R")
source("lib/market_lib.R")
source("lib/neural_lib.R")
source("lib/portfolio_lib.R")

## Create GBM plots
W0 = 1.0 # initial wealth
nrep.comp = 10000L # set to negative if no utility comparison desired

eta.grid = rev(1.0/seq(from = 1.0/4.0, to = 1.0/1.0, length.out = 7L))
pi.grid  = rep(0.0, length(eta.grid))

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  
  if (exists("optimal.portfolio")) {
    rm(list = "optimal.portfolio")
  }
  
  file.name = paste("output/Portfolio.GBM.eta=", zapsmall(eta, 4), ".RData", sep = "")
  load(file = file.name)
  
  pi.grid[i.eta] = mean(sapply(seq(from = 0.0, to = 1.0, length.out = 100L), as.function(optimal.portfolio@strategy)))
  
  if (i.eta == 1) {
    pi.analyt = (optimal.portfolio@market@param$mu_S - optimal.portfolio@market@param$mu_P)/optimal.portfolio@market@V0
    
    file.name = paste("plots/Market.GBM.pdf", sep = "")
    grDevices::pdf(file.name, width = 10, height = 6)
    innovations(optimal.portfolio@market) = NULL
    plot(optimal.portfolio@market, type = "both")
    grDevices::dev.off()
  }
  
  file.name = paste("plots/Wealth.GBM.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 1)
  grDevices::dev.off()
  
  file.name = paste("plots/Weight.GBM.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 2)
  grDevices::dev.off()
  
  file.name = paste("plots/ANN.GBM.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 3)
  grDevices::dev.off()
  
  file.name = paste("plots/Utility.GBM.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 4)
  grDevices::dev.off()
  
  file.name = paste("plots/SGD.GBM.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 5)
  grDevices::dev.off()
  
  # Evaluate and compare expected utility
  if (nrep.comp > 0) {
    cat("GBM for SP500: eta = ", eta, "\n", sep = "")
    
    myopic.portfolio = optimal.portfolio
    myopic.portfolio@strategy = optimal.portfolio@myopic.strategy
    
    NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = nrep.comp)
    myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = nrep.comp)
    
    cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
    cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")
    cat("\n")
  }
}

file.name = paste("plots/GBM.ANN.vs.Analytic.pdf", sep = "")
grDevices::pdf(file.name, width = 10, height = 6)
plot(1.0/eta.grid, pi.grid, col = "black", xlim = c(0.0, 1.0), ylim = c(min(0.0, pi.analyt), max(0.0, pi.analyt)),
     lty = 1, lwd = 2, xlab = bquote("1/"~eta), ylab = "Stock Weight")
abline(a = 0.0, b = pi.analyt, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Theoretical", "ANN"), lty = c(2, NA), lwd = c(2, NA), pch = c(NA, 1), col = c("red", "black"))
grDevices::dev.off()

## Create Heston plots for SP500 and VIX
eta.grid = 1:4

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  
  if (exists("optimal.portfolio")) {
    rm(list = "optimal.portfolio")
  }
  
  file.name = paste("output/Portfolio.Heston01.eta=", zapsmall(eta, 4), ".RData", sep = "")
  load(file = file.name)
  
  if (i.eta == 1) {
    file.name = paste("plots/Market.Heston01.pdf", sep = "")
    grDevices::pdf(file.name, width = 10, height = 6)
    innovations(optimal.portfolio@market) = NULL
    plot(optimal.portfolio@market, type = "both")
    grDevices::dev.off()
  }
  
  file.name = paste("plots/Wealth.Heston01.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 1)
  grDevices::dev.off()
  
  file.name = paste("plots/Weight.Heston01.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 2)
  grDevices::dev.off()
  
  file.name = paste("plots/ANN.Heston01.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 3)
  grDevices::dev.off()
  
  file.name = paste("plots/Utility.Heston01.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 4)
  grDevices::dev.off()
  
  file.name = paste("plots/SGD.Heston01.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 5)
  grDevices::dev.off()
  
  # Evaluate and compare expected utility
  if (nrep.comp > 0) {
    cat("Heston for SP500 and VIX: eta = ", eta, "\n", sep = "")
    
    myopic.portfolio = optimal.portfolio
    myopic.portfolio@strategy = optimal.portfolio@myopic.strategy
    
    NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = nrep.comp)
    myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = nrep.comp)
    
    cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
    cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")
    cat("\n")
  }
}

try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)

## Create Heston plots for USO and OVX
eta.grid = 1:4

for (i.eta in 1:length(eta.grid)) {
  eta = eta.grid[i.eta]
  
  if (exists("optimal.portfolio")) {
    rm(list = "optimal.portfolio")
  }
  
  file.name = paste("output/Portfolio.Heston02.eta=", zapsmall(eta, 4), ".RData", sep = "")
  load(file = file.name)
  
  if (i.eta == 1) {
    file.name = paste("plots/Market.Heston02.pdf", sep = "")
    grDevices::pdf(file.name, width = 10, height = 6)
    innovations(optimal.portfolio@market) = NULL
    plot(optimal.portfolio@market, type = "both")
    grDevices::dev.off()
  }
  
  file.name = paste("plots/Wealth.Heston02.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 1)
  grDevices::dev.off()
  
  file.name = paste("plots/Weight.Heston02.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 2)
  grDevices::dev.off()
  
  file.name = paste("plots/ANN.Heston02.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 3)
  grDevices::dev.off()
  
  file.name = paste("plots/Utility.Heston02.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 4)
  grDevices::dev.off()
  
  file.name = paste("plots/SGD.Heston02.eta=", zapsmall(eta, 4), ".pdf", sep = "")
  grDevices::pdf(file.name, width = 10, height = 6)
  plot(optimal.portfolio, which = 5)
  grDevices::dev.off()
  
  # Evaluate and compare expected utility
  if (nrep.comp > 0) {
    cat("Heston for USO and OVX: eta = ", eta, "\n", sep = "")
    
    myopic.portfolio = optimal.portfolio
    myopic.portfolio@strategy = optimal.portfolio@myopic.strategy
    
    NN.util     = evaluatePortfolio(optimal.portfolio, W0 = W0, nrep = nrep.comp)
    myopic.util = evaluatePortfolio(myopic.portfolio,  W0 = W0, nrep = nrep.comp)
    
    cat("Expected utility under ANN weight: ",    NN.util$mean,     " (", NN.util$se,")\n", sep = "")
    cat("Expected utility under myopic weight: ", myopic.util$mean, " (", myopic.util$se,")\n", sep = "")
    cat("\n")
  }
}

try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)
