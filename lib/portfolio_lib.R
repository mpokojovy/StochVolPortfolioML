## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

## Utility functions

isoelastic.utility = function(W, eta = 1.0) {
  if (eta < 0) {
    stop("eta needs to be greater or equal 0")
  }
  
  if (eta == 1.0) {
    return(log(W))
  } else {
    (W^(1.0 - eta) - 1.0)/(1.0 - eta)
  }
}

terminal.utility = function(W, utility = isoelastic.utility) {
  n = length(W)
  return(utility(W[n]))
}

## Feedback

setClassUnion("Feedback", c("function", "MLP"))

## Optimal portfolio class

setClass("Portfolio", 
  slots = c(
    W0 = "numeric",
    market = "Market",
    utility = "function",
    obj = "numeric",
    strategy = "Feedback",
    myopic.strategy = "function",
    training.time = "numeric",
    obj.path = "numeric",
    step.path = "numeric",
    grad.path = "numeric"),
  prototype = c(
    W0 = NA_real_,
    market = new("Market"),
    utility = terminal.utility,
    obj = NA_real_,
    strategy = function(x) 0.0,
    myopic.strategy = function(x) 0.0,
    training.time = NA_real_,
    obj.path = numeric(),
    step.path = numeric(),
    grad.path = numeric())
)

setMethod("plot", "Portfolio", function(x, which, ...) {
  eta = attr(x@utility, "eta")
  
  gbm.flag    = !is.element(tolower(x@market@vol.type), c("heston", "garch", "3/2", "kraft"))
  heston.flag = tolower(x@market@vol.type) == "heston"
  
  if (!is.element(tolower(x@market@vol.type), c("heston", "garch", "3/2", "kraft"))) {
    merton.ratio = x@myopic.strategy(c(1.0)) # t = time.horizon(x@market)
  } else {
    merton.ratio = function(v) x@myopic.strategy(c(1.0, v/x@market@V0)) # t = time.horizon(x@market)
  }

  if (which == 1) {
    # Plot simulated optimal wealth
    
    par(mfcol = c(1, 2))
    
    nrep = 10L
    
    n  = x@market@n
    dt = x@market@dt
    t.grid = seq(from = 0.0, to = horizon(x@market), by = dt)
    
    wealth.mat.ANN    = matrix(0.0, nrow = n + 1, ncol = nrep)
    wealth.mat.myopic = matrix(0.0, nrow = n + 1, ncol = nrep)
    
    for (rep in 1L:nrep) {
      innovations(x@market) = NULL
      
      wealth.mat.ANN[, rep]    = c(x@W0, computeUtility(x@W0, x@market, x@strategy,        x@utility)$wealth)
      wealth.mat.myopic[, rep] = c(x@W0, computeUtility(x@W0, x@market, x@myopic.strategy, x@utility)$wealth)
    }
    
    wealth.min = min(wealth.mat.ANN, wealth.mat.myopic)
    wealth.max = max(wealth.mat.ANN, wealth.mat.myopic)
    
    ylim = c(0.5*(wealth.min + wealth.max) - 0.7*(wealth.max - wealth.min),
             0.5*(wealth.min + wealth.max) + 0.7*(wealth.max - wealth.min))
    
    caption = "Optimal Wealth Process under ANN"
    
    plot(t.grid, rep(0.0, length(t.grid)), ylim = ylim, type = "n",
         xlab = "Time (in years)", ylab = "Simulated Optimal Wealth (in $)", main = caption)
    
    for (rep in 1L:nrep) {
      lines(t.grid, wealth.mat.ANN[, rep], col = "black")
    }
    
    caption = paste("Optimal Wealth Process under", 
                    if (((eta == 1) && (heston.flag)) || (gbm.flag)) "Analytic" else "Myopic")
    
    plot(t.grid, rep(0.0, length(t.grid)), ylim = ylim, type = "n",
         xlab = "Time (in years)", ylab = "Simulated Optimal Wealth (in $)", main = caption)
    
    for (rep in 1L:nrep) {
      lines(t.grid, wealth.mat.myopic[, rep], col = "black")
    }
    
  } else if (which == 2) {
    # Plot stock weight
    
    par(mfcol = c(1, 1))
    
    # sample volatilities
    .market = x@market
    V.quant = c(0.0, 0.0)
    S.quant = c(0.0, 0.0)
    
    for (rep in 1:20) {
      innovations(.market) = NULL
      V.quant = V.quant + quantile(marketForwardMap(.market)[, 3], c(0.025, 0.975))/20
      S.quant = S.quant + quantile(marketForwardMap(.market)[, 2], c(0.025, 0.975))/20
    }
    
    if (is.element(tolower(x@market@vol.type), c("heston", "garch", "3/2", "kraft"))) {
      # prepare plot
      x0 = c(x@market@S0, x@market@V0)
      
      t.grid = seq(from = 0.0, to = horizon(x@market), length.out = 9)
      V.grid = seq(from = V.quant[1], to = V.quant[2], length.out = 500L)
      
      pi.array = matrix(0.0, nrow = length(V.grid), ncol = length(t.grid))
      
      for (i.t in 1L:length(t.grid)) {
        pi.array[, i.t] = sapply(1:length(V.grid), 
                                 function(i) as.function(x@strategy)(c(t.grid[i.t]/horizon(x@market), V.grid[i]/x@market@V0)))
      }
      
      pi.analyt = sapply(V.grid, merton.ratio)
      
      pi.min = min(pi.array, pi.analyt)
      pi.max = max(pi.array, pi.analyt)
      
      ylim = c(0.5*(pi.min + pi.max) - 0.6*(pi.max - pi.min) - 0.5,
               0.5*(pi.min + pi.max) + 0.8*(pi.max - pi.min) + 0.5)
      
      col.vec = rgb(0.5, 0.5, seq(from = 0.25, to = 1.0, length.out = length(t.grid)))
      
      plot(V.grid , pi.array[, 1], type = "n", ylim = ylim,
           main = "Merton Ratio", xlab = "Squared Volatility V", ylab = "Stock Weight", lty = 1, lwd = 2)
      
      for (i.t in 1L:length(t.grid)) {
        lines(V.grid, pi.array[, i.t], col = col.vec[i.t], lty = 1, lwd = 2)
      }
      
      lines(V.grid, pi.analyt, col = "red", lty = 2, lwd = 2)
      
      if ((eta == 1) && (heston.flag)) {
        legend("topright", legend = as.expression(c(sapply(zapsmall(t.grid, 4), function(t) bquote("ANN"~italic(italic(t)*"="*.(t)))), bquote("Analytic"))),
               lty = c(rep(1, length(t.grid)), 2), lwd = c(rep(2, length(t.grid)), 2), col = c(col.vec, rgb(1, 0, 0)))
      } else {
        legend("topright", legend = as.expression(c(sapply(zapsmall(t.grid, 4), function(t) bquote("ANN"~italic(italic(t)*"="*.(t)))), bquote("Myopic"))),
               lty = c(rep(1, length(t.grid)), 2), lwd = c(rep(2, length(t.grid)), 2), col = c(col.vec, rgb(1, 0, 0)))
      }
    } else {
      # prepare plot
      x0 = c(x@market@S0, x@market@V0)
      
      t.grid = seq(from = 0.0, to = horizon(x@market), length.out = 500)
      
      pi.array = sapply(1:length(t.grid), function(i) as.function(x@strategy)(c(t.grid[i]/horizon(x@market))))
      
      pi.min = min(pi.array, merton.ratio)
      pi.max = max(pi.array, merton.ratio)
      
      ylim = c(0.5*(pi.min + pi.max) - 0.6*(pi.max - pi.min) - 0.5,
               0.5*(pi.min + pi.max) + 0.8*(pi.max - pi.min) + 0.5)
      
      plot(t.grid , pi.array, type = "l", ylim = ylim,
           main = "Merton Ratio", xlab = bquote("Time"~t), ylab = "Stock Weight", lty = 1, lwd = 2)
      
      lines(t.grid, merton.ratio*rep(1, length(t.grid)), col = "red", lwd = 2, lty = 2)
      
      legend("topright", legend = c("ANN", "Analytic"), col = c("black", "red"), lty = c(1, 2), lwd = c(2, 2))
    }
  } else if (which == 3) {
    # Plot ANN
    
    hidden = optimal.portfolio@strategy@layer_size[c(-1, -length(optimal.portfolio@strategy@layer_size))]
    
    if (is.element(tolower(x@market@vol.type), c("heston", "garch", "3/2"))) {
      ANN.dummy = neuralnet::neuralnet(pi ~ t + y, data = data.frame(pi = 0, t = 0, y = 0), hidden = hidden, linear.output = TRUE)
    } else {
      ANN.dummy = neuralnet::neuralnet(pi ~ t, data = data.frame(pi = 0, t = 0), hidden = hidden, linear.output = TRUE)
    }
    
    for (i in 1:(length(x@strategy@layer_size) - 1)) {
      ANN.dummy$weights[[1]][[i]] = t(cbind(x@strategy@.weights$b[[i]], x@strategy@.weights$W[[i]]))
    }
    
    plot(ANN.dummy, rep = "best")
  } else if (which == 4) {
    # Plot utility path
    
    par(mfcol = c(1, 1))

    umin = min(x@obj.path)
    umax = max(x@obj.path)

    ylim = c(0.5*(umin + umax) - 0.60*(umax - umin),
             0.5*(umin + umax) + 0.75*(umax - umin))

    plot(1:length(x@obj.path), x@obj.path, ylim = ylim,
         main = paste("Terminal Value:", zapsmall(x@obj.path[length(x@obj.path)], digits = 4)),
         xlab = "Step", ylab = "Empirical Expected Utility", lty = 1)
    
    lines(1:length(x@obj.path), x@obj.path)
  } else {
    # Plot step and grad path
    
    par(mfcol = c(1, 1))
    
    umin = min(x@step.path, x@grad.path)
    umax = max(x@step.path, x@grad.path)
    
    ylim = c(0.5*(umin + umax) - 0.60*(umax - umin),
             0.5*(umin + umax) + 0.75*(umax - umin))
    
    plot(1:length(x@obj.path), x@grad.path, type = "l", ylim = ylim, xlab = "Step", ylab = "Euclidean norm", 
         col = "blue", lty = 2, lwd = 1)
    lines(1:length(x@obj.path), x@step.path,  col = "black", lty = 1, lwd = 1)
    
    legend("topleft", c("Objective Gradient", "Adam SGD Step"), 
           col = c("blue", "black"), lty = c(2, 1), lwd = c(1, 1))
  }
})

# Compute Utility
computeUtility = function(W0, market, strategy, utility = terminalUtility,
                          grad.too = FALSE, eps = 1E-6) {
  x  = marketForwardMap(market)
  x0 = c(1.0, market@S0, market@V0)

  n = nrow(x)

  wealth = rep(0.0, n)
  W = W0
  
  x.prev = x0

  wealthProcess = function(strategy) {
    wealth = rep(0.0, n)
    
    for (i in 1L:n) {
      if (is.element(tolower(market@vol.type), c("heston", "garch", "3/2", "kraft"))) {
        prop = as.function(strategy)(c(i/n, x.prev[3]/x0[3]))
      } else {
        prop = as.function(strategy)(c(i/n))
      }
      
      prop = c(1 - as.numeric(prop), as.numeric(prop))

      W = W*(1.0 + sum(prop*(x[i, 1:2] - x.prev[1:2])/x.prev[1:2]))
      x.prev = x[i, ]
      
      wealth[i] = max(W, 1E-6*W0)
    }

    return(wealth)
  }

  wealth = wealthProcess(strategy)
  obj = utility(wealth)

  if (grad.too) {
    if (typeof(strategy) != "function") {
      "The type of strategy needs to be MLP to compute the gradient"
    }
    
    grad = gradMLP(strategy, function(strategy) utility(wealthProcess(strategy)), eps)

    return(list(wealth = wealth, obj = obj, grad = grad))
  }

  return(list(wealth = wealth, obj = obj))
}

# Initialize strategy
initStrategy = function(input_layer_size, deep_layer_size = c(5), activation = c(replicate(length(layer_size) - 1, SiLU), IdU),
                        rg = runif, scaling = 1E-2) {
  layer_size = c(input_layer_size, deep_layer_size, 1L) # input: time, volatility; output: stock weight
  strategy = initMLP(layer_size, activation = activation, rg = rg, scaling = scaling)
  return(strategy)
}

# Optimize portfolio
optimizePortfolio = function(W0, market, strategy, utility = terminal.utility,
                             learning_rate = 1E1, batch = 10L, nstep = 20L, silent = TRUE) {
  if (!silent) {
    cat("Portfolio optimization\n")
    cat("MLP training using ADAM (steps = ", sum(nstep), ") ...\n", sep = "")
  }
  
  # compute myopic strategy
  eta = attr(utility, "eta")
  
  if (!is.element(tolower(market@vol.type), c("heston", "garch", "3/2"))) {
    W0    = W0
    mu    = market@param$mu_S
    r     = market@param$mu_P
    sigma = sqrt(market@V0)
    
    myopic.strategy = function(x) (mu - r)/sigma^2/eta
  } else {
    mu = market@param$mu_S
    r  = market@param$mu_P
    myopic.strategy = function(x) (mu - r)/(x[2]*market@V0)/eta
  }

  # perform training
  n  = market@n
  dt = market@dt

  wgt = flatten(strategy)
  
  if (length(unique(length(learning_rate), length(batch), length(nstep))) != 1) {
    stop("learning_rate, batch and nstep must have the same length.")
  }
  
  sgd = adam()
  
  learning_rate_vec = rep(learning_rate, nstep)
  batch_vec = rep(batch, nstep)
  nstep = sum(nstep)
  
  obj.path  = rep(0.0, nstep)
  step.path = rep(0.0, nstep)
  grad.path = rep(0.0, nstep)
  
  tic = proc.time()
  
  for (step in 1:nstep) {
    learning_rate = learning_rate_vec[step]
    batch = batch_vec[step]
    
    sgd$epsilon = learning_rate
    
    if (!silent) {
      cat("step ", step, ": learning rate = ", learning_rate, ", batch = ", batch, ", ", sep = "")
    }
    
    for (b in 1:batch) {
      innovations(market) = NULL # resimulate innovations

      res = computeUtility(W0, market, strategy, utility, grad.too = TRUE)

      obj.path[step] = obj.path[step] + res$obj/batch

      grad.vec = if (b == 1)  res$grad/batch else grad.vec + res$grad/batch
      step.vec = if (b == 1) -sgd$step(res$grad)/batch else step.vec - sgd$step(res$grad)/batch
    }
    
    step.path[step] = sqrt(sum(step.vec*step.vec))
    grad.path[step] = sqrt(sum(grad.vec*grad.vec))

    wgt = wgt + step.vec
    weights(strategy) = wgt
    
    if (!silent) {
      cat("objective = ", obj.path[step], "\n", sep = "")
    }
  }
  
  toc = proc.time() - tic
  
  portfolio = new("Portfolio", W0 = W0, market = market, strategy = strategy, myopic.strategy = myopic.strategy, utility = utility,
                  training.time = as.numeric(toc[3]), obj = obj.path[nstep], obj.path = obj.path, step.path = step.path, grad.path = grad.path)
  
  return(portfolio)
}

evaluatePortfolio = function(portfolio, W0, nrep = 500L) {
  mean = 0.0
  sd   = 0.0
  
  market = portfolio@market
  
  for (rep in 1L:nrep) {
    innovations(market) = NULL

    util = computeUtility(W0, market, portfolio@strategy, utility = portfolio@utility)$obj
    
    mean = mean + util/nrep
    sd   = sd + util^2/(nrep - 1)
  }
  
  sd  = max(sd - nrep/(nrep - 1)*mean^2, 0.0)
  se = sd/sqrt(nrep)
  
  return(list(mean = mean, sd = sd, se = se))
}
