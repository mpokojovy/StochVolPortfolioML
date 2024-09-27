## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

setClass("Market", 
  slots = c(
    n  = "integer",
    dt = "numeric",
    dt.hist = "numeric",
    param = "list",
    vol.type = "character",
    #
    S.historic = "numeric",
    V.historic = "numeric",
    S0 = "numeric",
    V0 = "numeric",
    .innovations = "matrix"
  ), 
  prototype = list(
    n  = NA_integer_,
    dt = NA_real_,
    dt.hist = NA_real_,
    param = list(),
    vol.type = "character",
    #
    S.historic = numeric(0),
    V.historic = numeric(0),
    S0 = NA_real_,
    V0 = NA_real_,
    .innovations = matrix(NA_real_)
  )           
)

setGeneric("horizon", function(x) standardGeneric("horizon"))
setGeneric("mu_V",    function(x) standardGeneric("mu_V"))
setGeneric("sigma_V", function(x) standardGeneric("sigma_V"))

setGeneric("innovations<-", function(x, value) standardGeneric("innovations<-"))

setMethod("horizon", "Market", function(x) {
  return(x@n*x@dt)
})

setMethod("mu_V", "Market", function(x) {
  vol.type = tolower(x@vol.type)
  
  if (vol.type == "3/2") {
    # 3/2 model
    mu_V = function(S, V) x@param$kappa*V*(x@param$V_ast - V)
  } else if (vol.type == "heston") {
    # Heston
    mu_V = function(S, V) x@param$kappa*(x@param$V_ast - V)
  } else if (vol.type == "garch") {
    # GARCH
    mu_V = function(S, V) x@param$kappa*(x@param$V_ast - V)
  } else if (vol.type == "kraft") {
    # Kraft
    mu_V = function(S, V) x@param$kappa*(x@param$V_ast - V)
  } else {
    # vanilla
    mu_V = function(S, V) 0.0
  }
  
  return(mu_V)
})

setMethod("sigma_V", "Market", function(x) {
  vol.type = tolower(x@vol.type)
  
  if (vol.type == "3/2") {
    # 3/2 model
    sigma_V = function(S, V) x@param$xi*(V)^(1.5)
  } else if (vol.type == "heston") {
    # Heston
    sigma_V = function(S, V) x@param$xi*sqrt(V)
  } else if (vol.type == "garch") {
    # GARCH
    sigma_V = function(S, V) x@param$xi*V
  } else if (vol.type == "kraft") {
    # Kraft
    sigma_V = function(S, V) x@param$xi*sqrt(V)
  } else {
    # vanilla
    sigma_V = function(S, V) 0.0
  }
   
  return(sigma_V)
})

setMethod("innovations<-", "Market", function(x, value) {
  if (is.null(value)) {
    rho = x@param$rho
    root.cor = expm::sqrtm(matrix(c(1.0, rho, rho, 1.0), nrow = 2L, ncol = 2L))
    x@.innovations = matrix(rnorm(2*x@n), ncol = 2L) %*% t(root.cor*sqrt(x@dt))
  } else {
    if (!is.matrix(value)) {
      warning("value must be a matrix")
    } else {
      if ((ncol(value) != 2) || (ncol(value) != x@n)) {
        warning(paste("value must be a 2 x", x@n, "matrix"))
      }
    }
    
    x@.innovations = value
  }
  return(x)
})

# Calibrate Chan-Karolyi-Longstaff-Sanders (CKLS) market
calibrateCKLSmarket = function(S, V = NULL, dt.hist, vol.type = "Heston") {
  n = length(S)
  I = 1:(n - 1)
  
  vol.type = tolower(vol.type)
  
  if (is.null(V) && (vol.type != "vanilla")) {
    stop("V must be suplemented unless vol.type == vanilla")
  }
  
  vanilla.flag = !is.element(vol.type, c("3/2", "heston", "garch"))
  
  if (!vanilla.flag) {
    # Calibrate CKLS for stochastic volatility
    if (vol.type == "3/2") {
      vol.type = "3/2"
      df  = data.frame(y  =  diff(V)/(V[I]^(1.5)),
                       x1 =  dt.hist/(V[I]^(1.5)),
                       x2 = -dt.hist/sqrt(V[I]))
    } else if (is.element(vol.type, c("heston", "kraft"))) {
      if (vol.type == "heston") {
        vol.type = "Heston"
      } else {
        vol.type = "Kraft"
      }
      
      df  = data.frame(y  =  diff(V)/sqrt(V[I]),
                       x1 =  dt.hist/sqrt(V[I]),
                       x2 = -dt.hist*sqrt(V[I]))
    } else if (vol.type == "garch") {
      vol.type = "GARCH"
      df  = data.frame(y  =  diff(V)/V[I],
                       x1 =  dt.hist/V[I],
                       x2 = -dt.hist)
    }
    
    fit = lm(y ~ . - 1, data = df)
    
    theta = as.numeric(fit$coef)
    
    kappa = theta[2]
    V_ast = theta[1]/kappa
    xi    = as.numeric(summary(fit)$sigma/sqrt(dt.hist))
    eps_V = as.numeric(fit$residuals/(xi*sqrt(dt.hist)))
  }
  
  # Calibrate GBM for stock prices
  if (!vanilla.flag) {
    if (vol.type == "Kraft") {
      df  = data.frame(y = diff(S)/S[I])
      fit = lm(y ~ V[I], data = df, weights = 1.0/V[I])
    } else {
      df  = data.frame(y = diff(S)/S[I])
      fit = lm(y ~ 1, data = df, weights = 1.0/V[I])
    }
    
    mu_S  = as.numeric(fit$coef[1]/dt.hist)
    eps_S = as.numeric(fit$residuals/sqrt(V[I]*dt.hist))
    
    rho = cor(eps_S, eps_V)
  } else {
    mu_S = as.numeric(mean(diff(log(S)))/dt.hist)
    rho  = 0.0
  }
  
  # Initial values (can be later modified by user)
  S0 = S[n]
  
  if (!vanilla.flag) {
    V0 = V[n]
  } else {
    V0 = as.numeric(var(diff(log(S)))/dt.hist)
  }
  
  # Create and initialize market variable
  market = new("Market", dt.hist = dt.hist, vol.type = vol.type,
               S.historic = S, V.historic = V, S0 = S0, V0 = V0)
  
  if (!vanilla.flag) {
    market@param = list(mu_S = mu_S, V_ast = V_ast, kappa = kappa, xi = xi, rho = rho)
  } else {
    market@param = list(mu_S = mu_S, rho = rho)
  }
  
  return(market)
}

# Propagate market evolution along innovations under the Heston model
marketForwardMap = function(market) {
  innovations = market@.innovations
  param = market@param

  vol.type = tolower(market@vol.type)
  
  vanilla.flag = !is.element(vol.type, c("3/2", "heston", "garch", "kraft"))
    
  n  = nrow(innovations)
  dt = market@dt

  r       = market@param$mu_P
  mu_S    = market@param$mu_S
  mu_V    = mu_V(market)
  sigma_V = sigma_V(market)
  
  x = matrix(0.0, nrow = n, ncol = 3L)
  
  .P = 1.0
  .S = market@S0
  .V = market@V0

  for (i in 1L:n) {
    P = (1 + r*dt)*.P
    
    if (vol.type != "kraft") {
      S = max(1E-3*market@S0, .S + mu_S*.S*dt + sqrt(.V)*.S*innovations[i, 1]) 
    } else {
      S = max(1E-3*market@S0, .S + mu_S*.V*.S*dt + sqrt(.V)*.S*innovations[i, 1])
    }
    
    if (vanilla.flag) {
      V = market@V0 
    } else {
      V = max(0.05*market@param$V_ast, .V + mu_V(.S, .V)*dt + sigma_V(.S, .V)*innovations[i, 2])
    }
    
    x[i, ] = c(P, S, V)
    
    .P = P
    .S = S
    .V = V
  }

  return(x)
}

# Plot a market
setMethod("plot", "Market", function(x, type = "both", n.rep = 5L, ...) {
  type = tolower(type)
  
  ylim.S = c(Inf, -Inf)
  ylim.V = c(Inf, -Inf)
  
  vol.type = tolower(x@vol.type)
  
  # prepare title
  if (vol.type == "3/2") {
    # 3/2 model
   caption = "GBM model with 3/2 stochastic volatility"
  } else if (vol.type == "heston") {
    # Heston
    caption = "Heston's model"
  } else if (vol.type == "garch") {
    # GARCH
    caption = "GBM model with GARCH(1, 1) volatility"
  } else if (vol.type == "kraft") {
    # Heston
    caption = "Kraft's model"
  } else {
    # vanilla
    caption = "Vanilla GBM model"
  }
  
  # prepare legend
  legend.text = character()
  legend.lty  = integer()
  legend.col  = character()
  
  if (is.element(type, c("historic", "both"))) {
    legend.text = c(legend.text, "Historic")
    legend.lty  = c(legend.lty, 1L)
    legend.col  = c(legend.col, "black")
  }
  
  if (is.element(type, c("future", "both"))) {
    legend.text = c(legend.text, "Simulated")
    legend.lty  = c(legend.lty, 2L)
    legend.col  = c(legend.col, "black")
  }
  
  
  if (is.element(type, c("historic", "both"))) {
    if ((length(x@S.historic) < 1) || (length(x@V.historic) < 1)) {
      stop("S.historic and V.historic must be nonempty")
    } else if ((length(x@S.historic) < 1) != (length(x@V.historic) < 1)) {
      stop("S.historic and V.historic must have equal lengths")
    }
    
    n.historic = length(x@S.historic)
    t.historic = -rev(1:n.historic)*x@dt.hist
    
    ylim.S = c(min(x@S.historic), max(x@S.historic))
    ylim.V = c(min(x@V.historic), max(x@V.historic))
  }
  
  if (is.element(type, c("future", "both"))) {
    n.future = x@n
    t.future = (0:(n.future - 1))*x@dt
    
    if (is.na(n.future)) {
      stop("n must be non-NA")
    } else if (n.future < 0) {
      stop("n must be positive")
    }
    
    V.future = matrix(0.0, nrow = n.future, ncol = n.rep)
    S.future = matrix(0.0, nrow = n.future, ncol = n.rep)
    
    .innovations = x@.innovations
    
    for (rep in 1:n.rep) {
      innovations(x) = NULL # this resets the innovations randomly
      SDE = marketForwardMap(x)
      S.future[, rep] = SDE[, 2]
      V.future[, rep] = SDE[, 3]
    }
    
    x@.innovations = .innovations
    
    ylim.S = c(min(S.future, ylim.S[1]), max(S.future, ylim.S[2]))
    ylim.V = c(min(V.future, ylim.V[1]), max(V.future, ylim.V[2]))
  }
  
  ylim.S = c(mean(ylim.S) - 0.65*diff(ylim.S), mean(ylim.S) + 0.85*diff(ylim.S))
  ylim.V = c(max(0.0, mean(ylim.V) - 0.60*diff(ylim.V)), mean(ylim.V) + 1.20*diff(ylim.V))
  
  ylim.V = 100.0*sqrt(ylim.V)
  
  plot.new()
  mtext(caption, side = 3, line = -3, cex = 1.5, outer = TRUE)
  
  par(mfrow = c(1, 2))
  par(mar = c(par("mar")[1], par("mar")[2] + 0.1, par("mar")[3], par("mar")[4]))
  
  if (type == "historic") {
    xlim = c(min(t.historic), max(t.historic))
  } else if (type == "future") {
    xlim = c(min(t.future), max(t.future))
  } else {
    xlim = c(min(c(t.historic, t.future)), max(c(t.historic, t.future)))
  }
  
  par(mfg = c(1, 1))
  plot(NULL, type = "n",
       xlab = "Time (in years)", ylab = bquote("Asset Price"~S[t]~"($)"),
       xlim = xlim, ylim = ylim.S)
  legend("topleft", legend.text, col =legend.col, lty = legend.lty)
  
  if (is.element(type, c("historic", "both"))) {
    lines(t.historic, x@S.historic, type = "l", col = "black")
  }
  
  for (rep in 1:n.rep) {
    lines(t.future, S.future[, rep], col = "black", lty = 2)
  }
  
  par(mfg = c(1, 2))
  plot(NULL, type = "n",
       xlab = "Time (in years)", ylab = bquote("Annualized Volatility 100%"~sqrt(Y[t])~"(%)"),
       xlim = xlim, ylim = ylim.V)
  legend("topleft", legend.text, col =legend.col, lty = legend.lty)
  
  if (is.element(type, c("historic", "both"))) {
    lines(t.historic, 100.0*sqrt(x@V.historic), type = "l", col = "black")
  }
  
  if (is.element(type, c("future", "both"))) {
    for (rep in 1:n.rep) {
      lines(t.future, 100.0*sqrt(V.future[, rep]), col = "black", lty = 2)
    }
  }
})
