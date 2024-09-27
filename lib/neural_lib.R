## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

## activation functions
sigmoid = function(x) {
  return(1.0/(1.0 + exp(-x)))
}

SiLU = function(x) {
  return(x/(1.0 + exp(-x)))
}

IdU = function(x) {
  return(x)
}

## MLP class
setClass("MLP", 
  slots = c(
    layer_size = "integer",
    .weights = "list",
    activation = "list",
    shift = "function"),
  prototype = c(
    layer_size = 0L,
    .weights = list(),
    activation = list(),
    shift = function(x) 0.0)
)

setGeneric("flatten",   function(x) standardGeneric("flatten"))
setGeneric("weights",   function(x) standardGeneric("weights"))
setGeneric("weights<-", function(x, value) standardGeneric("weights<-")) # used to assign or unflatten

# get weights
setMethod("as.function", "MLP", function(x) {
  return(function(arg) forwardPropagation(arg, x))
})

# get weights
setMethod("weights", "MLP", function(x) {
  return(x@.weights)
})

# set weights or unflatten
setMethod("weights<-", "MLP", function(x, value) {
  if (!is.numeric(value) && !is.list(value)) {
    stop("value mubst be a list or vector of weights")
  }
  
  if (is.list(x)) {
    x@.weights = value
  } else {
    depth = length(x@layer_size) - 1L
    
    weights = list(W = list(), b = list())
    
    last = 0L
    
    for (i in 1:depth) {
      nin  = x@layer_size[i]
      nout = x@layer_size[i + 1L]
      
      first = last + 1L
      last  = last + nin*nout + nout
      
      weights$b[[i]] = matrix(value[first:(first + nout - 1L)], nrow = nout)
      weights$W[[i]] = matrix(value[(first + nout):last],       nrow = nout, ncol = nin)
    }
    
    x@.weights = weights
  }
  
  return(x)
})

# flatten MLP
setMethod("flatten", "MLP", function(x) {
  depth = length(x@layer_size) - 1L
  
  lst = sapply(1L:depth, function(i) c(as.vector(x@.weights$b[[i]]), as.vector(x@.weights$W[[i]])))
  
  return(unlist(lst))
})

# random initialization of MLP
initMLP = function(layer_size, activation = c(replicate(length(layer_size) - 1, SiLU), IdU),
                   rg = runif, scaling = 1E-2) {
  depth = length(layer_size) - 1L

  mlp = new("MLP", layer_size = layer_size, .weights = list(W = list(), b = list()),
            activation = activation)

  for (i in 1L:depth) {
    nin  = layer_size[i]
    nout = layer_size[i + 1L]

    mlp@.weights$b[[i]] = matrix(rg(nout),     nrow = nout, byrow = TRUE)*scaling
    mlp@.weights$W[[i]] = matrix(rg(nin*nout), nrow = nout, ncol = nin, byrow = TRUE)*scaling
  }

  return(mlp)
}

# MLP forward propagation
forwardPropagation = function(x, mlp) {
  depth = length(mlp@layer_size) - 1L
  
  shift = if (is.null(mlp@shift)) 0.0 else shift = mlp@shift(x)
  
  x = matrix(x, ncol = 1L)

  for (i in 1L:depth) {
    x = mlp@activation[[i]](mlp@.weights$W[[i]] %*% x + mlp@.weights$b[[i]])
  }

  return(shift + x)
}

gradMLP = function(mlp, obj, eps = 1E-6) {
  mlp0 = mlp
  w0   = flatten(mlp0)
  obj0 = obj(mlp0)
  
  grad = rep(0.0, length(w0))
  
  for (i in 1:length(w0)) {
    w = w0
    w[i] = w[i] + eps
    
    mlp = mlp0
    weights(mlp) = w
    
    grad[i] = (obj(mlp) - obj0)/eps
  }
  
  return(grad)
}
