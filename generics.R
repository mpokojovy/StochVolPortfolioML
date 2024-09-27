## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

if (!require("neuralnet")) {
  install.packages("neuralnet")
}

setGeneric("flatten",   function(x) standardGeneric("flatten"))
setGeneric("weights",   function(x) standardGeneric("weights"))
setGeneric("weights<-", function(x, value) standardGeneric("weights<-")) # used to assign or unflatten

setGeneric("horizon", function(x) standardGeneric("horizon"))
setGeneric("mu_V",    function(x) standardGeneric("mu_V"))
setGeneric("sigma_V", function(x) standardGeneric("sigma_V"))
setGeneric("horizon", function(x) standardGeneric("horizon"))

setGeneric("innovations<-", function(x, value) standardGeneric("innovations<-"))
