## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

## Set market parameters
vol.type = "Heston" # choose volatility model from "Heston", "GARCH", "3/2" or "vanilla"

time.horizon = 1.0 # investment horizon in yrs
dt = time.horizon/252L/8.5 # time step
n  = as.integer(round(time.horizon/dt)) # number of grid points

mu_P = 0.05 # annual cash return

W0 = 1.0 # initial wealth

## ANN setup
deep_layer_size = c(5L)
scaling = 1E-1

## Load market data
USO = read.csv(file = "data/USO.csv")
OVX  = read.csv(file = "data/^OVX.csv")

## Calibrate market
date.range = seq(from = as.Date("2021-01-01"), to = as.Date("2023-12-31"), by = "day")

ntd = 252

Date = intersect(intersect(USO$Date, OVX$Date), as.character(date.range))
Date = as.Date(Date)

USO$Date = as.Date(USO$Date)
OVX$Date  = as.Date(OVX$Date)

I = which(is.element(USO$Date, Date))
USO = as.numeric(USO$Adj.Close[I])

I = which(is.element(OVX$Date, Date))
OVX = as.numeric(OVX$Adj.Close[I])

market = calibrateCKLSmarket(S = USO, V = (OVX/100.0)^2, dt.hist = 1/ntd, vol.type = vol.type)
market@n  = n
market@dt = dt
market@param$mu_P = mu_P

## Portfolio optimization

strategy0 = initStrategy(input_layer_size = 2L, deep_layer_size = deep_layer_size, rg = rnorm, scaling = scaling)

optimal.portfolio = optimizePortfolio(W0, market, strategy0, utility = my.utility,
                                     learning_rate = learning_rate, batch = batch, nstep = nstep, silent = FALSE)

plot(optimal.portfolio, which = 1)
plot(optimal.portfolio, which = 2)
plot(optimal.portfolio, which = 3)
plot(optimal.portfolio, which = 4)
plot(optimal.portfolio, which = 5)
