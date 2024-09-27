## Portfolio Optimization with Feedback Strategies Based on Artificial Neural Networks
## Michael Pokojovy and Yaacov Kopeliovich (C) 2024
## Version 1.0

## Set market parameters
vol.type = "vanilla" # choose volatility model from "Heston", "GARCH", "3/2" or "vanilla"

time.horizon = 1.0 # investment horizon in yrs
dt = time.horizon/252L/8.5 # time step
n  = as.integer(round(time.horizon/dt)) # number of grid points

mu_P = 0.05 # annual cash return

W0 = 1.0 # initial wealth

## ANN setup
deep_layer_size = c(3L)
scaling = 1E-1

## Load market data
GSPC = read.csv(file = "data/^GSPC.csv")
VIX  = read.csv(file = "data/^VIX.csv")

## Calibrate market
date.range = seq(from = as.Date("2021-01-01"), to = as.Date("2023-12-31"), by = "day")

ntd = 252

Date = intersect(intersect(GSPC$Date, VIX$Date), as.character(date.range))
Date = as.Date(Date)

GSPC$Date = as.Date(GSPC$Date)
VIX$Date  = as.Date(VIX$Date)

I = which(is.element(GSPC$Date, Date))
SP500 = as.numeric(GSPC$Adj.Close[I])

I = which(is.element(VIX$Date, Date))
VIX = as.numeric(VIX$Adj.Close[I])

market = calibrateCKLSmarket(S = SP500, V = (VIX/100.0)^2, dt.hist = 1/ntd, vol.type = vol.type)
market@n  = n
market@dt = dt
market@param$mu_P = mu_P

## Plot market

innovations(market) = NULL
plot(market, type = "both")

## Portfolio optimization

strategy0 = initStrategy(input_layer_size = 1L, deep_layer_size = deep_layer_size, rg = rnorm, scaling = scaling)

optimal.portfolio = optimizePortfolio(W0, market, strategy0, utility = my.utility,
                                      learning_rate = learning_rate, batch = batch, nstep = nstep, silent = FALSE)

plot(optimal.portfolio, which = 1)
plot(optimal.portfolio, which = 2)
plot(optimal.portfolio, which = 3)
plot(optimal.portfolio, which = 4)
plot(optimal.portfolio, which = 5)
