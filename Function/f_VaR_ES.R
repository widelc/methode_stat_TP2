
f_profits_and_loss <- function(init_price, simul_price) {
  
  ### Computes the P&L of a portfolio
  
  #  INPUTS
  #   init_price  : [scalar] Last known price of the portfolio
  #   simul_price : [vector] (n_simul x 1) of simulated future prices
  
  #  OUTPUTS
  #   PL : [vector] (n_simul x 1) of profits ans loss
  
  PL <- log(simul_price / init_price)
  PL
  
}

f_VaR <- function(init_price, simul_price, level) {
  
  ### Computes the VaR of a portfolio from simulated prices
  
  #  INPUTS
  #   init_price  : [scalar] Last known price of the portfolio
  #   simul_price : [vector] (n_simul x 1) of simulated future prices
  #   level       : [scalar] Level at which the VaR is estimated
  
  #  OUTPUTS
  #   VaR : [scalar] Value at Risk at the specified level
  
  # Compute the absolute return of the simulated prices
  abs_return <- simul_price - init_price
  
  #Compute VaR @ level
  n_simul <- length(simul_price)
  index   <- round((1-level) * n_simul)   
  VaR     <- sort(abs_return)[index]
  
  #Output
  VaR
  
}

f_ES <- function(init_price, simul_price, level) {
  
  ### Computes the ES of a portfolio from simulated prices
  
  #  INPUTS
  #   init_price  : [scalar] Last known price of the portfolio
  #   simul_price : [vector] (n_simul x 1) of simulated future prices
  #   level       : [scalar] Level at which the VaR is estimated
  
  #  OUTPUTS
  #   ES : [scalar] Expected shortfall at the specified level
  
  # Compute the absolute return of the simulated prices
  abs_return <- simul_price - init_price
  
  #Compute ES @ level
  n_simul <- length(simul_price)
  index   <- round((1-level) * n_simul)   
  ES      <- mean(sort(abs_return)[1:index])

  #Output
  ES
  
}

