
f_VaR_ES_PL <- function(init_price, simul_price, level) {
  
  ### Computes the VaR, expected shortfall and P&L of a portfolio at a given
  ### level for simulated prices.
  
  #  INPUTS
  #   init_price  : [scalar] Last known price of the portfolio
  #   simul_price : [vector] (n_simul x 1) of simulated future prices
  #   level       : [scalar] Level at which the VaR ans ES are estimated
  
  #  OUTPUTS
  #   out : [list] containing the VaR, the ES and the P&L
  
  
  # Calculate risk mesure and P&L
  VaR <- f_VaR(init_price, simul_price, level)
  ES  <- f_ES(init_price, simul_price, level)
  PL  <- f_profits_and_loss(init_price, simul_price)
  
  # Output
  out <- list(VaR = VaR, ES = ES, PL = PL)
  
}

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
  VaR <-as.numeric(quantile(abs_return, probs = 1-level))
  
  #Output
  VaR
  
}

f_ES <- function(init_price, simul_price, level) {
  
  ### Computes the ES of a portfolio from simulated prices
  
  #  INPUTS
  #   init_price  : [scalar] Last known price of the portfolio
  #   simul_price : [vector] (n_simul x 1) of simulated future prices
  #   level       : [scalar] Level at which the ES is estimated
  
  #  OUTPUTS
  #   ES : [scalar] Expected shortfall at the specified level
  
  # Compute the absolute return of the simulated prices
  abs_return <- simul_price - init_price
  
  #Compute ES @ level
  VaR <- f_VaR(init_price, simul_price, level)
  ES  <- mean(abs_return[abs_return <= VaR])
  
  #Output
  ES
  
}

