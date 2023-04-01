
f_eval_portfolio <- function(last, portfolio, int_rate, d_simul = 0) {
  
  ### Computes the value of an option portfolio
  
  #  INPUTS
  #   last      : [vector] (2 x 1) of the last price of the index and the last known volatility
  #   portfolio : [list] of information about the option portfolio (see NOTE)
  #   int_rate  : [vector] of the term structure of interest rates (from Market database)
  #   d_simul   : [scalar] Number of days ahead we want to simulate prices
  
  #  OUTPUTS
  #   port_value : [scalar] Value of the option portfolio
  
  #  NOTE
  #   o The input 'portfolio' is as list that contains :
  #       Qty     : [vector] (T x 1) Quantity of each options in the portfolio
  #       Strike  : [vector] (T x 1) Strike of each options in the portfolio
  #       tau     : [vector] (T x 1) Time to maturity of each options in the portfolio
  #       is_call : [vector] (T x 1) Boolean (TRUE for calls, FALSE for puts) 
          

  
  # Interest rate for each options (linear interpolation)
  tau_rate <- as.numeric(names(int_rate))
  tau_port <- portfolio$tau / 360
  r        <- approx(tau_rate, int_rate, xout = tau_port)$y
  
  # Implied volatility for each option
  n_last <- length(last)
  if (n_last == 2) {
    # Case where there is only 1 vol for every option
    IV <- as.numeric(last[2])
  } else {
    # Case where there is a vol for each option
    IV <- last[2:n_last]
  }
  
  # Option prices with Black and Scholes
  S       <- as.numeric(last[1])
  K       <- portfolio$Strike
  #IV      <- as.numeric(last[2])
  is_call <- portfolio$is_call
  tau_opt <- (portfolio$tau - d_simul) / 250
  
  option_prices <- f_BlackScholes(S, K, r, IV, tau_opt, is_call)
  
  # Output
  port_value <- sum(option_prices * portfolio$Qty)
  port_value
  
}


f_BlackScholes <- function(S, K, r, IV, tau, is_call) {
  
  ### Compute the prices of a set of European options (vanilla calls or puts)
  
  #  INPUTS
  #   S       : [scalar] The last observed price (from S&P500)
  #   K       : [vector] (T x 1) of the strike prices
  #   r       : [vector] (T x 1) of the interest rates 
  #   IV      : [scalar] The last observed implied volatility (from VIX)
  #   tau     : [vector] (T x 1) of the time to maturities
  #   is_call : [vector] (T x 1) of boolean (TRUE for calls, FALSE for puts) 
  
  #  OUTPUTS
  #   option_price   : [vector] (T x 1) of option prices
  
  #  NOTE
  #   o we use the Black-Scholes formula for calculations
  
  d1 <- (log(S / K) + (r + 0.5 * IV^2) * tau) / (IV * sqrt(tau))
  d2 <- d1 - IV * sqrt(tau)
  
  N_d1 <- pnorm(d1)
  N_d2 <- pnorm(d2)
  
  # Option evaluation (depending on if it's a put or a call)
  discount_f <- exp(-r[is_call] * tau[is_call])
  opt_price  <- S * N_d1[is_call] - K[is_call] * discount_f * N_d2[is_call]
  
  # Output
  opt_price
  
}