
f_opt_alpha <- function(option_info) {
  
  ### Function that finds the parameters in order to minimize the
  ### objective function 
  
  #  INPUTS
  #   option_info : [matrix] (T x 4) of the information about both puts
  #                 and calls. In order, the columns are the strike, the
  #                 time to maturity in years, the IV and the moneyness (K/S)
  
  #  OUTPUTS
  #   alpha : [vector] (5 x 1) of the values of the parameters
  
  alpha0 <- rep(1, 5)
  alpha  <- optim(par = alpha0,
                  fn  = f_objective,
                  option_info = option_info)$par
  alpha
}


f_objective <- function(alpha, option_info) {
  
  ### Function that defines the objective function to minimize
  
  #  INPUTS
  #   alpha       : [vector] (5 x 1) of the values of the parameters
  #   option_info : [matrix] (T x 4) of the information about both puts
  #                 and calls. In order, the columns are the strike, the
  #                 time to maturity in years, the IV and the moneyness (K/S)
  
  #  OUTPUTS
  #   obj : [scalar] Value of the objective function
  
  # Market volatility
  vol_mkt   <- option_info[,3] 
  
  # Parametric volatility
  vol_param <- f_vol_param(alpha, option_info)
  
  # Objective function
  obj <- sum(abs(vol_mkt - vol_param))
  if (!is.finite(obj)) {
    nll <- 1e10
  }
  
  obj
  
}


f_vol_param <- function(alpha, option_info) {
  
  ### Function that computes the parametric volatility
  
  #  INPUTS
  #   alpha       : [vector] (5 x 1) of the values of the parameters
  #   option_info : [matrix] (T x 4) of the information about both puts
  #                 and calls. In order, the columns are the strike, the
  #                 time to maturity in years, the IV and the moneyness (K/S)
  
  #  OUTPUTS
  #   vol_param : [vector] (T x 1) of the computed volatility
  
  alpha_1 <- alpha[1]
  alpha_2 <- alpha[2]
  alpha_3 <- alpha[3]
  alpha_4 <- alpha[4]
  
  tau <- option_info[,2]
  m   <- option_info[,4]
  
  # Compute parametric volatility
  vol_param <- alpha_1 + alpha_2 * (m-1)^2 + alpha_3 * (m-1)^3 + alpha_4 * sqrt(tau)
  vol_param
  
}