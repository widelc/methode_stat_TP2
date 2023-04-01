iv_surface_simul <- function(alpha, matrix) {
  
  ## Function that compute the IV for each Strike according to the volatily ajustment of each simulation
  ## using the parametric volatility surface
  
  alpha_1 <- alpha[1]
  alpha_2 <- alpha[2]
  alpha_3 <- alpha[3]
  alpha_4 <- alpha[4]
  
  maturity <- as.array(portfolio$tau - 5)/360
  moneyness <- matrix[,c(3,4,5,6)]
  iv <- matrix(NA, nrow = nrow(moneyness), ncol = ncol(moneyness))
  
  # Compute parametric volatility
  
  for (i in seq(length(maturity))) { 
  iv[,i] <- (alpha_1 + alpha_2 * (moneyness[,i]-1)^2 + alpha_3 * (moneyness[,i]-1)^3 + alpha_4 * sqrt(maturity[i]))
  
  for (i in seq(nrow(matrix))) {
  iv[i,] <- iv[i,] + matrix[i,2]
  }
  }
  iv <- cbind(matrix[,1],iv)
  colnames(iv) <- c("index_simul","1600", "1650", "1750", "1850")
  iv
}


f_price_simul_vol2 <- function(alpha, last_vol, price_simul) {
  
  ### Function that adjusts the values from a prediction of the VIX with
  ### with a parametric volatility surface
  
  #  INPUTS
  #   alpha       : [vector] (4 x 1) of the values of the parameters for the 
  #                 volatility surface
  #   last_vol    : The value of the VIX now (last known value of the VIX)
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for 
  #                 index and vol) from one of the previous methods
  
  #  OUTPUTS
  #   price_simul : [matrix] (n_simul x 6) of all the simulated prices, ajustment
  #                 required to the vol surface which consists in:
  #                 simulated VIX in 5 days - initial VIX + delta between vol_param & VIX
  #                 and the moneyness for all the strikes according to the simulation
  
  
  vol_ATM_param   <- alpha[1] + alpha[4]
  delta_vol_param <- as.numeric(last_vol - vol_ATM_param)
  
  price_simul[,2] <- price_simul[,2] - as.numeric(last_vol) + delta_vol_param
  moneyness_srike <- apply(X =as.array(portfolio$Strike),
                           MARGIN = 1,
                           FUN = function(x) {price_simul[,1] / x})
  
  price_simul <- cbind(price_simul,moneyness_srike)
  colnames(price_simul) <- c("index_simul 1", "vol_ajustment", "1600", "1650", "1750", "1850")
  price_simul
}

f_price_simul_vol <- function(alpha, last_vol, price_simul) {
  
  ### Function that adjusts the values from a prediction of the VIX with
  ### with a parametric volatility surface
  
  #  INPUTS
  #   alpha       : [vector] (4 x 1) of the values of the parameters for the 
  #                 volatility surface
  #   last_vol    : The value of the VIX now (last known value of the VIX)
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for 
  #                 index and vol) from one of the previous methods
  
  #  OUTPUTS
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for 
  #                 index and vol) with an adjustment made for the volatility
  
  
  vol_ATM_param   <- alpha[1] + alpha[4]
  delta_vol_param <- as.numeric(last_vol - vol_ATM_param)
  
  price_simul[,2] <- price_simul[,2] - delta_vol_param
  
  price_simul
}


f_opt_alpha <- function(option_info) {
  
  ### Function that finds the parameters (alpha) in order to minimize the
  ### objective function. Only OTM puts and calls are used for the 
  ### optimization.
  
  #  INPUTS
  #   option_info : [matrix] (T x 4) of the information about both puts
  #                 and calls. In order, the columns are the strike, the
  #                 time to maturity in years, the IV and the moneyness (K/S)
  
  #  OUTPUTS
  #   alpha : [vector] (4 x 1) of the values of the parameters
  
  # Optimize
  alpha0 <- rep(0.1, 4)
  alpha  <- optim(par = alpha0,
                  fn  = f_objective,
                  option_info = option_info)$par
  alpha
}


f_objective <- function(alpha, option_info) {
  
  ### Function that defines the objective function to minimize
  
  #  INPUTS
  #   alpha       : [vector] (4 x 1) of the values of the parameters
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
  #   alpha       : [vector] (4 x 1) of the values of the parameters
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


f_option_info <- function(call_info, put_info, last_price) {
  
  ### Function that collects the information about the puts and the calls
  ### which will be needed in the construction of the volatility surface
  
  #  INPUTS
  #   call_info  : [matrix] (T x 3) of the information about calls. In order, 
  #                the columns are the strike, the time to maturity in years 
  #                and the implied volatility (IV)
  #   put_info   : [matrix] (T x 3) of the information about calls. In order, 
  #                the columns are the strike, the time to maturity in years 
  #                and the implied volatility (IV)
  #   last_price : [scalar] Price for which the information is given
  
  #  OUTPUTS
  #   option_info : [matrix] (T x 4) of the information about both puts
  #                 and calls. In order, the columns are the strike, the
  #                 time to maturity in years, the IV and the moneyness (K/S)
  #  NOTE
  #   o Since the volatility surface will be calibrated with OTM puts and 
  #     calls, only those option will be included in the output option_info
  
  # Add the moneyness to the information
  last_price  <- as.numeric(last_price)
  m_call      <- call_info[,1] / as.numeric(last_price)
  m_put       <- put_info[,1] / as.numeric(last_price)
  
  call_info <- cbind(call_info, m_call)
  put_info  <- cbind(put_info, m_put)
  
  # Get OTM puts (m<1) and calls (m>1) 
  call_info <- call_info[call_info[,4]>=1,]
  put_info  <- put_info[put_info[,4]<=1,]
  
  # Combine OTM call information and put information
  option_info <- rbind(call_info, put_info)
  option_info
}