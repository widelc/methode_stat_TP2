library("copula")
source(here("function", "f_price_simul_univariate.R"))

f_price_simul_complete <- function(rets, price_init, d_simul, n_simul) {
  
  ### Simulates n_simul prices and volatility in d_simul days from a Gaussian
  ### copula and marginals for residuals modeled with GARCH(1,1) for the index
  ### and AR(1) for the volatility index
  
  #  INPUTS
  #   rets       : [matrix] (T x 2) of returns from past price ans volatility of the asset
  #   price_init : [vector] (2 x 1) of prices from which we start simulating (last price and last vol)
  #   d_simul    : [scalar] Number of days ahead we want to simulate prices
  #   n_simul    : [scalar] Number of simulated path we want
  
  #  OUTPUTS
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for index and vol)
  
  rets_index <- rets[,1]
  rets_vol   <- rets[,2]
  
  # Fit data to residuals marginals to Gaussian models 
  GARCH_index <- f_GARCH(rets_index)
  AR1_vol     <- f_AR1(rets_vol)
  
  theta_res_index <- f_opt(GARCH_index$res)
  theta_res_vol   <- f_opt(AR1_vol$res)
  
  # Fit Gaussian copula to data
  fit  <- f_fit_residuals_copula(rets)
  
  # Draw from the Gaussian copula (for returns)
  set.seed(1234)
  U_sim <- rCopula((n_simul * d_simul), fit@copula)
  
  res_index_simul <- qnorm(U_sim[,1], mean = theta_res_index[1], sd = theta_res_index[2])
  res_index_simul <- matrix(res_index_simul, nrow = n_simul, ncol = d_simul)
  
  res_vol_simul   <- qnorm(U_sim[,2], mean = theta_res_vol[1], sd = theta_res_vol[2])
  res_vol_simul   <- matrix(res_vol_simul, nrow = n_simul, ncol = d_simul)
  
  # Convert residuals from GARCH and AR(1) to returns
  rets_index_simul <- matrix(rep(NA, d_simul * n_simul), ncol = d_simul, nrow = n_simul)
  rets_index_prev  <- rep(last(as.numeric(rets_index)), n_simul)
  sig2_prev        <- rep(GARCH_index$last_sig^2, n_simul)
  
  rets_vol_simul   <- matrix(rep(NA, d_simul * n_simul), ncol = d_simul, nrow = n_simul)
  rets_vol_prev    <- rep(last(as.numeric(rets_vol)), n_simul)
  
  for (i in 1:d_simul) {
    
    # GARCH
    sig2 <- GARCH_index$theta[1] + GARCH_index$theta[2]*(rets_index_prev ^ 2) + GARCH_index$theta[3]*sig2_prev
    sig  <- sqrt(sig2)
    
    rets_index_simul[,i] <- sig * res_index_simul[,i]
    
    sig2_prev <- sig2
    rets_prev <- rets_index_simul[,i]
    
    # AR(1)
    rets_vol_simul[,i] <- AR1_vol$theta[1] + AR1_vol$theta[2] * rets_vol_prev + res_vol_simul[,i]
    
    rets_vol_prev <- rets_vol_simul[,i]
    
  }
  
  # Translation from returns to price in 5 days
  index_simul <- as.numeric(price_init[1]) * apply(1 + rets_index_simul, 1, cumprod)[d_simul,] 
  vol_simul   <- as.numeric(price_init[2]) * apply(1 + rets_vol_simul, 1, cumprod)[d_simul,] 
  price_simul <- cbind(index_simul, vol_simul)
  
  # Output
  price_simul

}


f_fit_residuals_copula <- function(rets) {
  
  ### Function which fits the data of the index and the volatility index to
  ### a Gaussian copula.
  
  #  INPUTS
  #   x  : [matrix] (T x 2) of observations
  
  #  OUTPUTS
  #   fit : Fitted copula
  
  rets_index <- rets[,1]
  rets_vol   <- rets[,2]
  
  # GARCH(1,1) and AR(1) models
  GARCH_index <- f_GARCH(rets_index)
  AR1_vol     <- f_AR1(rets_vol)
  
  # Fit residuals to Gaussian marginals
  theta_res_index <- f_opt(GARCH_index$res)
  theta_res_vol   <- f_opt(AR1_vol$res)
  
  U1 <- pnorm(GARCH_index$res, mean = theta_res_index[1], sd = theta_res_index[2])
  U2 <- pnorm(AR1_vol$res, mean = theta_res_vol[1], sd = theta_res_vol[2])
  U  <- cbind(U1, U2)
  
  # Fit copula
  C   <- normalCopula(dim = 2)
  fit <- fitCopula(C, data = as.matrix(U), method = "ml")
  
  # Output
  fit
  
}


f_AR1 <- function(y) {
  
  ### Compute the empirical residual from a AR1 model with Normal errors 
  ### relative to the data.
  
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  
  #  OUTPUTS
  #   out : [list] of  1) theta : GARCH model parameters
  #                    2) res   : (T x 1) of empirical residual
  
  y <- as.matrix(y)
  
  model <- arima(y, order=c(1,0,0))
  theta <- model$coef
  res   <- model$residuals
  
  # Output
  out <- list(theta = c(theta[2],theta[1]),
              res = as.vector(res))
  out
  
}

f_GARCH <- function(y) {
  
  ### Compute the empirical residual from a GARCH(1,1) model with Normal errors 
  ### relative to the data.
  
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  
  #  OUTPUTS
  #   out : [list] of  1) theta    : GARCH model parameters
  #                    2) res      : (T x 1) of empirical residual
  #                    3) last_sig : Conditionnal volatility at T
  
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  eps    <- 1e-6
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB     <- c(0, eps, eps)                        
  
  # Stationarity condition
  A      <- matrix(c(0, 1, 1), nrow = 1, ncol = 3)  
  b      <- 1                                       
  
  # Run the optimization (ui & ci define constraints)
  ui  <- rbind(-A, diag(3))  
  ci  <- c(-(b - eps) , LB)
  
  opt   <- constrOptim(theta  = theta0, 
                       f      = f_nll_GARCH, 
                       method = "Nelder-Mead",
                       y      = as.matrix(y), 
                       ui     = ui, 
                       ci     = ci)
  theta <- opt$par
  
  # Recompute the conditional variance (from 1 to T)
  sig2 <- f_ht_GARCH(theta, y)
  n    <- length(sig2)
  sig  <- sqrt(sig2[1:n-1])
  
  # Compute the empirical residual
  res <- y / sig
  as.matrix(res)
  
  # Output
  out <- list(theta = theta,
              res = as.vector(res),
              last_sig = sig[n-1])
  out
  
}

f_nll_GARCH <- function(theta, y) {
  
  ### Function which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- f_ht_GARCH(theta, y) 
  
  # Consider the T values
  sig2 <- sig2[1:T]
  
  # Compute the loglikelihood
  ll <- sum(dnorm(y, 0, sqrt(sig2), log = TRUE))   
  if (!is.finite(ll)) {
    ll <- -1e10
  }
  
  # Output the negative value
  nll <- -ll
  
  nll
}



f_ht_GARCH <- function(theta, y)  {
  
  ### Function which computes the vector of conditional variance
  
  #  INPUTS
  #   theta : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]
  a1 <- theta[2]
  b1 <- theta[3]
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # Compute conditional variance at each step
  for (i in 2:(T+1)) {
    sig2[i] = a0 + a1 * (y[i-1]^2) + b1 * sig2[i-1]
  }
  
  sig2
}
