# Import libraries
library("mvtnorm")

f_price_simul_bivariate <- function(rets, price_init, d_simul, n_simul) {
  
  ### Simulates n_simul prices and volatility in d_simul days 
  ### from a bivariate Gaussian model
  
  #  INPUTS
  #   rets       : [matrix] (T x 2) of returns from past price and volatility of the asset
  #   price_init : [vector] (2 x 1) of prices from which we start simulating (last price and last vol)
  #   d_simul    : [scalar] Number of days ahead we want to simulate prices
  #   n_simul    : [scalar] Number of simulated paths we want
  
  #  OUTPUTS
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for index and vol)
  
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  
  # Fit S&P500 returns with Gaussian model by MLE
  theta <- f_opt_bi(rets)
  mu    <- theta[1:2]
  sig   <- matrix(c(theta[3], theta[5], theta[5], theta[4]), 2, 2)
  
  # Draw from the Gaussian model (for returns)
  set.seed(1234)
  rets_simul <- rmvnorm(n = n_simul*d_simul, mean = mu, sigma = sig)
  
  rets_SP500_simul <- rets_simul[,1]
  rets_SP500_simul <- matrix(rets_SP500_simul, nrow = n_simul, ncol = d_simul)
  
  rets_VIX_simul   <- rets_simul[,2]
  rets_VIX_simul   <- matrix(rets_VIX_simul, nrow = n_simul, ncol = d_simul)
  
  # Translation from returns to price in 5 days
  SP500_simul <- as.numeric(price_init[1]) * apply(1 + rets_SP500_simul, 1, cumprod)[d_simul,] 
  VIX_simul   <- as.numeric(price_init[2]) * apply(1 + rets_VIX_simul, 1, cumprod)[d_simul,] 
  
  # Output
  price_simul <- cbind(SP500_simul, VIX_simul)
  
}


f_opt_bi <- function(x) {
  
  ### Function which fits the data (x) to a bivariate Gaussian model by MLE
  
  #  INPUTS
  #   x : [matrix] (T x 2) of observations
  
  #  OUTPUTS
  #   theta_mle : [vector] (5 x 1) of estimated parameters
  
  # Convert data to matrix
  x <- as.matrix(x)

  # Initial guess
  mu  <- as.vector(apply(x, 2, mean))
  sig <- as.vector(cov(x))
  theta_0 <- c(mu, sig[1], sig[4], sig[3])
  
  # Optimize log-likelihood function
  opt <- optim(par    = theta_0, 
               fn     = f_nll_n_bi,
               x      = x,
               method = "L-BFGS-B",
               lower  = c(-Inf, -Inf, 1e-5, 1e-5, -Inf))
  
  # Output
  theta_mle <- opt$par
  theta_mle
  
}
 
 
f_nll_n_bi <- function(theta, x) {
  
  ### Function which computes the negative log likelihood value 
  ### of a bivariate Gaussian model
  
  #  INPUTS
  #   theta : [vector] (5 x 1) of parameters
  #   x     : [matrix] (T x 2) of observations
  
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  mu    <- theta[1:2]
  sig11 <- theta[3]
  sig22 <- theta[4]
  sig12 <- theta[5]
  Sigma <- matrix(c(sig11, sig12, sig12, sig22), 2, 2)
  
  nll <- -sum(dmvnorm(x, mean = mu, sigma = Sigma, log = TRUE))
  
  if (!is.finite(nll)) {
    nll <- 1e10
  }
  
  nll
  
}