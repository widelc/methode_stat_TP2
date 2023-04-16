# Import libraries
library("MASS")
library("fGarch")
library("copula")

f_price_simul_copula <- function(rets, nu, price_init, d_simul, n_simul) {
  
  ### Simulates n_simul prices and volatility in d_simul days 
  ### from a Gaussian copula and Student marginals
  
  #  INPUTS
  #   rets       : [matrix] (T x 2) of returns from past price and volatility of the asset
  #   nu : [vector] (2 x 1) student parameter for both indexes marginals
  #   price_init : [vector] (2 x 1) of prices from which we start simulating (last price and last vol)
  #   d_simul    : [scalar] Number of days ahead we want to simulate prices
  #   n_simul    : [scalar] Number of simulated paths we want
  
  #  OUTPUTS
  #   price_simul : [matrix] (n_simul x 2) of all the simulated prices (for index and vol)
  
  #  NOTE
  #   o the estimations are done by maximum likelihood
  
  # Fit data to student models (marginals)
  theta       <- f_fit_marginals_t(rets, nu)
  theta_index <- theta$index
  theta_vol   <- theta$vol
  
  # Fit Gaussian copula to data
  fit  <- f_fit_copula(rets, nu)
  
  # Draw from the Gaussian copula (for returns)
  set.seed(1234)
  U_sim <- rCopula((n_simul * d_simul), fit@copula)
  
  rets_index_simul <- qstd(U_sim[,1], mean = theta_index[1], sd = theta_index[2], nu = nu[1])
  rets_index_simul <- matrix(rets_index_simul, nrow = n_simul, ncol = d_simul)
  
  rets_vol_simul   <- qstd(U_sim[,2], mean = theta_vol[1], sd = theta_vol[2], nu = nu[2])
  rets_vol_simul   <- matrix(rets_vol_simul, nrow = n_simul, ncol = d_simul)
  
  # Translation from returns to price in 5 days
  index_simul <- as.numeric(price_init[1]) * apply(1 + rets_index_simul, 1, cumprod)[d_simul,] 
  vol_simul   <- as.numeric(price_init[2]) * apply(1 + rets_vol_simul, 1, cumprod)[d_simul,] 
  
  # Output
  price_simul <- cbind(index_simul, vol_simul)
  price_simul
  
}

f_fit_copula <- function(x, nu) {
  
  ### Function which fits the data of the index and the volatility index to
  ### a Gaussian copula.
  
  #  INPUTS
  #   x  : [matrix] (T x 2) of observations
  #   nu : [vector] (2 x 1) student parameter for both indexes marginals
  
  #  OUTPUTS
  #   fit : Fitted copula
  
  # Fit data to student models
  theta       <- f_fit_marginals_t(x, nu)
  theta_index <- theta$index
  theta_vol   <- theta$vol
  
  # Transform data to hyper-cube
  U1 <- pstd(x[,1], mean = theta_index[1], sd = theta_index[2], nu = nu[1])
  U2 <- pstd(x[,2], mean = theta_vol[1], sd = theta_vol[2], nu = nu[2])
  U  <- cbind(U1, U2)
  
  # Fit copula
  C   <- normalCopula(dim = 2)
  fit <- fitCopula(C, data = U, method = "ml")
  
  fit
}

f_fit_marginals_t <- function(x, nu) {
  
  ### Function which fits the data of the index and the volatility index to
  ### 2 student models by MLE.
  
  #  INPUTS
  #   x  : [matrix] (T x 2) of observations
  #   nu : [vector] (2 x 1) student parameter for both indexes marginals
  
  #  OUTPUTS
  #   theta : [list] containing the mean and standard deviation estimated
  #           for both indexes.
  
  x <- as.matrix(x)
  
  # Fitting student model to the price index observations
  fit_index <- suppressWarnings(fitdistr(x = x[,1], 
                                         densfun = dstd, 
                                         start = list(mean = 0, sd = 1),
                                         nu = nu[1]))
  theta_index <- fit_index$estimate
  
  # Fitting student model to the volatility index observations
  fit_vol <- suppressWarnings(fitdistr(x = x[,2], 
                                       densfun = dstd, 
                                       start = list(mean = 0, sd = 1),
                                       nu = nu[2]))
  
  theta_vol   <- fit_vol$estimate
  
  # Output
  theta <- list(index = theta_index,
                vol = theta_vol)
  theta
}