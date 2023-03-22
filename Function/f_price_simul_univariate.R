
f_price_simul_univariate <- function(rets, price_init, d_simul, n_simul) {
  
  ### Simulates n_simul prices in d_simul days from a Gaussian univariate model
  
  #  INPUTS
  #   rets       : [vector] (T x 1) of returns from past price of the asset
  #   price_init : [scalar] Price from which we start simulating (last price)
  #   d_simul    : [scalar] Number of days ahead we want to simulate prices
  #   n_simul    : [scalar] Number of simulated path we want
  
  #  OUTPUTS
  #   price_simul : [vector] (n_simul x 1) of all the simulated prices
  
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  
  # Fit S&P500 returns with Gaussian model by MLE
  theta <- f_opt(rets)
  
  # Draw from the Gaussian model (for returns)
  set.seed(1234)
  rets_simul <- rnorm(d_simul * n_simul, mean = theta[1], sd = theta[2])
  rets_simul <- matrix(rets_simul, nrow = n_simul, ncol = d_simul)
  
  # Translation from returns to price in 5 days
  price_simul <- as.numeric(price_init) * apply(1 + rets_simul, 1, cumprod)[d_simul,] 
  
  # Output
  price_simul
  
}


f_opt <- function(x) {
  
  ### Function which fits the data (x) to a Gaussian model by MLE
  
  #  INPUTS
  #   x : [vector] (T x 1) of observations
  
  #  OUTPUTS
  #   $par : [vector] (2 x 1) of estimated parameters
  
  theta0 <- c(mean(x), sd(x))
  optim(par = theta0,
        fn = f_nll_n,
        method = "L-BFGS-B",
        lower = c(-Inf, 1e-5),
        x = x)$par
}


f_nll_n <- function(theta, x) {
  
  ### Function which computes the negative log likelihood value 
  ### of a Gaussian model
  
  #  INPUTS
  #   theta : [vector] (2 x 1) of parameters
  #   x     : [vector] (T x 1) of observations
  
  #  OUTPUTS
  #   nll : [scalar] negative log likelihood value
  
  mu  <- theta[1]
  sig <- theta[2]
  nll <- -sum(dnorm(x, mu, sig, log = TRUE))

  if (!is.finite(nll)) {
    nll <- 1e10
  }
  
  nll
}