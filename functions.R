#permutation function wring 
perm = function(n, r) {
  factorial(n) / factorial(n-r)
}

#combination function
comb = function(n, r) {
  factorial(n) / factorial(n-r) / factorial(r)
}

#---------------- BINOMIAL DISTRIBUTION -------------------------
#single value function for binomial function
adeckbinom <- function(k, n, p) {
  binom <- comb(n, k) * (p^k * ((1-p)^(n-k)))
  binom
}

#apply the binomial function to an array or vector of values
binomdistro <- function(k, n, p) {
  prob <- sapply(k, function(k_val) adeckbinom(k_val, n, p))
  cum_prob <- cumsum(prob)
  bindistro <- cbind(k,prob,cum_prob)
  data.frame(bindistro)
}

#---------------- GEOMETRIC DISTRIBUTION -------------------------
#single value function for geometric function
adeckgeom <- function(k, p) {
  binom <- p * ((1-p)^(k-1))
  binom
}

#apply the geometric function to an array or vector of values
geomdistro <- function(k, p) {
  prob <- sapply(k, function(k_val) adeckgeom(k_val, p))
  geomdistro <- cbind(k,prob)
  data.frame(geomdistro)
}

#---------------- NEGATIVE BINOMIAL DISTRIBUTION -------------------------
#single value function for geometric function
adecknegBinom <- function(k, r, p) {
  binom <- comb((k-1), (r-1)) * (p^r * ((1-p)^(k-r)))
  binom
}

#apply the geometric function to an array or vector of values
negBinomdistro <- function(k, r, p) {
  prob <- sapply(k, function(k_val) adecknegBinom(k_val, r, p))
  cum_prob <- cumsum(prob)
  negBinomdistro <- cbind(k,prob,cum_prob)
  data.frame(negBinomdistro)
}

#---------------- HYPERGEOMETRIC DISTRIBUTION -------------------------
#single value function for geometric function
adeckHypergeom <- function(k, r, N) {
  m <- r
  binom <- ( comb(r, k) * comb( (N-r), (m-k) ) ) / comb(N,m)
  binom
}

#apply the geometric function to an array or vector of values
hyperGeomdistro <- function(k, r, N) {
  prob <- sapply(k, function(k_val) adeckHypergeom(k_val, r, N))
  cum_prob <- cumsum(prob)
  Hypergeomdistro <- cbind(k,prob,cum_prob)
  data.frame(Hypergeomdistro)
}

#---------------- POISSON DISTRIBUTION -------------------------
#single value function for geometric function
adeckPoisson <- function(k,lambda) {
  poisson <- ((lambda^k)/factorial(k)) * exp(-lambda)
  poisson
}

#apply the geometric function to an array or vector of values
poissondistro <- function(k, lambda) {
  prob <- sapply(k, function(k_val) adeckPoisson(k_val, lambda ))
  cum_prob <- cumsum(prob)
  Poissondistro <- cbind(k,prob,cum_prob)
  data.frame(Poissondistro)
}

#---------------- UNIFORM DISTRIBUTION -------------------------
#single value function for geometric function
# Function to compute the probability for a Uniform Distribution
adeckUnif <- function(k, a, b) {
  ifelse(k >= a & k <= b, 1 / (b - a), 0)
}

# Apply the uniform function to an array or vector of values
uniformdistro <- function(k, a, b) {
  prob <- sapply(k, function(k_val) adeckUnif(k_val, a, b))
  cum_prob <- cumsum(prob) / sum(prob)  # Normalize cumulative probability
  UnifDistro <- cbind(k, prob, cum_prob)
  data.frame(UnifDistro)
}

#---------------- NORMAL DISTRIBUTION -------------------------
#single value function for geometric function
# Function to compute the probability for a Normal Distribution
# Define the normal distribution function

getRnorm <- function(n, mean=NULL, sd=NULL) {
  
  if(is.null(mean) || mean == 0) {
    mean = 0
  }
  else {
    mean=mean
  }
  
  if(is.null(sd) || sd == 1) {
    sd = 1
  }
  else {
    sd = sd
  }
  
  rnormalDistro <- rnorm(n, mean=mean, sd=sd)
  rnormalDistro
  
}

adeckNormal <- function(k, mean, sd) {
  set.seed(100)
  fx <- ( 1/( sd*sqrt(2*pi) ) )*(exp( (-(k-mean)^2)/(2*(sd^2)) ))
  fx
}

adeckreverseNormal <- function(k, mean, sd) {
  set.seed(100)
  fx <- ( 1/( sd*sqrt(2*pi) ) )*(exp( ((mean-k)^2)/(2*(sd^2)) ))
  fx
}

adecknormDistro <- function(xdf, norm_reverse = FALSE) {
  m <- mean(xdf)
  sdev <- sd(xdf)
  fx <- c()
  if (isFALSE(norm_reverse)) {
    fx <- adeckNormal(xdf, m, sdev)
  }
  else {
    fx <- adeckreverseNormal(xdf, m, sdev)
  }
  rfx <- dnorm(xdf, m, sdev)
  df <- cbind(xdf, fx, rfx)
  df <- as.data.frame(df)
  names(df) <- c("k", "prob", "rprob")
  df
}

# erf <- function(x) {
#   integral <- integrate(function(t) exp(-t^2), lower = 0, upper = x)$value
#   (2 / sqrt(pi)) * integral
# }
# 
# # Example usage
# erf(0.707)  # Should return approximately 0.6827
# 
# erf_taylor <- function(x, n_terms = 10) {
#   sum <- 0
#   for (n in 0:(n_terms-1)) {
#     sum <- sum + ((-1)^n * x^(2*n + 1)) / (factorial(n) * (2*n + 1))
#   }
#   (2 / sqrt(pi)) * sum
# }
# 
# # Example usage
# erf_taylor(0.707)  # Should return approximately 0.6827
# 
# erf_approx <- function(x) {
#   1 - 1 / (1 + 0.278393 * x + 0.230389 * x^2 + 0.000972 * x^3 + 0.078108 * x^4)^4
# }
# 
# # Example usage
# erf_approx(0.707)  # Should return approximately 0.6827
# 
# x <- 0.707
# c(Numerical = erf(x), Taylor = erf_taylor(x, 10), Approximation = erf_approx(x))
# 
# 
# 
# # Apply the normal function to an array or vector of values
# Gaussiandistro <- function(k, mean, sd) {
#   prob <- sapply(k, function(k_val) adeckNormal(k_val, mean, sd))
#   cum_prob <- cumsum(prob) / sum(prob)  # Normalize cumulative probability
#   NormalDistro <- cbind(k, prob, cum_prob)
#   data.frame(NormalDistro)
# }

# p_value_PRO <- function(mu,sigma,x) {
#   p <- (1/(sigma*sqrt(2*pi)))*( exp( (-(x-mu)^2)/(2*sigma^2) ) )
#   p
# }
# p_value_PRO <- p_value_PRO(140, 25,130)
# p_value_PRO
# 
# erfz <- function(z) {
#   erfz <- 1 - (( 1/( (1+0.3275911*z)^4 ))* ( exp(-z^2) ))
#   erfz
# }
# ez <- erfz(z)

#CDF-Harts approximation
# my_pnorm_hart <- function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
#   # Standardize q
#   z <- (q - mean) / sd
#   if (z > 6) return(ifelse(log.p, 0, 1)) # Handle extreme cases
#   if (z < -6) return(ifelse(log.p, -Inf, 0))
#   
#   # Constants from Hart's approximation
#   b0 <- 0.2316419
#   b1 <- 0.319381530
#   b2 <- -0.356563782
#   b3 <- 1.781477937
#   b4 <- -1.821255978
#   b5 <- 1.330274429
#   
#   # Compute approximation
#   t <- 1 / (1 + b0 * abs(z))
#   poly <- ((((b5 * t + b4) * t + b3) * t + b2) * t + b1) * t
#   phi <- (1 / sqrt(2 * pi)) * exp(-0.5 * z^2)
#   
#   # Compute cumulative probability
#   Phi <- ifelse(z >= 0, 1 - phi * poly, phi * poly)
#   
#   # Handle lower tail and log options
#   if (!lower.tail) Phi <- 1 - Phi
#   if (log.p) Phi <- log(Phi)
#   
#   return(Phi)
# }
# 
# # Testing with your example
# z <- (130 - 140) / (25 / sqrt(200))
# p_value_R <- 2 * pnorm(-abs(z))   # R's built-in pnorm
# p_value_my <- 2 * my_pnorm_hart(-abs(z))  # My Hart's approximation
# 
# cat("R's p-value:", p_value_R, "\n")
# cat("My p-value:", p_value_my, "\n")
# 
# #CDF-Abraham's approximation
# my_pnorm <- function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
#   # Standardize q
#   z <- (q - mean) / sd
#   
#   # Constants for Abramowitz & Stegun approximation
#   a <- c(0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429)
#   p <- 0.3275911
#   
#   # Compute absolute z
#   sign_z <- ifelse(z < 0, -1, 1)
#   z_abs <- abs(z) / sqrt(2)
#   
#   # Compute error function approximation (Abramowitz & Stegun 7.1.26)
#   t <- 1 / (1 + p * z_abs)
#   erf_approx <- 1 - (((((a[5] * t + a[4]) * t) + a[3]) * t + a[2]) * t + a[1]) * t * exp(-z_abs^2)
#   
#   # Compute Phi(z)
#   phi_z <- 0.5 * (1 + sign_z * erf_approx)
#   
#   # Handle tail cases
#   if (!lower.tail) phi_z <- 1 - phi_z
#   if (log.p) phi_z <- log(phi_z)
#   
#   return(phi_z)
# }
# 
# # Test cases
# my_pnorm(-abs(z)) 


