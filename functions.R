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
adeckNormal <- function(k, mean, sd) {
  dnorm(k, mean = mean, sd = sd)
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
