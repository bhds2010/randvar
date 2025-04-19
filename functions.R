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
  cum_prob <- cumsum(prob)
  geomdistro <- cbind(k,prob, cum_prob)
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

# Error function integral produces far off values
erf <- function(x) {
  integral <- integrate(function(t) exp(-t^2), lower = 0, upper = x)$value
  (2 / sqrt(pi)) * integral
}

adecknormDistro <- function(xdf, norm_reverse = FALSE, p.val=FALSE) {
  
  #harts approximation for z <=0
  hart_phi_approximation <- function(q, mean = 0, sd = 1, log.p=FALSE, p.val=FALSE) {
    #can get the p-val using p.val or simply supplying q as the abs as done in the scale up.
    if (p.val) {
      q <- -abs(q)
    }
    else {
      q <- q
    }
    
    # Standardize q
    z <- (q - mean) / sd
    #if (z >= 6) return(Inf )
    #if (z < -6) return(-Inf )
    
    #Otherwise approximate here
    #no need for extreme z checks, the formula takes care of it.
    b0 <- 0.2316419
    b1 <- 0.319381530
    b2 <- -0.356563782
    b3 <- 1.781477937
    b4 <- -1.821255978
    b5 <- 1.330274429
    
    #Compute approximation for values <= 0
    t <- 1 / (1 + b0 * abs(z))
    poly <- ((((b5 * t + b4) * t + b3) * t + b2) * t + b1) * t
    phi <- (1 / sqrt(2 * pi)) * exp(-0.5 * z^2) #can also approximate from phi (PDF)
    
    #Compute cumulative probability
    #handles all negative to 0 values based on the formula i suppose
    # z>0 values = 1 - phi
    Phi <- ifelse( z >= 0  , 1 - phi * poly, phi * poly)
    
    if (log.p) Phi <- log(Phi)
    return(Phi)
  }
  
  xdf <- sort(xdf, decreasing=F)
  m <- mean(xdf)
  sdev <- sd(xdf)
  z <- (xdf-m)/(sdev/sqrt(2)) #z_1 <- (xdf-m)/(sdev*sqrt(2))
  fx <- c()
  Fx <- c()
  if (isFALSE(norm_reverse)) {
    fx <- adeckNormal(xdf, m, sdev)
  }
  else {
    fx <- adeckreverseNormal(xdf, m, sdev)
  }
  rfx <- dnorm(xdf, m, sdev)
  Fx <- hart_phi_approximation(z, p.val = p.val)
  rFx <- pnorm(z)
  #pval <- hart_phi_approximation(-abs(z))
  pval <- sapply(z, function(z_val) hart_phi_approximation(-abs(z_val)) )
  rpval <- ifelse(z <= 0, pnorm(z), 1- pnorm(z))
  df <- cbind(xdf, z, fx, rfx, Fx, rFx, pval, rpval)
  df <- as.data.frame(df)
  names(df) <- c("k", "z", "prob", "rprob", "cum_prob", "rcum_prob", "pval", "rpval")
  df
}

#---------------- EXPONENTIAL DENSITY -------------------------








