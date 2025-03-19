library(tidyverse)
library(pracma)

# Set parameters
n <- 16           # number of teams in a division, should always be 16
sigma <- 0.6      # parameterize the underlying distribution of team skill
sigma_e <- 0.1    # parameterize the noise around true skill that impacts seed


# Define function to estimate order statistic distribution
osf <- function(x, n, k, sigma, sigma_e){
  sd <- sqrt(sigma^2 + sigma_e^2)
  k * choose(n, k) * 
    pnorm(x, sd = sd)^(k - 1) * 
    (1 - pnorm(x, sd = sd))^(n - k) * 
    dnorm(x, sd = sd)  
}

# Define function to calculate win probability conditional on thetatilde
pwin_cdtl_thetatild <- function(thetatild){
  # print(thetatild)
  thetatild_i <- thetatild[1]
  thetatild_j <- thetatild[2]
  boundaries_i <- c(thetatild_i - 10 * sigma_e, thetatild_i + 10 * sigma_e)
  boundaries_j <- c(thetatild_j - 10 * sigma_e, thetatild_j + 10 * sigma_e)
  
  integral2(
    \(theta_i, theta_j){
      log_integrand <-
        log(pnorm(theta_i - theta_j)) +
        log(dnorm(theta_i, mean = thetatild_i, sd = sigma_e)) +
        log(dnorm(theta_j, mean = thetatild_j, sd = sigma_e))
      integrand <- exp(log_integrand)
      return(integrand)
    }, 
    xmin = boundaries_i[1], 
    xmax = boundaries_i[2], 
    ymin = boundaries_j[1], 
    ymax = boundaries_j[2], 
    abstol = 1e-20
  )$Q
}

# Define function to calculate win probability conditional on ranks (seeds)
pwin_cdtl_R <- function(R){
  R_i <- R[1]
  R_j <- R[2]
  sd <- sqrt(sigma^2 + sigma_e^2)
  bound <- 10 * sd
  
  integral2(
    \(thetatild_i, thetatild_j){
      # print(c(thetatild_i, thetatild_j))
      pw_cdtl <- pwin_cdtl_thetatild(c(thetatild_i, thetatild_j))
      log_integrand <- log(pw_cdtl) +
        log(osf(thetatild_i, n, R_i, sigma, sigma_e)) +
        log(osf(thetatild_j, n, R_j, sigma, sigma_e))
      
      integrand <- exp(log_integrand)
      # print(integrand)
      return(integrand)
    },
    xmin = -bound,
    xmax = bound,
    ymin = -bound,
    ymax = bound,
    vectorized = FALSE)$Q
}

# Check that inner integral performs as expected
pwin_cdtl_thetatild(c(0, 0))
pwin_cdtl_thetatild(c(1, 1))
pwin_cdtl_thetatild(c(1, -1))

# --- Calculate win probabilities for each pair --- #
probabilities <- 
  expand_grid(
    rank_A = n:1,
    rank_B = 1:n) %>% 
  filter(rank_A > rank_B) %>% 
  mutate(
    seed_A = n + 1 - rank_A,
    seed_B = n + 1 - rank_B,
    pwin_A = map2_dbl(rank_A, rank_B, \(a, b){
      print(paste(a, "versus", b))
      pr <- pwin_cdtl_R(c(a, b))
      print(paste("Probability:", pr))
      return(pr)
    }))

# Save probabilities
saveRDS(probabilities, "probs.RDS")
