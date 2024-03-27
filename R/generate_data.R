#' Generate one stepped wedge dataset
#'
#' @param data_type One of c("normal", "binomial"). type of outcome
#' @param mu Numeric. Overall mean outcome (on linear predictor scale)
#' @param sigma Numeric. SD of the outcome (if data_type="normal")
#' @param tau Numeric. Cluster random effect SD
#' @param delta_s Vector of length J-1. Exposure time treatment effects
#'     (delta_1,...,delta_{J-1})
#' @param gamma_j Vector of length J. Calendar time treatment effects
#'     (gamma_2,...,gamma_J); first must equal zero
#' @param beta_j Vector of length J. Calendar time effects (beta_2,...,beta_J);
#'     first must equal zero
#' @param n_clusters Integer. Number of clusters (I)
#' @param n_time_points Integer. Number of time points (J)
#' @param n_ind_per_cluster Integer. Number of individuals per cluster (K)
#' @param n_extra_time_points Integer. Number of "extra" time points added to
#'     the design.
#' @return A dataframe representing a stepped wedge dataset
generate_dataset <- function(
  data_type, mu, sigma, tau, beta_j, delta_s, gamma_j, n_clusters,
  n_time_points, n_ind_per_cluster, n_extra_time_points
) {
  
  # New variables
  J <- n_time_points + n_extra_time_points
  n_cluster_per_sequence <- n_clusters/(n_time_points-1)
  passed_args <- as.list(environment())
  
  # Dataset checks
  if (n_cluster_per_sequence %% 1 != 0) {
    stop("n_clusters must be divisible by n_time_points-1.")
  }
  if (length(delta_s)!=(J-1)) { stop("Length of delta_s must equal J-1.") }
  if (length(gamma_j)!=J) { stop("Length of delta_s must equal J.") }
  if (length(beta_j)!=J) { stop("Length of delta_s must equal J.") }
  if (beta_j[1]!=0) { stop("beta_j[1] must equal 0.") }
  if (gamma_j[1]!=0) { stop("gamma_j[1] must equal 0.") }
  
  # Generate data frame
  dat <- data.frame(
    "i" = integer(), # index, cluster
    "j" = integer(), # index, time
    "k" = integer(), # index, individual
    "s" = integer(), # exposure time (time since intervention start)
    "x_ij" = integer(), # intervention indicator
    "c_i" = integer(), # crossover time (i.e., start time of intervention)
    "y" = double() # outcome
  )
  
  # Generate crossover times (assumes a "balanced and complete" design)
  crossover_times <- rep(2:n_time_points, each=n_cluster_per_sequence)
  sequence <- as.integer(as.factor(crossover_times))

  # Loop through clusters, time, and individuals
  for (i in 1:n_clusters) {
    
    # Cluster random intercept
    alpha_i <- rnorm(1, mean=0, sd=tau)

    for (j in 1:(n_time_points+n_extra_time_points)) {
      
      # Intervention indicator
      x_ij <- In(j>=crossover_times[i])
      
      # Exposure time index
      s <- ifelse(j>=crossover_times[i], (j-crossover_times[i]+1), 0)
      delta <- ifelse(s>0, delta_s[s], 0)

      # Linear predictor
      mu_ij <- mu + beta_j[j] + (delta+gamma_j[j])*x_ij + alpha_i
      
      # Create new observations
      k <- n_ind_per_cluster
      if (data_type=="normal") {
        y <- mu_ij + rnorm(k, mean=0, sd=sigma)
      } else if (data_type=="binomial") {
        y <- rbinom(n=k, size=1, prob=expit(mu_ij))
      }
      
      # Add new observations to dataset
      dat <- rbind(dat, data.frame(cbind(
        i=rep(i,k), j=rep(j,k), k=c(1:k), s=rep(s,k),
        x_ij=rep(x_ij,k), seq=rep(sequence[i],k), y=y
      )))
      
    }
    
  }
  
  attr(dat, "params") <- passed_args
  
  return (dat)
  
}
