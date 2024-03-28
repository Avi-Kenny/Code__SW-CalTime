######################.
##### Estimation #####
######################.

if (cfg$sim_which=="estimation") {
  
  #' Run a single simulation
  #'
  #' @return A list with ...
  one_simulation <- function() {
    
    # Generate data
    dat <- generate_dataset(
      data_type = L$data_type,
      mu = 5,
      sigma = L$sigma,
      tau = L$tau,
      # beta_j = c(0,0,0,0),
      beta_j = rep(0, L$n_time_points), # KL attempt # linear period effects
      delta_s = L$true_model$delta_s,
      gamma_j = L$true_model$gamma_j,
      # n_clusters = 24,
      # n_time_points = 4,
      # n_ind_per_cluster = 10,
      # n_extra_time_points = 0
      n_clusters = (L$n_time_points-1)*8, # KL attempt # generalize # of clusters
      n_time_points = L$n_time_points, # KL attempt # set n_time_periods in levels.R
      n_ind_per_cluster = L$n_ind_per_cluster, # KL attempt # set n_ind_per_cluster in levels.R
      n_extra_time_points = L$n_extra_time_points # KL attempt # set n_extra_time_points in levels.$
    )
    
    # Analyze data
    results <- analyze_data(dat=dat, model=L$analysis_model$model,
                            target=L$analysis_model$target)
    
    # True value of ETATE or CTATE
    if (L$estimand=="ETATE") {
      true_tate <- mean(L$true_model$delta_s)
    } else if (L$estimand=="CTATE") {
      # true_tate <- mean(L$true_model$gamma_j[2:3])
      true_tate <- mean(L$true_model$gamma_j[2:(length(L$true_model$gamma_j)-1)]) # KL attempt generalize true_tate code
    }

    # Return results
    return(list(
      est = results$est,
      se = results$se,
      true_tate = true_tate
    ))
    
  }
  
}
