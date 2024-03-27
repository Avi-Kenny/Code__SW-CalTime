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
      beta_j = c(0,0,0,0),
      delta_s = L$true_model$delta_s,
      gamma_j = L$true_model$gamma_j,
      n_clusters = 24,
      n_time_points = 4,
      n_ind_per_cluster = 10,
      n_extra_time_points = 0
    )
    
    # Analyze data
    results <- analyze_data(dat=dat, model=L$analysis_model$model,
                            target=L$analysis_model$target)
    
    # True value of ETATE or CTATE
    if (L$estimand=="ETATE") {
      true_tate <- mean(L$true_model$delta_s)
    } else if (L$estimand=="CTATE") {
      true_tate <- mean(L$true_model$gamma_j[2:3])
    }

    # Return results
    return(list(
      est = results$est,
      se = results$se,
      true_tate = true_tate
    ))
    
  }
  
}
