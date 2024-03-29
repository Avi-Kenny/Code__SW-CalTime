# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Level set: ETATE estimation
  # Figures: ...
  level_sets[["ETATE set 1"]] <- list(
    data_type = "normal",
    estimand = "ETATE",
    sigma = 1,
    # tau = 1,
    tau = sqrt((0.1*1)/(1-0.1)), # tau2 = ICC*sigma2/(1-ICC) %>% sqrt to get tau
    
    n_time_points = 10,
    n_ind_per_cluster = 30,
    n_extra_time_points = 0,
    
    true_model = list(
      # "IT" = list(delta_s=c(6,6,6), gamma_j=c(0,0,0,0)),
      # "ETI" = list(delta_s=c(2,4,6), gamma_j=c(0,0,0,0))
      
      # "IT" = list(delta_s=c(6,6,6,6,6,6,6,6,6), gamma_j=c(0,0,0,0,0,0,0,0,0,0)), # KL attempt
      "ETI" = list(delta_s=c(0,0,0,1,2,4,6,6,6), gamma_j=c(0,0,0,0,0,0,0,0,0,0)) # KL attempt
      
    ),
    analysis_model = list(
      "IT/ATE" = list(model="IT", target="ATE"),
      "ETI/ETATE" = list(model="ETI", target="ETATE"),
      "CTI/CTATE" = list(model="CTI", target="CTATE")
    )
  )
  
  # Level set: CTATE estimation
  # Figures: ...
  level_sets[["CTATE set 1"]] <- list(
    data_type = "normal",
    estimand = "CTATE",
    sigma = 1,
    # tau = 1,
    tau = sqrt((0.1*1)/(1-0.1)), # tau2 = ICC*sigma2/(1-ICC) %>% sqrt to get tau
    
    # n_time_points = 4,
    # n_ind_per_cluster = 10,
    # n_extra_time_points = 0,
    
    n_time_points = 10,
    n_ind_per_cluster = 30,
    n_extra_time_points = 0,
    
    true_model = list(
      # "IT" = list(delta_s=c(0,0,0), gamma_j=c(0,6,6,6)),
      # "CTI" = list(delta_s=c(0,0,0), gamma_j=c(0,2,4,6))
      
      # "IT" = list(delta_s=c(0,0,0,0,0,0,0,0,0), gamma_j=c(0,6,6,6,6,6,6,6,6,6)), # KL attempt
      "CTI" = list(delta_s=c(0,0,0,0,0,0,0,0,0), gamma_j=c(0,6,3,1,0.5,0.1,0,0,0,0)) # KL attempt
    ),
    analysis_model = list(
      "IT/ATE" = list(model="IT", target="ATE"),
      "ETI/ETATE" = list(model="ETI", target="ETATE"),
      "CTI/CTATE" = list(model="CTI", target="CTATE")
    )
  )
  
  # Level set: IT estimation
  # Figures: ...
  level_sets[["IT set 1"]] <- list(
    data_type = "normal",
    estimand = "IT",
    sigma = 1,
    # tau = 1,
    tau = sqrt((0.1*1)/(1-0.1)), # tau2 = ICC*sigma2/(1-ICC) %>% sqrt to get tau
    
    # n_time_points = 4,
    # n_ind_per_cluster = 10,
    # n_extra_time_points = 0,
    
    n_time_points = 10,
    n_ind_per_cluster = 30,
    n_extra_time_points = 0,
    
    true_model = list(
      # "IT" = list(delta_s=c(6,6,6), gamma_j=c(0,0,0,0))
      
      "IT" = list(delta_s=c(6,6,6,6,6,6,6,6,6), gamma_j=c(0,0,0,0,0,0,0,0,0,0)) # KL attempt
    ),
    analysis_model = list(
      "IT/ATE" = list(model="IT", target="ATE"),
      "ETI/ETATE" = list(model="ETI", target="ETATE"),
      "CTI/CTATE" = list(model="CTI", target="CTATE")
    )
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
}
