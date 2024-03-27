# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Level set: ETATE estimation
  # Figures: ...
  level_sets[["ETATE set 1"]] <- list(
    data_type = "normal",
    estimand = "ETATE",
    sigma = 1,
    tau = 1,
    true_model = list(
      "IT" = list(delta_s=c(6,6,6), gamma_j=c(0,0,0,0)),
      "ETI" = list(delta_s=c(2,4,6), gamma_j=c(0,0,0,0))
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
    tau = 1,
    true_model = list(
      "IT" = list(delta_s=c(6,6,6), gamma_j=c(0,0,0,0)),
      "CTI" = list(delta_s=c(0,0,0), gamma_j=c(0,2,4,6))
    ),
    analysis_model = list(
      "IT/ATE" = list(model="IT", target="ATE"),
      "ETI/ETATE" = list(model="ETI", target="ETATE"),
      "CTI/CTATE" = list(model="CTI", target="CTATE")
    )
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
}
