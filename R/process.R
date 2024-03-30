############################################.
##### Test dataset generating function #####
############################################.

if (F) {
  
  # Generate data
  dat <- generate_dataset(
    data_type = "normal",
    mu = 5,
    sigma = 0.5,
    tau = 0.2,
    # beta_j = c(0,2,4,6),
    beta_j = c(0,0,0,0),
    # delta_s = c(5,5,5),
    # gamma_j = c(0,0,0,0),
    # delta_s = c(2,20,200), # KL test with bigger ETE (looks good)
    gamma_j = c(0,0,0,0), # KL test with bigger ETE (looks good)
    # delta_s = c(0,0,0), # KL test with CTE (looks good)
    # gamma_j = c(0,2,20,200), # KL test with CTE (looks good)
    delta_s = c(6,6,6), # KL test with bigger ETE (looks good)
    n_clusters = 24,
    n_time_points = 4,
    n_ind_per_cluster = 10,
    n_extra_time_points = 0
  )
  
  # Visualize cluster means
  dat_plot <- dat %>%
    dplyr::group_by(i,j,seq) %>%
    dplyr::summarize(y_bar=mean(y))
  ggplot(
    dat_plot,
    aes(x=j, y=y_bar, color=factor(seq), group=factor(i))
  ) +
    geom_line(alpha=0.5) +
    labs(color="Sequence")
  
}



############################.
##### VIZ: Scatterplot #####
############################.

# Figures produced: fig_1, fig_2

# sim <- readRDS("SimEngine.out/estimation_1_20231112.rds")

summ <- sim %>% SimEngine::summarize(
  list(name="n_clusters", stat="mean", x="n_clusters"),
  list(name="ICC", stat="mean", x="ICC"),
  list(name="true_tate", stat="mean", x="true_tate"), # KL attempt (add true_tate from one_simulation)
  
  list(stat="mean", x="est_ME"),
  list(stat="mean", x="est_OLS"),
  
  list(stat="bias", estimate="est_ME", truth="true_tate"),
  list(stat="bias", estimate="est_OLS", truth="true_tate"),
  list(name="bias_est_ME_pct", stat="bias_pct", estimate="est_ME", truth="true_tate"),
  list(name="bias_est_OLS_pct", stat="bias_pct", estimate="est_OLS", truth="true_tate"),
  
  list(name="MSE_ME", stat="mse", estimate="est_ME", truth="true_tate"),
  list(name="MSE_OLS", stat="mse", estimate="est_OLS", truth="true_tate"),
  
  list(name="mean_est_se_ME", stat="mean", x="se_ME"), # average se
  list(name="mean_est_se_OLS", stat="mean", x="se_OLS"), # average se
  list(name="mean_est_se_OLS_CR2", stat="mean", x="se_OLS_CR2"), # average se
  list(name="mean_est_se_OLS_CR3", stat="mean", x="se_OLS_CR3"), # average se
  
  list(name="mean_est_var_ME", stat="mean", x="var_ME"), # average variance
  list(name="mean_est_var_OLS", stat="mean", x="var_OLS"), # average variance
  list(name="mean_est_var_OLS_CR2", stat="mean", x="var_OLS_CR2"), # average variance
  list(name="mean_est_var_OLS_CR3", stat="mean", x="var_OLS_CR3"), # average variance
  
  list(name="monte_carlo_se_ME", stat="sd", x="est_ME"), # monte-carlo se
  list(name="monte_carlo_se_OLS", stat="sd", x="est_OLS"), # monte-carlo se

  list(name="CP_ME", stat="coverage", estimate="est_ME", se="se_ME", truth="true_tate"),
  list(name="CP_OLS", stat="coverage", estimate="est_OLS", se="se_OLS", truth="true_tate"),
  list(name="CP_OLS_CR2", stat="coverage", estimate="est_OLS", se="se_OLS_CR2", truth="true_tate"),
  list(name="CP_OLS_CR3", stat="coverage", estimate="est_OLS", se="se_OLS_CR3", truth="true_tate"),
  
  list(name="Power_ME", stat="coverage", estimate="est_ME", se="se_ME", truth=0), # probability 95% CI contains 0
  list(name="Power_OLS", stat="coverage", estimate="est_OLS", se="se_OLS", truth=0), # probability 95% CI contains 0
  list(name="Power_OLS_CR2", stat="coverage", estimate="est_OLS", se="se_OLS_CR2", truth=0), # probability 95% CI contains 0
  list(name="Power_OLS_CR3", stat="coverage", estimate="est_OLS", se="se_OLS_CR3", truth=0) # probability 95% CI contains 0
  
)

# report percent bias in (%), not decimal
summ$bias_est_ME_pct <- summ$bias_est_ME_pct*100
summ$bias_est_OLS_pct <- summ$bias_est_OLS_pct*100

# correct Power
summ$Power_ME <- 1-summ$Power_ME # probability 95% CI doesn't contain 0
summ$Power_OLS <- 1-summ$Power_OLS # probability 95% CI doesn't contain 0
summ$Power_OLS_CR2 <- 1-summ$Power_OLS_CR2 # probability 95% CI doesn't contain 0
summ$Power_OLS_CR3 <- 1-summ$Power_OLS_CR3 # probability 95% CI doesn't contain 0

# report RMSE
summ$RMSE_ME <- sqrt(summ$MSE_ME)
summ$RMSE_OLS <- sqrt(summ$MSE_OLS)

# correct Precision
summ$precision_ME <- 1/summ$mean_est_var_ME
summ$precision_OLS <- 1/summ$mean_est_var_OLS
summ$precision_OLS_CR2 <- 1/summ$mean_est_var_OLS_CR2
summ$precision_OLS_CR3 <- 1/summ$mean_est_var_OLS_CR3

# output summ
print(summ)

# save output dataset
write.csv(x=summ, file=file.path(getwd(), "Output Results",
                                 paste0(sim$levels$estimand, "_dataset_", sim$config$num_sim,".csv")
                                 )
          )
