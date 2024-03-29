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
  list(stat="mean", x="est"),
  list(stat="bias", estimate="est", truth="true_tate"),
  list(name="bias_est_pct", stat="bias_pct", estimate="est", truth="true_tate"),
  list(name="MSE", stat="mse", estimate="est", truth="true_tate"),
  list(name="mean_est_se", stat="mean", x="se"), # average se
  list(name="mean_est_var", stat="mean", x="var"), # average variance
  list(name="monte_carlo_se", stat="sd", x="est"), # monte-carlo se
  list(name="CP", stat="coverage", estimate="est", se="se", truth="true_tate"),
  list(name="Power", stat="coverage", estimate="est", se="se", truth=0) # probability 95% CI contains 0
)

# report percent bias in (%), not decimal
summ$bias_est_pct <- summ$bias_est_pct*100

# correct Power
summ$Power <- 1-summ$Power # probability 95% CI doesn't contain 0

# report RMSE
summ$RMSE <- sqrt(summ$MSE)

# correct Precision
summ$precision <- 1/summ$mean_est_var

print(summ)
