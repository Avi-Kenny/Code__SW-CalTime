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
    delta_s = c(2,20,200), # KL test with bigger ETE (looks good)
    gamma_j = c(0,0,0,0), # KL test with bigger ETE (looks good)
    # delta_s = c(0,0,0), # KL test with CTE (looks good)
    # gamma_j = c(0,2,20,200), # KL test with CTE (looks good)
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
  list(stat="mean", x="true_tate"), # KL attempt (add true_tate from one_simulation)
  list(stat="mean", x="est"),
  list(stat="bias", estimate="est", truth="true_tate")
)

print(summ)
