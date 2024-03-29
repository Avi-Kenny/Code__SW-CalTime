# Generate Figure

library(ggplot2)
library(dplyr)

# import datasets

# Barplots
# (x facet) Estimand: IT, ETATE, CTATE
# (barplot fills) Analyses: IT, ETATE, CTATE
# (y facet) bias_est_pct, precision, RMSE, CP, Power

# Lineplot of true effect curves
# (x facet) Estimand: IT, ETE, CTE
# (horizontal line) mean_est 
# (line colors) Analyses: IT, ETATE, CTATE
ETE_effect_curve <- c(0,0,0.5,1,2,4,6,6,6)
CTE_effect_curve <- c(6,3,1,0.5,0.1,0,0,0)
IT_effect_curve <- c(6,6,6,6,6,6,6,6,6)

ggplot(ETEtestscenario, aes(x=factor(ETE), y=ETE_effect, group=1)) + 
  geom_point(fill="blue") +
  geom_line(color="blue") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_hline(aes(yintercept=ETE_weight), linetype="solid", color = "green", lwd=0.75) +
  theme_bw() +
  facet_grid(~ scenario) +
  # labs(title="Scenarios with different exposure time-varying treatment effects (eTE) \nas analyzed by a constant immediate treatment effect (IT)", x ="eTE", y = "eTE effect") +
  labs(title="Scenarios with different eTE as analyzed by an IT estimator", x ="Exposure time (s)", y = "eTE") +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1))