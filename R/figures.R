# Generate Figure

library(ggplot2)
library(dplyr)
library(cowplot)

# import datasets
ETATE.df <- read.csv(file.path(getwd(), "Output Results", "ETATE_dataset_1000.csv"))
CTATE.df <- read.csv(file.path(getwd(), "Output Results", "CTATE_dataset_1000.csv"))
IT.df <- read.csv(file.path(getwd(), "Output Results", "IT_dataset_1000.csv"))


# Barplots
# (x facet) Estimand: IT, ETATE, CTATE
# (barplot fills) Analyses: IT, ETATE, CTATE
# (y facet) bias_est_pct, precision, RMSE, CP, Power
simresults.df <- rbind(ETATE.df, CTATE.df, IT.df)
simresults.df$estimator <- ifelse(
  simresults.df$analysis_model=="IT/ATE", 
  "IT",
  ifelse(
    simresults.df$analysis_model=="ETI/ETATE",
    "ETATE",
    ifelse(
      simresults.df$analysis_model=="CTI/CTATE",
      "CTATE",
      NA
    )
  )
)
simresults.df$estimandlabel <- paste0("Estimand: ", simresults.df$estimand)
# df for reported results
simresults.df2 <- rbind(
  simresults.df %>% mutate(simresult=bias_est_pct, simresultlabel="Percent Bias (%)"),
  simresults.df %>% mutate(simresult=precision, simresultlabel="Precision"),
  simresults.df %>% mutate(simresult=CP, simresultlabel="CP"),
  simresults.df %>% mutate(simresult=RMSE, simresultlabel="RMSE")
)
simresults.df2$simresultlabel <- factor(simresults.df2$simresultlabel, levels=c("Percent Bias (%)","Precision","CP","RMSE"))
colnames(simresults.df2)

# plot facet barplot
ggplot(simresults.df2, aes(x=estimator, y=simresult, color=estimator, fill=estimator)) +
  geom_bar(stat="identity") +
  facet_grid(simresultlabel~estimandlabel, scales = "free") +
  theme_bw() +
  labs(x ="Estimators", y = "Simulation Results") +
  theme(legend.position = "none")
# width: 550, height: 500

# plot appendix plot (Power & MCSE)
ggplot(simresults.df, aes(x=estimator, y=Power, color=estimator, fill=estimator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_grid(~estimandlabel, scales = "free") +
  labs(x ="Estimators", y = "Power") +
  theme(legend.position = "none")
ggplot(simresults.df, aes(x=estimator, y=monte_carlo_se, color=estimator, fill=estimator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_grid(~estimandlabel, scales = "free") +
  labs(x ="Estimators", y = "Monte Carlo SE") +
  theme(legend.position = "none")
# width 550, height=200


# Lineplot of true effect curves
ETE.df <- data.frame(
  IT = filter(ETATE.df, analysis_model=="IT/ATE")$mean_est,
  ETATE = filter(ETATE.df, analysis_model=="ETI/ETATE")$mean_est,
  CTATE = filter(ETATE.df, analysis_model=="CTI/CTATE")$mean_est,
  effect_curve = c(0,0,0.5,1,2,4,6,6,6),
  time = c(1,2,3,4,5,6,7,8,9),
  time_effect = "ETE"
)
CTE.df <- data.frame(
  IT = filter(CTATE.df, analysis_model=="IT/ATE")$mean_est,
  ETATE = filter(CTATE.df, analysis_model=="ETI/ETATE")$mean_est,
  CTATE = filter(CTATE.df, analysis_model=="CTI/CTATE")$mean_est,
  effect_curve = c(6,3,1,0.5,0.1,0,0,0),
  time = c(2,3,4,5,6,7,8,9),
  time_effect = "CTE"
)
# set plot colors
colors <- c("ETATE" = "#00BA38", "CTATE" = "#F8766D", "IT" = "#619CFF")
# plots
ETEplot <-  ggplot(ETE.df, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkgreen", color="darkgreen") +
  geom_line(color="darkgreen") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(title="Simulated exposure time-varying effects", x ="Exposure time (s)", y = "ETE")
CTEplot <- ggplot(CTE.df, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkred", color="darkred") +
  geom_line(color="darkred") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(values = colors) +
  theme_bw() +
  labs(title="Simulated calendar time-varying effects", x ="Calendar time (j)", y = "CTE")
# cowplot altogether:
cowplot::plot_grid(
  cowplot::plot_grid(
    ETEplot+theme(legend.position = "none"),
    CTEplot+theme(legend.position = "none"),
    nrow=1
  ),
  cowplot::get_legend(ETEplot+theme(legend.position = "bottom")+labs(colour="Analysis")),
  nrow=2,
  rel_heights=c(0.9, 0.1)
)
# width: 700, height: 300