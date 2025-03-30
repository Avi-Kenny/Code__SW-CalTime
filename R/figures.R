# Generate Figure

library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpattern)

# import datasets
ETATE.df <- read.csv(file.path(getwd(), "Output Results", "ETATE_dataset_1000.csv"))
CTATE.df <- read.csv(file.path(getwd(), "Output Results", "CTATE_dataset_1000.csv"))
IT.df <- read.csv(file.path(getwd(), "Output Results", "IT_dataset_1000.csv"))


# Barplots
# (x facet) Estimand: IT, ETATE, CTATE
# (barplot fills) Analyses: IT, ETATE, CTATE
# (y facet) bias_est_pct, precision, RMSE, CP, Power
simresults.df <- rbind(ETATE.df, CTATE.df, IT.df)
simresults.df$Estimator <- ifelse(
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
str(simresults.df)
# df for reporting results
# Simulated ME results
simresults.df_ME <- rbind(
  simresults.df %>% mutate(simresult=bias_est_ME_pct, simresultlabel="Percent Bias (%)"),
  simresults.df %>% mutate(simresult=precision_ME, simresultlabel="Precision"),
  simresults.df %>% mutate(simresult=CP_ME, simresultlabel="CP"),
  simresults.df %>% mutate(simresult=RMSE_ME, simresultlabel="RMSE")
)
simresults.df_ME$simresultlabel <- factor(simresults.df_ME$simresultlabel, levels=c("Percent Bias (%)","Precision","CP","RMSE"))
colnames(simresults.df_ME)
# Simulated OLS results with model-based variance
simresults.df_OLS <- rbind(
  simresults.df %>% mutate(simresult=bias_est_OLS_pct, simresultlabel="Percent Bias (%)"),
  simresults.df %>% mutate(simresult=precision_OLS, simresultlabel="Precision"),
  simresults.df %>% mutate(simresult=CP_OLS, simresultlabel="CP"),
  simresults.df %>% mutate(simresult=RMSE_OLS, simresultlabel="RMSE")
)
simresults.df_OLS$simresultlabel <- factor(simresults.df_OLS$simresultlabel, levels=c("Percent Bias (%)","Precision","CP","RMSE"))
# Simulated OLS results with cluster-robust variance CR2
simresults.df_OLS_CR2 <- rbind(
  simresults.df %>% mutate(simresult=bias_est_OLS_pct, simresultlabel="Percent Bias (%)"),
  simresults.df %>% mutate(simresult=precision_OLS_CR2, simresultlabel="Precision"),
  simresults.df %>% mutate(simresult=CP_OLS_CR2, simresultlabel="CP"),
  simresults.df %>% mutate(simresult=RMSE_OLS, simresultlabel="RMSE")
)
simresults.df_OLS_CR2$simresultlabel <- factor(simresults.df_OLS$simresultlabel, levels=c("Percent Bias (%)","Precision","CP","RMSE"))
simresults.df_OLS_CR3 <- rbind(
  simresults.df %>% mutate(simresult=bias_est_OLS_pct, simresultlabel="Percent Bias (%)"),
  simresults.df %>% mutate(simresult=precision_OLS_CR3, simresultlabel="Precision"),
  simresults.df %>% mutate(simresult=CP_OLS_CR3, simresultlabel="CP"),
  simresults.df %>% mutate(simresult=RMSE_OLS, simresultlabel="RMSE")
)
simresults.df_OLS_CR3$simresultlabel <- factor(simresults.df_OLS$simresultlabel, levels=c("Percent Bias (%)","Precision","CP","RMSE"))

simresults.df2_bias <- rbind(
  simresults.df_ME %>% mutate(corr = "Exchangeable"),
  simresults.df_OLS %>% mutate(corr = "Independence")
)
simresults.df2_inference <- rbind(
  simresults.df_ME %>% mutate(corr = "Exchangeable"),
  simresults.df_OLS %>% mutate(corr = "Independence (model)"),
  simresults.df_OLS_CR2 %>% mutate(corr = "Independence (CR2)"),
  simresults.df_OLS_CR3 %>% mutate(corr = "Independence (CR3)")
)
simresults.df2_inference$corr <- factor(simresults.df2_inference$corr, levels=c("Exchangeable", "Independence (model)", "Independence (CR2)", "Independence (CR3)"))

# # plot facet barplot with exchangeable exchangeable working correlation
# ggplot(simresults.df_ME, aes(x=Estimator, y=simresult, color=Estimator, fill=Estimator)) +
#   geom_bar(stat="identity") +
#   facet_grid(simresultlabel~estimandlabel, scales = "free") +
#   theme_bw() +
#   labs(title="Analysis with an exchangeable working correlation structure", x ="Estimators", y = "Simulation Results") +
#   theme(legend.position = "none")
# # width: 550, height: 500
# # plot facet barplot with independence working correlation structure
# ggplot(simresults.df_OLS, aes(x=Estimator, y=simresult, color=Estimator, fill=Estimator)) +
#   geom_bar(stat="identity") +
#   facet_grid(simresultlabel~estimandlabel, scales = "free") +
#   theme_bw() +
#   labs(title="Analysis with an independence working correlation structure", x ="Estimators", y = "Simulation Results") +
#   theme(legend.position = "none")
# # width: 550, height: 500

# plot both together
# plot percent bias results
ggplot(
  simresults.df2_bias %>% filter(simresultlabel=="Percent Bias (%)") %>% 
    mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
    mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))), 
  aes(x=corr, y=simresult,  fill=Estimator, color=Estimator)
) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x ="Correlation Structure", y = "Simulation Results") +
  scale_color_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  # aes(x=estimator, y=simresult,  fill=estimator, pattern=corr)
  # ) +
  # ggpattern::geom_bar_pattern(stat="identity", position=position_dodge(), color="black", pattern_fill="black") +
  # ggpattern::scale_pattern_manual(values = c(Exchangeable = "stripe", Independence = "none")) +
  # labs(x ="Estimators", y = "Simulation Results") +
  facet_grid(.~estimandlabel, scales = "free") +
  labs(y="Percent Bias (%)") + 
  theme_bw() +
  theme(legend.position = "bottom")
# width: 650, height: 300

ggplot(
  simresults.df2_inference %>% filter(simresultlabel%in% c("Precision", "CP")) %>% 
    mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
    mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))), 
  aes(x=corr, y=simresult,  fill=Estimator, color=Estimator)
) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x ="Correlation Structure", y = "Simulation Results") +
  scale_color_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  # aes(x=estimator, y=simresult,  fill=estimator, pattern=corr)
  # ) +
  # ggpattern::geom_bar_pattern(stat="identity", position=position_dodge(), color="black", pattern_fill="black") +
  # ggpattern::scale_pattern_manual(values = c(Exchangeable = "stripe", Independence = "none")) +
  # labs(x ="Estimators", y = "Simulation Results") +
  facet_grid(simresultlabel~estimandlabel, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# width: 650, height: 500

# [bw] plot
# ggplot(
#   simresults.df2_inference %>% filter(simresultlabel%in% c("Percent Bias (%)","Precision", "CP")) %>% 
#     mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
#     mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))), 
#   aes(x=corr, y=simresult,  fill=Estimator, color=Estimator)
# ) +
#   geom_bar(stat="identity", position=position_dodge(), color="black") +
#   labs(x ="Correlation Structure", y = "Simulation Results") +
#   # scale_color_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
#   # scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
#   scale_color_manual(values=c("black", "gray40", "gray80")) +
#   scale_fill_manual(values=c("black", "gray40", "gray80")) +
#   # aes(x=estimator, y=simresult,  fill=estimator, pattern=corr)
#   # ) +
#   # ggpattern::geom_bar_pattern(stat="identity", position=position_dodge(), color="black", pattern_fill="black") +
#   # ggpattern::scale_pattern_manual(values = c(Exchangeable = "stripe", Independence = "none")) +
#   # labs(x ="Estimators", y = "Simulation Results") +
#   facet_grid(simresultlabel~estimandlabel, scales = "free") +
#   theme_bw() +
#   theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# # width: 650, height: 500
# ggsave(filename = "sim_results.tiff", plot = last_plot(),
#        width=6.5, height=5, units="in",
#        dpi=800, device='tiff')
cowplot::plot_grid(
  ggplot(
    simresults.df2_bias %>% filter(simresultlabel=="Percent Bias (%)") %>% 
      mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
      mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))), 
    aes(x=corr, y=simresult,  fill=Estimator, color=Estimator)
  ) +
    geom_bar(stat="identity", position=position_dodge(), color="black") +
    labs(x ="Correlation Structure", y = "Simulation Results") +
    scale_color_manual(values=c("black", "gray40", "gray80")) +
    scale_fill_manual(values=c("black", "gray40", "gray80")) +
    facet_grid(simresultlabel~estimandlabel, scales = "free") +
    labs(y=NULL, x=NULL) + 
    theme_bw() +
    theme(legend.position = "none")
  ,
  ggplot(
    simresults.df2_inference %>% filter(simresultlabel%in% c("Precision", "CP")) %>%
      mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
      mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))),
    aes(x=corr, y=simresult,  fill=Estimator, color=Estimator)
  ) +
    geom_bar(stat="identity", position=position_dodge(), color="black") +
    labs(x ="Correlation Structure", y = NULL) +
    # scale_color_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
    # scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
    scale_color_manual(values=c("black", "gray40", "gray80")) +
    scale_fill_manual(values=c("black", "gray40", "gray80")) +
    # aes(x=estimator, y=simresult,  fill=estimator, pattern=corr)
    # ) +
    # ggpattern::geom_bar_pattern(stat="identity", position=position_dodge(), color="black", pattern_fill="black") +
    # ggpattern::scale_pattern_manual(values = c(Exchangeable = "stripe", Independence = "none")) +
    # labs(x ="Estimators", y = "Simulation Results") +
    facet_grid(simresultlabel~estimandlabel, scales = "free") +
    theme_bw() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ,
  nrow=2, rel_heights = c(1, 2), labels = c('A.', 'B.')
)
ggsave(filename = "Kenneth_Menglin_Lee_Figure_9.tiff", plot = last_plot(),
       width=6.25, height=5, units="in",
       dpi=600, device='tiff')
ggsave(filename = "Kenneth_Menglin_Lee_Figure_9.png", plot = last_plot(),
       width=6.25, height=5, units="in",
       dpi=300, device='png')

# plot appendix plot (Power & MCSE)
ggplot(simresults.df, aes(x=Estimator, y=Power_ME, color=Estimator, fill=Estimator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_grid(~estimandlabel, scales = "free") +
  labs(title="Analysis with an exchangeable working correlation structure", x ="Estimators", y = "Power_ME") +
  theme(legend.position = "none")
ggplot(simresults.df %>% 
         mutate(estimandlabel = factor(estimandlabel, levels=c("Estimand: IT", "Estimand: ETATE", "Estimand: CTATE"))) %>%
         mutate(Estimator = factor(Estimator, levels=c("IT", "ETATE", "CTATE"))), 
       aes(x=Estimator, y=monte_carlo_se_ME, color=Estimator, fill=Estimator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_grid(~estimandlabel, scales = "free") +
  labs(x ="Estimators", y = "Monte Carlo SE") +
  scale_color_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  scale_fill_manual(values=c("#619CFF", "#00BA38", "#F8766D")) +
  theme(legend.position = "none")
# width 550, height=200


# Lineplot of true effect curves
ETE.df_ME <- data.frame(
  IT = filter(ETATE.df, analysis_model=="IT/ATE")$mean_est_ME,
  ETATE = filter(ETATE.df, analysis_model=="ETI/ETATE")$mean_est_ME,
  CTATE = filter(ETATE.df, analysis_model=="CTI/CTATE")$mean_est_ME,
  effect_curve = c(0,0,0.5,1,2,4,6,6,6),
  time = c(1,2,3,4,5,6,7,8,9),
  time_effect = "ETE"
)
CTE.df_ME <- data.frame(
  IT = filter(CTATE.df, analysis_model=="IT/ATE")$mean_est_ME,
  ETATE = filter(CTATE.df, analysis_model=="ETI/ETATE")$mean_est_ME,
  CTATE = filter(CTATE.df, analysis_model=="CTI/CTATE")$mean_est_ME,
  effect_curve = c(6,3,1,0.5,0.1,0,0,0),
  time = c(2,3,4,5,6,7,8,9),
  time_effect = "CTE"
)
ETE.df_OLS <- data.frame(
  IT = filter(ETATE.df, analysis_model=="IT/ATE")$mean_est_OLS,
  ETATE = filter(ETATE.df, analysis_model=="ETI/ETATE")$mean_est_OLS,
  CTATE = filter(ETATE.df, analysis_model=="CTI/CTATE")$mean_est_OLS,
  effect_curve = c(0,0,0.5,1,2,4,6,6,6),
  time = c(1,2,3,4,5,6,7,8,9),
  time_effect = "ETE"
)
CTE.df_OLS <- data.frame(
  IT = filter(CTATE.df, analysis_model=="IT/ATE")$mean_est_OLS,
  ETATE = filter(CTATE.df, analysis_model=="ETI/ETATE")$mean_est_OLS,
  CTATE = filter(CTATE.df, analysis_model=="CTI/CTATE")$mean_est_OLS,
  effect_curve = c(6,3,1,0.5,0.1,0,0,0),
  time = c(2,3,4,5,6,7,8,9),
  time_effect = "CTE"
)
# set plot colors
colors <- c("IT" = "#619CFF", "ETATE" = "#00BA38", "CTATE" = "#F8766D")
# plots
ETEplot_ME <-  ggplot(ETE.df_ME, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkgreen", color="darkgreen") +
  geom_line(color="darkgreen") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(breaks = c("IT", "ETATE", "CTATE"), values = colors) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Exchangeable correlation", x ="Exposure time (s)", y = "ETE")
CTEplot_ME <- ggplot(CTE.df_ME, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkred", color="darkred") +
  geom_line(color="darkred") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(breaks = c("IT", "ETATE", "CTATE"), values = colors) +
  theme_bw() +
  labs(title="Exchangeable correlation", x ="Calendar time (j)", y = "CTE")
ETEplot_OLS <-  ggplot(ETE.df_OLS, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkgreen", color="darkgreen") +
  geom_line(color="darkgreen") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(breaks = c("IT", "ETATE", "CTATE"), values = colors) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Independence correlation", x ="Exposure time (s)", y = NULL)
CTEplot_OLS <- ggplot(CTE.df_OLS, aes(x=factor(time), y=effect_curve, group=1)) + 
  geom_point(fill="darkred", color="darkred") +
  geom_line(color="darkred") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(aes(y = IT, color = "IT")) +
  geom_line(aes(y = ETATE, color = "ETATE")) +
  geom_line(aes(y = CTATE, color = "CTATE")) +
  scale_color_manual(breaks = c("IT", "ETATE", "CTATE"), values = colors) +
  theme_bw() +
  labs(title="Independence correlation", x ="Calendar time (j)", y = NULL)

# cowplot altogether:
cowplot::plot_grid(
  cowplot::plot_grid(
    cowplot::plot_grid(
      ETEplot_ME+theme(legend.position = "none") + ylim(-1.5, 6),
      CTEplot_ME+theme(legend.position = "none"),
      nrow=2,
      rel_heights=c(1,0.9), labels = c("A.", "B.")
    ),
    cowplot::plot_grid(
      ETEplot_OLS+theme(legend.position = "none") + ylim(-1.5, 6),
      CTEplot_OLS+theme(legend.position = "none"),
      nrow=2,
      rel_heights=c(1,0.9)
    ),
    ncol=2,
    rel_widths=c(1,0.95)
  ),
  cowplot::get_legend(ETEplot_ME+theme(legend.position = "bottom")+labs(colour="Analysis")),
  nrow=2,
  rel_heights=c(0.9, 0.1)
)
# width: 500, height: 500

# exposure time-varying plots
cowplot::plot_grid(
  cowplot::plot_grid(
    ETEplot_ME+theme(legend.position = "none") + ylim(-1.5, 6),
    ETEplot_OLS+theme(legend.position = "none") + ylim(-1.5, 6),
    nrow=1, rel_widths=c(1,0.95)
  ),
  cowplot::get_legend(ETEplot_ME+theme(legend.position = "bottom")+labs(colour="Analysis")),
  nrow=2, rel_heights=c(0.9, 0.1)
)
# width: 600, height: 320

cowplot::plot_grid(
  cowplot::plot_grid(
    CTEplot_ME+theme(legend.position = "none"),
    CTEplot_OLS+theme(legend.position = "none"),
    nrow=1, rel_widths=c(1,0.95)
  ),
  cowplot::get_legend(CTEplot_ME+theme(legend.position = "bottom")+labs(colour="Analysis")),
  nrow=2, rel_heights=c(0.9, 0.1)
)
# width: 600, height: 320

