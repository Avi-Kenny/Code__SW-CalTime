#' Generate one stepped wedge dataset
#'
#' @param dat A dataset returned by generate_data()
#' @param model A list containing keys "type" and "target". "type" should be one
#'     of c("IT", "ETI", "CTI"). "target" should be one of c("ATE", "ETATE",
#'     "CTATE").
#' @param target The target estimand; one of c("ATE", "ETATE", "CTATE").
#' @return A list of the form list(est=0, se=0) representing the point estimate
#'     and SE estimate
analyze_data <- function(dat, model, target) {
  
  if (model=="IT") {
    
    model_obj <- lme4::lmer(
      y ~ factor(j) + x_ij + (1|i),
      data = dat
    )
    
    if (target=="ATE") {
      s <- summary(model_obj)
      results <- list(
        est = s$coefficients["x_ij", "Estimate"],
        se = s$coefficients["x_ij", "Std. Error"]
      )
    }
    
  } else if (model=="ETI") {
    
    model_obj <- lme4::lmer(
      y ~ factor(j) + factor(s) + (1|i),
      data = dat
    )
    
    if (target=="ETATE") {
      # Hard-coded for a 4x3 design
      s <- summary(model_obj)
      ests <- as.numeric(s$coefficients[5:7, "Estimate"])
      vcov <- vcov(model_obj)[5:7,5:7]
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov))
      )
    } else if (target=="LTE") {
      s <- summary(model_obj)
      ests <- as.numeric(s$coefficients[7, "Estimate"])
      vcov <- vcov(model_obj)[7,7]
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov))
      )
    } else {
      # TO DO
    }
    
  } else if (model=="CTI") {
    
    max_j <- max(dat$j)
    dat %<>% dplyr::mutate(
      j2 = x_ij*j*In(j!=max_j)
    )
    model_obj <- lme4::lmer(
      y ~ factor(j) + factor(j2) + (1|i),
      data = dat
    )
    
    if (target=="CTATE") {
      # Hard-coded for a 4x3 design
      s <- summary(model_obj)
      ests <- as.numeric(s$coefficients[5:6, "Estimate"])
      vcov <- vcov(model_obj)[5:6,5:6]
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov))
      )
    }
    
  }
  
  return(results)

}
