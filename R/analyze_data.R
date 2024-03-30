#' Generate one stepped wedge dataset
#'
#' @param dat A dataset returned by generate_data()
#' @param model A list containing keys "type" and "target". "type" should be one
#'     of c("IT", "ETI", "CTI"). "target" should be one of c("ATE", "ETATE",
#'     "CTATE").
#' @param target The target estimand; one of c("ATE", "ETATE", "CTATE").
#' @return A list of the form list(est=0, se=0) representing the point estimate
#'     and SE estimate
analyze_data_ME <- function(dat, model, target) {
  
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
      # ests <- as.numeric(s$coefficients[5:7, "Estimate"])
      ests <- as.numeric(s$coefficients[grep('factor\\(s\\)',rownames(s$coefficients)), "Estimate"]) # KL attempt to generalize calling the treatment effects
      # vcov <- vcov(model_obj)[5:7,5:7]
      vcov <- vcov(model_obj)[grep('factor\\(s\\)',rownames(s$coefficients)),grep('factor\\(s\\)',rownames(s$coefficients))] # KL attempt to generalize calling the treatment effects
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
      # ests <- as.numeric(s$coefficients[5:6, "Estimate"])
      ests <- as.numeric(s$coefficients[grep('factor\\(j2\\)',rownames(s$coefficients)), "Estimate"]) # KL attempt to generalize calling the treatment effects
      # vcov <- vcov(model_obj)[5:6,5:6]
      vcov <- vcov(model_obj)[grep('factor\\(j2\\)',rownames(s$coefficients)),grep('factor\\(j2\\)',rownames(s$coefficients))] # KL attempt to generalize calling the treatment effects
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov))
      )
    }
    
  }
  
  return(results)

}

analyze_data_OLS <- function(dat, model, target) {
  
  if (model=="IT") {
    
    model_obj <- lm(
      y ~ factor(j) + x_ij,
      data = dat
    )
    
    if (target=="ATE") {
      s <- summary(model_obj)
      results <- list(
        est = s$coefficients["x_ij", "Estimate"],
        se = s$coefficients["x_ij", "Std. Error"],
        se_CR2 = sqrt(clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR2")["x_ij", "x_ij"]),
        se_CR3 = sqrt(clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR3")["x_ij", "x_ij"])
      )
    }
    
  } else if (model=="ETI") {
    
    model_obj <- lm(
      y ~ factor(j) + factor(s),
      data = dat
    )
    
    if (target=="ETATE") {
      # Hard-coded for a 4x3 design
      s <- summary(model_obj)
      # ests <- as.numeric(s$coefficients[5:7, "Estimate"])
      ests <- as.numeric(s$coefficients[grep('factor\\(s\\)',rownames(s$coefficients)), "Estimate"]) # KL attempt to generalize calling the treatment effects
      # vcov <- vcov(model_obj)[5:7,5:7]
      vcov <- vcov(model_obj)[grep('factor\\(s\\)',rownames(s$coefficients)),grep('factor\\(s\\)',rownames(s$coefficients))] # KL attempt to generalize calling the treatment effects
      vcov_CR2 <- clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR2")[grep('factor\\(s\\)',rownames(s$coefficients)),grep('factor\\(s\\)',rownames(s$coefficients))]
      vcov_CR3 <- clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR3")[grep('factor\\(s\\)',rownames(s$coefficients)),grep('factor\\(s\\)',rownames(s$coefficients))]
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov)),
        se_CR2 = sqrt(sum(vcov_CR2)),
        se_CR3 = sqrt(sum(vcov_CR3))
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
    model_obj <- lm(
      y ~ factor(j) + factor(j2),
      data = dat
    )
    
    if (target=="CTATE") {
      # Hard-coded for a 4x3 design
      s <- summary(model_obj)
      # ests <- as.numeric(s$coefficients[5:6, "Estimate"])
      ests <- as.numeric(s$coefficients[grep('factor\\(j2\\)',rownames(s$coefficients)), "Estimate"]) # KL attempt to generalize calling the treatment effects
      # vcov <- vcov(model_obj)[5:6,5:6]
      vcov <- vcov(model_obj)[grep('factor\\(j2\\)',rownames(s$coefficients)),grep('factor\\(j2\\)',rownames(s$coefficients))] # KL attempt to generalize calling the treatment effects
      vcov_CR2 <- clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR2")[grep('factor\\(j2\\)',rownames(s$coefficients)),grep('factor\\(j2\\)',rownames(s$coefficients))]
      vcov_CR3 <- clubSandwich::vcovCR(model_obj, cluster=dat$i, type="CR3")[grep('factor\\(j2\\)',rownames(s$coefficients)),grep('factor\\(j2\\)',rownames(s$coefficients))]
      results <- list(
        est = mean(ests),
        se = sqrt(sum(vcov)),
        se_CR2 = sqrt(sum(vcov_CR2)),
        se_CR3 = sqrt(sum(vcov_CR3))
      )
    }
    
  }
  
  return(results)
  
}
