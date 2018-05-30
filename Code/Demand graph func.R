
# Function for probability of choosing E85 given retailer and premium
func_pr85_prem <- function(retailer, indep, coef_est, data, prem){
  if(retailer == "Y"){dta2 <- data %>% dplyr::filter(STNST_CA != 1)}
  if(retailer == "Z"){dta2 <- data %>% dplyr::filter(STNST_CA == 1)}

  indep_noprem <- indep[!(indep %in% "PREM")]
  coef_noprem <- as.numeric(coef_est[!(names(coef_est) %in% "PREM")])
  coef_prem <- coef_est[(names(coef_est) %in% "PREM")]

  return(mean((1/(1+exp(-(coef_prem*prem + data.matrix(dta2[, indep_noprem])%*%coef_noprem))))))
}

# Function for probability of choosing E85 given retailer and ratio
func_pr85_lnratio <- function(retailer, indep, coef_est, data, lnratio){
  if(retailer == "Y"){dta2 <- data %>% dplyr::filter(STNST_CA != 1)}
  if(retailer == "Z"){dta2 <- data %>% dplyr::filter(STNST_CA == 1)}

  indep_nolnratio <- indep[!(indep %in% "LN_RATIO")]
  coef_nolnratio <- as.numeric(coef_est[!(names(coef_est) %in% "LN_RATIO")])
  coef_lnratio <- coef_est[(names(coef_est) %in% "LN_RATIO")]

  return(mean((1/(1+exp(-(coef_lnratio*lnratio + data.matrix(dta2[, indep_nolnratio])%*%coef_nolnratio))))))
}

# Main function with SE's
prob_e85_prem = function(coef_est, vcov_est, data, prem){

  # Define the variables
  indep <- names(coef_est)

  # Calculate probability of choosing E85
  pr85_Y <- as.numeric(func_pr85_prem(retailer = "Y", indep = indep, coef_est = coef_est, data = data, prem = prem))
  pr85_Z <- as.numeric(func_pr85_prem(retailer = "Z", indep = indep, coef_est = coef_est, data = data, prem = prem))

  # Calculate SE of Pr85 for both retailers using delta method
  # Gradient using numDeriv package
  library(numDeriv)
  grad_pr85_Y <- grad(func = func_pr85_prem, x = coef_est, retailer = "Y", indep = indep, data = data, prem = prem)
  grad_pr85_Z <- grad(func = func_pr85_prem, x = coef_est, retailer = "Z", indep = indep, data = data, prem = prem)
  # Variance of mu with delta method
  var_pr85_Y <- t(grad_pr85_Y)%*%vcov_est%*%grad_pr85_Y
  var_pr85_Z <- t(grad_pr85_Z)%*%vcov_est%*%grad_pr85_Z
  # SE of prob is square root of variance
  SE_pr85_Y <- as.numeric(sqrt(var_pr85_Y))
  SE_pr85_Z <- as.numeric(sqrt(var_pr85_Z))

  # Summariz/label/return results
  results <- array(NA, c(1,2)) %>% data.frame
  colnames(results) <- c("Retailer Y", "Retailer Z")
  rownames(results) <- c("Pr85")
  results["Pr85",] <- c(pr85_Y, pr85_Z)
  results["SE of Pr85",] <- c(SE_pr85_Y, SE_pr85_Z)

  return(results)
}


# Main function with SE's
prob_e85_lnratio = function(coef_est, vcov_est, data, lnratio){

  # Define the variables
  indep <- names(coef_est)

  # Calculate probability of choosing E85
  pr85_Y <- as.numeric(func_pr85_lnratio(retailer = "Y", indep = indep, coef_est = coef_est, data = data, lnratio = lnratio))
  pr85_Z <- as.numeric(func_pr85_lnratio(retailer = "Z", indep = indep, coef_est = coef_est, data = data, lnratio = lnratio))

  # Calculate SE of Pr85 for both retailers using delta method
  # Gradient using numDeriv package
  grad_pr85_Y <- grad(func = func_pr85_lnratio, retailer = "Y", indep = indep, data = data, lnratio = lnratio, x = coef_est)
  grad_pr85_Z <- grad(func = func_pr85_lnratio, retailer = "Z", indep = indep, data = data, lnratio = lnratio, x = coef_est)
  # Variance of mu with delta method
  var_pr85_Y <- t(grad_pr85_Y)%*%vcov_est%*%grad_pr85_Y
  var_pr85_Z <- t(grad_pr85_Z)%*%vcov_est%*%grad_pr85_Z
  # SE of mu is square root of variance
  SE_pr85_Y <- as.numeric(sqrt(var_pr85_Y))
  SE_pr85_Z <- as.numeric(sqrt(var_pr85_Z))

  # Summariz/label/return results
  results <- array(NA, c(1,2)) %>% data.frame
  colnames(results) <- c("Retailer Y", "Retailer Z")
  rownames(results) <- c("Pr85")
  results["Pr85",] <- c(pr85_Y, pr85_Z)
  results["SE of Pr85",] <- c(SE_pr85_Y, SE_pr85_Z)

  return(results)
}
