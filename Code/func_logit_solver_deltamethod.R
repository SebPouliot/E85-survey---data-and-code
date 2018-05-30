
##############################
### Functions for RP model ###
##############################

logit_RP <- function(formula, data, var, weights = NULL){
  #Variable for weights
  data$.weights <- weights
  #Regression
  if(is.null(weights)){
    re_glm <- glm(formula, family = "binomial", data = data)
  } else {
    re_glm <- glm(formula, family = "binomial", weights = .weights, data = data)
  }
  
  coef <- re_glm$coef
  vcov <- (nrow(data)/(nrow(data) + length(coef)))^2*vcov(re_glm)
  se <- sqrt(diag(vcov))
  
  #Mcfadden R2
  if(is.null(weights)){
    re_glm0 <- glm(E85 ~ 1, family = "binomial", data = data)
  } else {
    re_glm0 <- glm(E85 ~ 1, family = "binomial", weights = .weights, data = data)
  }
  z <- data.matrix(data[, var])
  z0 <- data.matrix(data[, "ONES"])
  coef0 <- re_glm0$coef
  LL0 <- sum(data[, rp_dep]*log(plogis(z0%*%coef0)) + (1-data[, rp_dep])*log((1-plogis(z0%*%coef0))))
  LL <- sum(data[, rp_dep]*log(plogis(z%*%coef)) + (1-data[, rp_dep])*log((1-plogis(z%*%coef))))
  MF_R2 = 1 - LL/LL0
  #Predictions
  pred <- as.numeric(plogis(z%*%coef))
  #Return results
  return(list(reg = re_glm, coef = coef, se = se, vcov = vcov, fitted = pred, MF_R2 = MF_R2))
  #rm(formula, data, var, v_weights, weights, re_glm, w, z, Q, xi, bias, coef, vcov, se)
}

#Functions to calculate marginal effects and their standard errors
mfx_func0 <- function(theta, var, data){
  mfx <- mean(dlogis(as.matrix(data[,var])%*%theta))*theta
  return(as.vector(mfx))
}

mfx_func <- function(theta, vcov, var, data){
  mfx <- mfx_func0(theta, var, data = data)
  grad_mfx <- jacobian(mfx_func0, theta, var = var, data = data)
  se_mfx <- sqrt(diag(grad_mfx%*%vcov%*%t(grad_mfx)))
  results <- cbind(mfx, se_mfx)
  rownames(results) <- var
  return(results)
}

#################################################
###   Functions for the E85 ln ratio models   ###
#################################################

logit_param_ratio = function(coef_est, vcov_est, data){
  
  # Define the variables
  indep <- names(coef_est)
  
  # Assemble vector of coefficient estimates and data means (per retailer) and also big vcov matrix
  # See vec_vcov_func_ratio() below.
  temp <- vec_vcov_func_ratio(indep = indep, coef = coef_est, vcov_coef = vcov_est, data = data)
  # Vector of coefficients and data means
  coefs_and_means_est <- temp$coefs
  # Vcov matrix of coefficients and data means
  coefs_and_means_vcov <- temp$vcov
  
  # Calculate mu (log-ratio) for both retailers as a function of the coef ests and data means.
  # See func_mu_ratio() below.
  mu_Y <- as.numeric(func_mu_ratio(retailer = "Y", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
  mu_Z <- as.numeric(func_mu_ratio(retailer = "Z", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
  
  # Calculate SE of mu for both retailers using delta method
  # Gradient using numDeriv package
  grad_mu_Y <- grad(func = func_mu_ratio, x = temp$coefs, retailer = "Y", data = data, indep = indep) %>% setNames(names(temp$coefs))
  grad_mu_Z <- grad(func = func_mu_ratio, x = temp$coefs, retailer = "Z", data = data, indep = indep) %>% setNames(names(temp$coefs))
  # Variance of mu with delta method
  var_mu_Y <- t(grad_mu_Y)%*%temp$vcov%*%grad_mu_Y
  var_mu_Z <- t(grad_mu_Z)%*%temp$vcov%*%grad_mu_Z
  # SE of mu is square root of variance
  SE_mu_Y <- as.numeric(sqrt(var_mu_Y))
  SE_mu_Z <- as.numeric(sqrt(var_mu_Z))
  
  # Calculate s as a function of the coefficient estimates
  # See func_s_ratio
  s <- as.numeric(func_s_ratio(coef_est = coef_est))
  
  # Calculate SE of s using delta method
  # Gradient using numDeriv package
  grad_s <- grad(func = func_s_ratio, x = coef_est) %>% setNames(indep)
  # Variance of s
  var_s <- t(grad_s)%*%vcov_est%*%grad_s
  # SE of s is square root of variance
  SE_s <- as.numeric(sqrt(var_s))
  
  # Calculate alpha (ratio) for both retailers as a function of the coef ests and data means.
  # See func_alpha_ratio() below.
  alpha_Y <- as.numeric(func_alpha_ratio(retailer = "Y", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
  alpha_Z <- as.numeric(func_alpha_ratio(retailer = "Z", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
  
  # Calculate SE of alpha for both retailers using delta method
  # Gradient using numDeriv package
  grad_alpha_Y <- grad(func = func_alpha_ratio, x = temp$coefs, retailer = "Y", data = data, indep = indep) %>% setNames(names(temp$coefs))
  grad_alpha_Z <- grad(func = func_alpha_ratio, x = temp$coefs, retailer = "Z", data = data, indep = indep) %>% setNames(names(temp$coefs))
  # Variance of alpha with delta method
  var_alpha_Y <- t(grad_alpha_Y)%*%temp$vcov%*%grad_alpha_Y
  var_alpha_Z <- t(grad_alpha_Z)%*%temp$vcov%*%grad_alpha_Z
  # SE of mu is square root of variance
  SE_alpha_Y <- as.numeric(sqrt(var_alpha_Y))
  SE_alpha_Z <- as.numeric(sqrt(var_alpha_Z))
  
  # Calculate beta as a function of the coefficient estimates
  # See func_beta_ratio
  beta <- as.numeric(func_beta_ratio(coef_est = coef_est))
  
  # Calculate SE of beta using delta method
  # Gradient using numDeriv package
  grad_beta <- grad(func = func_beta_ratio, x = coef_est) %>% setNames(indep)
  # Variance of s
  var_beta <- t(grad_beta)%*%vcov_est%*%grad_beta
  # SE of s is square root of variance
  SE_beta <- as.numeric(sqrt(var_beta))
  
  
  # Summarize/label/return results
  results <- array(NA, c(1,2)) %>% data.frame
  colnames(results) <- c("Retailer Y", "Retailer Z")
  rownames(results) <- c("mu (ln ratio)")
  results["mu (ln ratio)",] <- c(mu_Y, mu_Z)
  results["se of mu",] <- c(SE_mu_Y, SE_mu_Z)
  results["s (ln ratio)",] <- c(s, s)
  results["se of s",] <- c(SE_s, SE_s)
  results["alpha (ratio)",] <- c(alpha_Y, alpha_Z)
  results["se of alpha",] <- c(SE_alpha_Y, SE_alpha_Z)
  results["beta (ratio)",] <- c(beta, beta)
  results["se of beta",] <- c(SE_beta, SE_beta)
  
  return(results)
}

# Function for assembling vector of coefs and data means and also making the big vcov matrix
# This is the first function called in the main function above.
vec_vcov_func_ratio <- function(indep, coef, vcov_coef, data){
  
  no_ratio <- rp_indep_ratio[!(indep %in% c("LN_RATIO"))]
  
  mean_Y <- data %>% dplyr::filter(STNST_CA != 1) %>% dplyr::select_(.dots = no_ratio) %>% summarize_all(funs(mean)) %>% as.numeric %>% setNames(paste("my", no_ratio, sep = "_"))
  mean_Z <- data %>% dplyr::filter(STNST_CA == 1) %>% dplyr::select_(.dots = no_ratio) %>% summarize_all(funs(mean)) %>% as.numeric %>% setNames(paste("mz", no_ratio, sep = "_"))
  
  #Compute variance covariance matrix of means
  var_Y <- data %>% dplyr::filter(STNST_CA != 1) %>% dplyr::select_(.dots = no_ratio) %>% var
  colnames(var_Y) <- paste("my", no_ratio, sep = "_")
  rownames(var_Y) <- paste("my", no_ratio, sep = "_")
  var_Z <- data %>% dplyr::filter(STNST_CA == 1) %>% dplyr::select_(.dots = no_ratio) %>% var
  colnames(var_Z) <- paste("mz", no_ratio, sep = "_")
  rownames(var_Z) <- paste("mz", no_ratio, sep = "_")
  
  #Vector of coef and means
  vec <- c(coef, mean_Y, mean_Z)
  
  #Variance-covariance matrix
  mat1 <- matrix(0, nrow=nrow(vcov_coef),ncol = ncol(var_Y))
  mat2 <- matrix(0, nrow=nrow(var_Y), ncol = ncol(vcov_coef))
  mat3 <- matrix(0, nrow=nrow(var_Y), ncol = nrow(var_Y))
  
  m0 <- cbind(vcov_coef, mat1) %>% cbind(mat1)
  m1 <- cbind(mat2, var_Y) %>% cbind(mat3)
  m2 <- cbind(mat2, mat3) %>% cbind(var_Z)
  
  vcov <- rbind(m0, m1) %>% rbind(m2)
  
  return(list(coefs = vec, vcov = vcov))
}


# Function to calculate mu as function of coef ests and data means
# This is the second function called in the main function above.
func_mu_ratio <- function(retailer, coef_est_and_indep_means, data, indep){

  # Rename the long input
  coefs <- coef_est_and_indep_means
  
  # Define coefficients
  coef_est <- coefs[1:length(indep)]
  
  # List of coefficients without the (log) ratio
  coef_noratio <- coef_est[!(names(coef_est) %in% "LN_RATIO")]
  
  # The coefficient for the (log) ratio
  coef_ratio <- coef_est[(names(coef_est) %in% "LN_RATIO")]
  
  # Define indep data means depending on retailer
  if(retailer == "Y"){sum_var <- coefs[names(coefs) %in% paste("my", indep, sep = "_")]}
  if(retailer == "Z"){sum_var <- coefs[names(coefs) %in% paste("mz", indep, sep = "_")]}
  
  # Return mu
  return((-sum_var%*%coef_noratio)/coef_ratio)
}


# Function to calculate s. Note that here is where I impose the negative sign to return positive s
# This is the third function called in the main function above.
func_s_ratio <- function(coef_est){
  coef_ratio <- coef_est[(names(coef_est) %in% "LN_RATIO")]
  return(-1/coef_ratio)
}

# Function to calculate alpha (scale parameter log-logistic in terms of the ratio)
# This is the fourth function called in the main function above
func_alpha_ratio <- function(retailer, coef_est_and_indep_means, data, indep){
  
  # Rename the long input
  coefs <- coef_est_and_indep_means
  
  # Define coefficients
  coef_est <- coefs[1:length(indep)]
  
  # List of coefficients without the (log) ratio
  coef_noratio <- coef_est[!(names(coef_est) %in% "LN_RATIO")]
  
  # The coefficient for the (log) ratio
  coef_ratio <- coef_est[(names(coef_est) %in% "LN_RATIO")]
  
  # Define indep data means depending on retailer
  if(retailer == "Y"){sum_var <- coefs[names(coefs) %in% paste("my", indep, sep = "_")]}
  if(retailer == "Z"){sum_var <- coefs[names(coefs) %in% paste("mz", indep, sep = "_")]}
  
  # Return mu
  return(exp((-sum_var%*%coef_noratio)/coef_ratio))
}


# Function for beta (shape parameter log-logistic in terms of the ratio)
# This is the fifth function called in the main function above
func_beta_ratio <- function(coef_est){
  coef_ratio <- coef_est[(names(coef_est) %in% "LN_RATIO")]
  return(-coef_ratio)
}

# Function for summarizing model results - SPRP paper ratio models
sum_func_lnratio_new <- function(mod_num) {
  df <- data.frame(rbind(
    eval(parse(text = paste("m", mod_num, "_mfx", sep = "")))[c("LN_RATIO", "STNST_AR", "STNST_CA", "STNST_CO", "STNST_OK"), 1:2],
    mu_Y = c(eval(parse(text = paste("m", mod_num, "_res[1,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,1]", sep = "")))),
    mu_Z = c(eval(parse(text = paste("m", mod_num, "_res[1,2]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,2]", sep = "")))),
    s = c(eval(parse(text = paste("m", mod_num, "_res[3,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[4,1]", sep = "")))),
    alpha_Y = c(eval(parse(text = paste("m", mod_num, "_res[5,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[6,1]", sep = "")))),
    alpha_Z = c(eval(parse(text = paste("m", mod_num, "_res[5,2]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[6,2]", sep = "")))),
    beta = c(eval(parse(text = paste("m", mod_num, "_res[7,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[8,1]", sep = "")))),
    n_obs = c(eval(parse(text = paste("m", mod_num, "_nobs", sep = ""))), NA),
    MF_R2 = c(eval(parse(text = paste("m", mod_num, "_R2", sep = ""))), NA),
    E85_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E85", sep = ""))), NA),
    E10_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E10", sep = ""))), NA),
    Total_correct = c(eval(parse(text = paste("m", mod_num, "_fit_total", sep = ""))), NA)))
  #AIC = c(eval(parse(text = paste("mod", mod_num, "$fit$aic", sep = ""))), NA),
  colnames(df) <- c(paste("M", mod_num, "_coef", sep = ""), paste("M", mod_num, "_sd", sep = ""))
  return(df)
}


# Function for summarizing model results - SPRP paper ratio models
sum_func_lnratio_fx <- function(mod_num) {
  df <- data.frame(rbind(
    eval(parse(text = paste("m", mod_num, "_mfx", sep = "")))[c("LN_RATIO", "STN_CO_1", "STN_CO_2", "STN_CO_3", "STN_OK_1", "STN_OK_2", "STN_OK_3", "STN_AR_1", "STN_AR_2", "STN_AR_3", "STN_CA_1", "STN_CA_2", "STN_CA_4", "STN_CA_3", "STN_IA_2", "STN_IA_3", "STN_IA_4"), 1:2],
    mu_Y = c(eval(parse(text = paste("m", mod_num, "_res[1,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,1]", sep = "")))),
    mu_Z = c(eval(parse(text = paste("m", mod_num, "_res[1,2]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,2]", sep = "")))),
    s = c(eval(parse(text = paste("m", mod_num, "_res[3,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[4,1]", sep = "")))),
    alpha_Y = c(eval(parse(text = paste("m", mod_num, "_res[5,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[6,1]", sep = "")))),
    alpha_Z = c(eval(parse(text = paste("m", mod_num, "_res[5,2]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[6,2]", sep = "")))),
    beta = c(eval(parse(text = paste("m", mod_num, "_res[7,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[8,1]", sep = "")))),
    n_obs = c(eval(parse(text = paste("m", mod_num, "_nobs", sep = ""))), NA),
    MF_R2 = c(eval(parse(text = paste("m", mod_num, "_R2", sep = ""))), NA),
    E85_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E85", sep = ""))), NA),
    E10_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E10", sep = ""))), NA),
    Total_correct = c(eval(parse(text = paste("m", mod_num, "_fit_total", sep = ""))), NA)))
  #AIC = c(eval(parse(text = paste("mod", mod_num, "$fit$aic", sep = ""))), NA),
  colnames(df) <- c(paste("M", mod_num, "_coef", sep = ""), paste("M", mod_num, "_sd", sep = ""))
  return(df)
}



################################################
###   Functions for the E85 premium models   ###
################################################

# This is the function to return mu (per retailer), s, and standard errors via delta method
# This is the main, primary function that is called from the other code.
logit_param_prem = function(coef_est, vcov_est, data){
  
  # Define the variables
  indep <- names(coef_est)
  
  # Assemble vector of coefficient estimates and data means (per retailer) and also big vcov matrix
  # See vec_vcov_func_prem() below.
  temp <- vec_vcov_func_prem(indep = indep, coef = coef_est, vcov_coef = vcov_est, data = data)
  # Vector of coefficients and data means
  coefs_and_means_est <- temp$coefs
  # Vcov matrix of coefficients and data means
  coefs_and_means_vcov <- temp$vcov
  
  # Calculate mu (premium) for both retailers as a function of the coef ests and data means.
  # See func_mu_prem() below.
  mu_Y <- as.numeric(func_mu_prem(retailer = "Y", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
  mu_Z <- as.numeric(func_mu_prem(retailer = "Z", coef_est_and_indep_means = temp$coefs, data = data, indep = indep))
   
  # Calculate SE of mu for both retailers using delta method
  # Gradient using numDeriv package
  grad_mu_Y <- grad(func = func_mu_prem, x = temp$coefs, retailer = "Y", data = data, indep = indep) %>% setNames(names(temp$coefs))
  grad_mu_Z <- grad(func = func_mu_prem, x = temp$coefs, retailer = "Z", data = data, indep = indep) %>% setNames(names(temp$coefs))
  # Variance of mu with delta method
  var_mu_Y <- t(grad_mu_Y)%*%temp$vcov%*%grad_mu_Y
  var_mu_Z <- t(grad_mu_Z)%*%temp$vcov%*%grad_mu_Z
  # SE of mu is square root of variance
  SE_mu_Y <- as.numeric(sqrt(var_mu_Y))
  SE_mu_Z <- as.numeric(sqrt(var_mu_Z))
  
  # Calculate s as a function of the coefficient estimates
  # See func_s_prem
  s <- as.numeric(func_s_prem(coef_est = coef_est))
  
  # Calculate SE of s using delta method
  # Gradient using numDeriv package
  grad_s <- grad(func = func_s_prem, x = coef_est) %>% setNames(indep)
  # Variance of s
  var_s <- t(grad_s)%*%vcov_est%*%grad_s
  # SE of s is square root of variance
  SE_s <- as.numeric(sqrt(var_s))
  
  # Summarize/label/return results
  results <- array(NA, c(1,2)) %>% data.frame
  colnames(results) <- c("Retailer Y", "Retailer Z")
  rownames(results) <- c("mu (premium)")
  results["mu (premium)",] <- c(mu_Y, mu_Z)
  results["se of mu",] <- c(SE_mu_Y, SE_mu_Z)
  results["s",] <- c(s, s)
  results["se of s",] <- c(SE_s, SE_s)
  
  return(results)
}

# Function for assembling vector of coefs and data means and also making the big vcov matrix
# This is the first function called in the main function above.
vec_vcov_func_prem <- function(indep, coef, vcov_coef, data){
  
  no_prem <- rp_indep_prem[!(indep %in% c("PREM"))]
  
  mean_Y <- data %>% dplyr::filter(STNST_CA != 1) %>% dplyr::select_(.dots = no_prem) %>% summarize_all(funs(mean)) %>% as.numeric %>% setNames(paste("my", no_prem, sep = "_"))
  mean_Z <- data %>% dplyr::filter(STNST_CA == 1) %>% dplyr::select_(.dots = no_prem) %>% summarize_all(funs(mean)) %>% as.numeric %>% setNames(paste("mz", no_prem, sep = "_"))
  
  #Compute variance covariance matrix of means
  var_Y <- data %>% dplyr::filter(STNST_CA != 1) %>% dplyr::select_(.dots = no_prem) %>% var
  colnames(var_Y) <- paste("my", no_prem, sep = "_")
  rownames(var_Y) <- paste("my", no_prem, sep = "_")
  var_Z <- data %>% dplyr::filter(STNST_CA == 1) %>% dplyr::select_(.dots = no_prem) %>% var
  colnames(var_Z) <- paste("mz", no_prem, sep = "_")
  rownames(var_Z) <- paste("mz", no_prem, sep = "_")
  
  #Vector of coef and means
  vec <- c(coef, mean_Y, mean_Z)
  
  #Variance-covariance matrix
  mat1 <- matrix(0, nrow=nrow(vcov_coef),ncol = ncol(var_Y))
  mat2 <- matrix(0, nrow=nrow(var_Y), ncol = ncol(vcov_coef))
  mat3 <- matrix(0, nrow=nrow(var_Y), ncol = nrow(var_Y))
  
  m0 <- cbind(vcov_coef, mat1) %>% cbind(mat1)
  m1 <- cbind(mat2, var_Y) %>% cbind(mat3)
  m2 <- cbind(mat2, mat3) %>% cbind(var_Z)
  
  vcov <- rbind(m0, m1) %>% rbind(m2)
  
  return(list(coefs = vec, vcov = vcov))
}


# Function to calculate mu as function of coef ests and data means
# This is the second function called in the main function above.
func_mu_prem <- function(retailer, coef_est_and_indep_means, data, indep){
  
  # Rename the long input
  coefs <- coef_est_and_indep_means
  
  # Define coefficients
  coef_est <- coefs[1:length(indep)]
  
  # List of coefficients without the premium
  coef_noprem <- coef_est[!(names(coef_est) %in% "PREM")]
  
  # The coefficient for the premium
  coef_prem <- coef_est[(names(coef_est) %in% "PREM")]
  
  # Define indep data means depending on retailer
  if(retailer == "Y"){sum_var <- coefs[names(coefs) %in% paste("my", indep, sep = "_")]}
  if(retailer == "Z"){sum_var <- coefs[names(coefs) %in% paste("mz", indep, sep = "_")]}
  
  # Return mu
  return((-sum_var%*%coef_noprem)/coef_prem)
}


# Function to calculate s. Note that here is where I impose the negative sign to return positive s
# This is the third function called in the main function above.
func_s_prem <- function(coef_est){
  coef_prem <- coef_est[(names(coef_est) %in% "PREM")]
  return(-1/coef_prem)
}


# Function for summarizing model results - SPRP paper prem models
sum_func_prem_new <- function(mod_num) {
  df <- data.frame(rbind(
    eval(parse(text = paste("m", mod_num, "_mfx", sep = "")))[c("PREM", "STNST_AR", "STNST_CA", "STNST_CO", "STNST_OK"), 1:2],
    mu_Y = c(eval(parse(text = paste("m", mod_num, "_res[1,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,1]", sep = "")))),
    mu_Z = c(eval(parse(text = paste("m", mod_num, "_res[1,2]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[2,2]", sep = "")))),
    s = c(eval(parse(text = paste("m", mod_num, "_res[3,1]", sep = ""))), eval(parse(text = paste("m", mod_num, "_res[4,1]", sep = "")))),
    n_obs = c(eval(parse(text = paste("m", mod_num, "_nobs", sep = ""))), NA),
    MF_R2 = c(eval(parse(text = paste("m", mod_num, "_R2", sep = ""))), NA),
    E85_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E85", sep = ""))), NA),
    E10_correct = c(eval(parse(text = paste("m", mod_num, "_fit_E10", sep = ""))), NA),
    Total_correct = c(eval(parse(text = paste("m", mod_num, "_fit_total", sep = ""))), NA)))
  #AIC = c(eval(parse(text = paste("mod", mod_num, "$fit$aic", sep = ""))), NA),
  colnames(df) <- c(paste("M", mod_num, "_coef", sep = ""), paste("M", mod_num, "_sd", sep = ""))
  return(df)
}
  
