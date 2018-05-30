# sp_wmlelogit is the sp-off-rp wmle logit function with Nelder-Mead, numDeriv for the sandwich vcov matrix, and marginal effects
# Returns 'coef', 'res' (df of coefficient/mfx, se, p-val estimates), 'optim', and 'll' (the log-likelihood which is the negative of the optim() value)

sp_wmlelogit = function(rp.dep, rp.indep, sp.dep, sp.indep, svy.wts, data, N_draw, par.start){
  
  ### Generate replicate dataset and random uniform draws outside of likelihood function
  # Keep only the necessary columns and add individual id for each observation
  data <- data.table(seq(1:nrow(data)), data[,unique(c(rp.dep, sp.dep, rp.indep, sp.indep, svy.wts))])
  colnames(data)[1] <- "ID"
  
  # Create the replicated data set
  dta_rep <- do.call("rbind", replicate(N_draw, data, simplify = FALSE))
  
  # Order by observation
  setorder(dta_rep, ID)
  
  # Need a total of 2 uniform draws to construct one conditional logit draw
  dta_rep[, c("u1", "u2") := list( runif(n = nrow(dta_rep), min = 0, max = 1),  runif(n = nrow(dta_rep), min = 0, max = 1))]
  
  # Define the negative log likelihood function (negative because optim() minimizes by default and we want the maximum likelihood)  
  logl <- function(parameters, repdata){
    
    #parameters <- c(par.start, 1)
    #repdata <- dta_rep
    
    # Define function terms
    theta <- as.numeric(as.matrix(parameters))[1:(length(parameters)-1)]
    delta <- as.numeric(as.matrix(parameters))[length(parameters)]
    
    # Calculate RP probabilities
    repdata[, PR_RP85 := as.numeric(plogis(as.matrix(repdata[, rp.indep, with = FALSE])%*%theta))]
    repdata[, PR_RP10 := 1 - repdata[["PR_RP85"]]]
    
    # Draw Eps E85 as if motorist chooses E85 and Eps E10 as if motorist chooses E10
    repdata[, c("EPS85_DRAW1", "EPS10_DRAW1") := list(-log(repdata[["PR_RP85"]]) - log(-log(repdata[["u1"]])), -log(repdata[["PR_RP10"]]) - log(-log(repdata[["u1"]])))]
    
    # Calculate m(Eps E85)
    repdata[, MEPS85:= exp(-exp(-(as.matrix(repdata[,rp.indep, with = FALSE])%*%theta + repdata[["EPS85_DRAW1"]])))]
    
    # Calculate m(Eps E10)
    repdata[, MEPS10:= exp(-exp(-(-(as.matrix(repdata[,rp.indep, with = FALSE])%*%theta) + repdata[["EPS10_DRAW1"]])))]
    
    # Draw Eps E85 as if motorist chooses E10
    repdata[, EPS85_DRAW2:= -log(-log(repdata[["MEPS10"]] * repdata[["u2"]]))]
    
    # Draw Eps E10 as if motorist chooses E85
    repdata[, EPS10_DRAW2:= -log(-log(repdata[["MEPS85"]] * repdata[["u2"]]))]
    
    # Calculate conditional epsilons depending on revealed preference rp.dep
    repdata[, c("EPS85_DRAW", "EPS10_DRAW") := list(repdata[[rp.dep]] * repdata[["EPS85_DRAW1"]] + (1-repdata[[rp.dep]]) * repdata[["EPS85_DRAW2"]], (1-repdata[[rp.dep]]) * repdata[["EPS10_DRAW1"]] + repdata[[rp.dep]] * repdata[["EPS10_DRAW2"]])]
    
    # Calculate ei for the motorist calculated from the conditional draws
    repdata[, EI:= repdata[["EPS10_DRAW"]] - repdata[["EPS85_DRAW"]]]
    
    # Calculate probability expressions
    repdata[, PR_SP85 := plogis(delta*(as.matrix(repdata[, sp.indep, with = FALSE])%*%theta-repdata[["EI"]]))]
    repdata[, PR_SP10 := 1 - repdata[["PR_SP85"]]]
    
    # Keep only necessary columns
    repdata   <- repdata[,c(rp.dep, sp.dep, svy.wts, "PR_RP85", "PR_RP10", "PR_SP85", "PR_SP10", "ID"), with = FALSE]
    
    # Now take the average by observation, this completes the integration
    dta3 <- repdata[, lapply(.SD, mean), by=ID]
    
    rm(repdata)
    gc()
    
    # Log-likelihood
    dta3[, ll := dta3[[svy.wts]] * ( (1-dta3[[rp.dep]])*(1-dta3[[sp.dep]]) * (log(dta3[["PR_RP10"]]) + log(dta3[["PR_SP10"]])) + (1-dta3[[rp.dep]])*(dta3[[sp.dep]]) * (log(dta3[["PR_RP10"]]) + log(dta3[["PR_SP85"]])) + (dta3[[rp.dep]])*(1-dta3[[sp.dep]]) * (log(dta3[["PR_RP85"]]) + log(dta3[["PR_SP10"]])) + ((dta3[[rp.dep]])*(dta3[[sp.dep]])) * (log(dta3[["PR_RP85"]]) + log(dta3[["PR_SP85"]])) )]
    return(-sum(dta3[["ll"]]))
  }
  
  # Define initial values for the parameters
  parameters.start <- c(par.start, 2)
  
  # Name the parameter vector
  names(parameters.start) <- c(rp.indep, "delta")
  
  # With Nelder-Mead
  #logl(parameters.start, dta_rep)
  mle <- optim(par = parameters.start, fn = logl, repdata = dta_rep, hessian = TRUE, control = list(maxit = 250000), method = "Nelder-Mead")
  #rm(parameters.start)

  # Define the vector of estimates theta as k x 1 vector 'theta'
  theta.est <- mle$par[1:length(rp.indep)]
  delta.est <- mle$par[length(rp.indep) + 1]
 
  # Define the maximum likelihood (the negative of the optim value)
  # Note this is the maximum log likelihood, which should be negative because the likelihood is between 0 and 1.
  ll <- -mle$value
  
  #Garbage collection to free-up memory
  gc()
  
  ############################################################
  ##  Calculate the robust sandwich vcov matrix of the MLE ###
  ############################################################
  
  ### This is the proper way, but it takes a long time.
  ###   Create the 'A' matrix 
  A <- -mle$hessian/nrow(data)

  ###   Create the 'B' matrix
  # Generate the k x k matrix of the product of first order partial derivatives wrt theta
  # The matrix is the squared weighted average of the gradients
  foo <- function(par, data){
    g <- grad(logl, par, repdata = data, method = "simple")
    return(tcrossprod(g,g))
  }

  matlistB1 <- lapply(1:round(nrow(data)/3), function(x){foo(mle$par, dta_rep[ID==x])})

  matlistB2 <- lapply(round(nrow(data)/3 +1):round(2*nrow(data)/3), function(x){foo(mle$par, dta_rep[ID==x])})
  
  matlistB3 <- lapply(round(2*nrow(data)/3 +1):nrow(data), function(x){foo(mle$par, dta_rep[ID==x])})
  
  matlistB <- append(matlistB1, matlistB2) %>% append(matlistB3)
  rm(matlistB1, matlistB2, matlistB3)
  
    #Garbage collection to free-up memory
  gc()  
  
  #Take the average of all the k x k matrices in the list
  B <- rowMeans(array(unlist(matlistB), c(nrow(matlistB[[1]]), ncol(matlistB[[1]]), length(matlistB))), dims = 2 )
  rm(matlistB)
  
  #Garbage collection to free-up memory
  gc()
  
  ### Calculate the asymptotic variance-covariance matrix of theta
  vcov <- 1/nrow(data)*solve(A)%*%B%*%solve(A)
  rm(A, B)

  # Calculate the vector of standard errors
  se <- sqrt(diag(vcov))
  
  # Calculate t-statistics
  tstat <- mle$par/se
  
  # And associated p-values
  pval <- 2*pnorm(abs(tstat), lower.tail=F)
  
  # Put results in a new data frame
  Results_coef <- data.frame(cbind(mle$par, se, tstat, pval))
  # Label the columns
  colnames(Results_coef) <- c("Estimate", "Std. Error", "T-Stat", "P-value")
  rm(se, tstat, pval)
  
  ############################
  ###   Marginal effects   ###
  ############################
  
  # Define the marginal effects function that converts the vector of coefficients to a vector of marginal effects
  mfx_func_sp <- function(theta) {
    
    mfx <- mean(dlogis(as.matrix(data[,rp.indep, with =FALSE])%*%theta))*theta
    return(as.vector(mfx))
  }
  
  mfx_est <- mfx_func_sp(theta.est)
  
  ##########################################################
  ### Standard errors of marginal effects (delta method) ###
  ##########################################################
  
  # Use the jacobian() function from numDeriv() to calculate gradient matrix
  grad_mfx <- jacobian(mfx_func_sp, theta.est)
  
  # Vcov matrix of marginal effects with delta method
  vcov_mfx <- grad_mfx%*%vcov[1:nrow(vcov)-1,1:ncol(vcov)-1]%*%t(grad_mfx)
  se_mfx <- sqrt(diag(vcov_mfx))
  
  # Save results
  res2 <- data.frame(mfx_est, se_mfx, Results_coef[1:nrow(Results_coef)-1,1], Results_coef[1:nrow(Results_coef)-1,2])
  colnames(res2) <- c("MFX", "MFX_SE", "Coef", "Coef_SE")
  
  # Calculate t-statistics
  tstat_mfx <- res2$MFX/res2$MFX_SE
  tstat_coef <- res2$Coef/res2$Coef_SE
  
  # And associated p-values
  pval_mfx <- 2*pnorm(abs(tstat_mfx), lower.tail=F)
  pval_coef <- 2*pnorm(abs(tstat_coef), lower.tail=F)
  
  # Save results
  Results_mfx <- data.frame(res2, pval_mfx, pval_coef)
  colnames(Results_mfx) <- c("MFX", "MFX SE", "Coef", "Coef SE", "MFX p-val", "Coef p-val")
  rownames(Results_mfx) <- rp.indep
  rm(grad_mfx, vcov_mfx, mfx_est, pval_coef, pval_mfx, se_mfx, tstat_coef, tstat_mfx, res2)
  
  #Garbage collection to free-up memory
  gc()
  
  #######################################
  ###   McFadden's Pseudo R-squared   ###
  #######################################
  
  # Estimate restricted model with only the intercept
  dta_rep_restrict <- dta_rep[,unique(c("ID", rp.dep, sp.dep, svy.wts, "u1", "u2", "ONES")), with = FALSE]
  
  # Define initial values for the parameter on the intercept
  beta.start <- c(0,delta.est)
  # Name the parameter vector
  names(beta.start) <- c("ONES", "SCALE")
  
  # Store variables names lists
  rp.indep.tmp.storage <- rp.indep
  sp.indep.tmp.storage <- sp.indep
  
  # Change independent variables list for logl function
  rp.indep <- c("ONES")
  sp.indep <- c("ONES")

  ### Calculate the maximum likelihood coefficient estimates
  mle_restrict <- optim(par = beta.start, fn = logl, repdata = dta_rep_restrict, hessian = FALSE, method = "Nelder-Mead", control = list(maxit = 250000))
  rm(beta.start)
  # log likelihood of restricted model
  restrict_ll <- - mle_restrict$value
  
  # Calculate McFadden's pseudo r-squared
  mcfad.rsq <- 1 - ll/restrict_ll
  rm(restrict_ll, dta_rep_restrict, mle_restrict)
  
  rp.indep <- rp.indep.tmp.storage
  sp.indep <- sp.indep.tmp.storage
  rm(rp.indep.tmp.storage, sp.indep.tmp.storage)

  ########################################################
  ###   Calculate Percent Correctly Predicted Values   ###
  ########################################################
  
  # Predicted probabilities for RP setting
  dta_rep[["PP_RP"]] <- plogis(as.matrix(dta_rep[, rp.indep, with = FALSE])%*%(as.matrix(theta.est)))

  # (Conditional) predicted probability for SP setting requires conditional logit draw
  # Construct the conditional epsilon draw
  #dta_rep <- dta_rep %>% mutate(EPS1_DRAW1 = -log(PP_RP) - log(-log(u1)), EPS0_DRAW1 = -log(1-PP_RP) - log(-log(u1)))
  dta_rep[["EPS1_DRAW1"]] <- -log(dta_rep[["PP_RP"]]) - log(-log(dta_rep[["u1"]]))
  dta_rep[["EPS0_DRAW1"]] <- -log(1-dta_rep[["PP_RP"]]) - log(-log(dta_rep[["u1"]]))
  dta_rep[["MEPS1"]] <- exp(-exp(-(as.matrix(dta_rep[, rp.indep, with = FALSE])%*%theta.est + dta_rep[["EPS1_DRAW1"]])))
  dta_rep[["MEPS0"]] <- exp(-exp(-(-(as.matrix(dta_rep[, rp.indep, with = FALSE])%*%theta.est) + dta_rep[["EPS0_DRAW1"]])))
  dta_rep[["EPS1_DRAW2"]] <- -log(-log(dta_rep[["MEPS0"]] * dta_rep[["u2"]]))
  dta_rep[["EPS0_DRAW2"]] <- -log(-log(dta_rep[["MEPS1"]] * dta_rep[["u2"]]))
  dta_rep[["EPS1_DRAW"]] <- dta_rep[[rp.dep]] * dta_rep[["EPS1_DRAW1"]] + (1-dta_rep[[rp.dep]]) * dta_rep[["EPS1_DRAW2"]]
  dta_rep[["EPS0_DRAW"]] <- (1-dta_rep[[rp.dep]]) * dta_rep[["EPS0_DRAW1"]] + dta_rep[[rp.dep]] * dta_rep[["EPS0_DRAW2"]]
  dta_rep[["EI"]] <- dta_rep[["EPS0_DRAW"]] - dta_rep[["EPS1_DRAW"]]
  dta_rep[["PP_SP"]] <- as.numeric(plogis(delta.est*(as.matrix(dta_rep[, sp.indep, with = FALSE])%*%theta.est-dta_rep[["EI"]])))
    
  # Now take the average by observation
  #dta4 <- dta_rep %>% dplyr::select(matches(rp.dep), matches(sp.dep), PP_RP, PP_SP, ID) %>% group_by(ID) %>% summarise_each(funs(mean)) %>% ungroup()
  
  dta4 <- dta_rep[,c("ID", rp.dep, sp.dep, "PP_RP", "PP_SP"), with = FALSE][, lapply(.SD, mean), by=ID]
  
  # Predicted outcomes
  dta4[["RP_PO"]] <- round(dta4[["PP_RP"]])
  dta4[["SP_PO"]] <- round(dta4[["PP_SP"]])
  
  # Actual outcomes
  dta4[["RP_AO"]] <- dta4[[rp.dep]]
  dta4[["SP_AO"]] <- dta4[[sp.dep]]

  # Table summarizing results
  act_00 <- rep(NA, 5)
  act_00[1] <- nrow(dta4[RP_AO==0 & SP_AO==0 & RP_PO ==0 & SP_PO==0])
  act_00[2] <- nrow(dta4[RP_AO==0 & SP_AO==0 & RP_PO ==0 & SP_PO==1])
  act_00[3] <- nrow(dta4[RP_AO==0 & SP_AO==0 & RP_PO ==1 & SP_PO==0])
  act_00[4] <- nrow(dta4[RP_AO==0 & SP_AO==0 & RP_PO ==1 & SP_PO==1])
  act_00[5] <- nrow(dta4[RP_AO==0 & SP_AO==0])

  act_01 <- rep(NA, 5)
  act_01[1] <- nrow(dta4[RP_AO==0 & SP_AO==1 & RP_PO==0 & SP_PO==0])
  act_01[2] <- nrow(dta4[RP_AO==0 & SP_AO==1 & RP_PO==0 & SP_PO==1])
  act_01[3] <- nrow(dta4[RP_AO==0 & SP_AO==1 & RP_PO==1 & SP_PO==0])
  act_01[4] <- nrow(dta4[RP_AO==0 & SP_AO==1 & RP_PO==1 & SP_PO==1])
  act_01[5] <- nrow(dta4[RP_AO==0 & SP_AO==1])
  
  act_10 <- rep(NA, 5)
  act_10[1] <- nrow(dta4[RP_AO==1 & SP_AO==0 & RP_PO==0 & SP_PO==0])
  act_10[2] <- nrow(dta4[RP_AO==1 & SP_AO==0 & RP_PO==0 & SP_PO==1])
  act_10[3] <- nrow(dta4[RP_AO==1 & SP_AO==0 & RP_PO==1 & SP_PO==0])
  act_10[4] <- nrow(dta4[RP_AO==1 & SP_AO==0 & RP_PO==1 & SP_PO==1])
  act_10[5] <- nrow(dta4[RP_AO==1 & SP_AO==0])
  
  act_11 <- rep(NA, 5)
  act_11[1] <- nrow(dta4[RP_AO==1 & SP_AO==1 & RP_PO==0 & SP_PO==0])
  act_11[2] <- nrow(dta4[RP_AO==1 & SP_AO==1 & RP_PO==0 & SP_PO==1])
  act_11[3] <- nrow(dta4[RP_AO==1 & SP_AO==1 & RP_PO==1 & SP_PO==0])
  act_11[4] <- nrow(dta4[RP_AO==1 & SP_AO==1 & RP_PO==1 & SP_PO==1])
  act_11[5] <- nrow(dta4[RP_AO==1 & SP_AO==1])

  total_vec <- rep(NA, 5)
  total_vec[1] <- nrow(dta4[RP_PO==0 & SP_PO==0])
  total_vec[2] <- nrow(dta4[RP_PO==0 & SP_PO==1])
  total_vec[3] <- nrow(dta4[RP_PO==1 & SP_PO==0])
  total_vec[4] <- nrow(dta4[RP_PO==1 & SP_PO==1])
  total_vec[5] <- nrow(dta4)

  perccorr_vec <- rep(NA, 5)
  perccorr_vec[1] <- (act_00[1]+act_01[2]+act_10[3]+act_11[4])/total_vec[5]
  ll_vec <- rep(NA, 5)
  ll_vec[1] <- ll
  mcfad.rsq_vec <- rep(NA, 5)
  mcfad.rsq_vec[1] <- mcfad.rsq

  pps <- rbind(act_00, act_01, act_10, act_11, total_vec, perccorr_vec, ll_vec, mcfad.rsq_vec)
  colnames(pps) <- c("Predict (0,0)", "Predict (0,1)", "Predict (1,0)", "Predict (1,1)", "Total")
  rownames(pps) <- c("Actual (0,0)", "Actual (0,1)", "Actual (1,0)", "Actual (1,1)", "Total", "Percent correct", "Log-likelihood", "Pseudo R-squared")
  rm(act_00, act_01, act_10, act_11, total_vec, dta4)
 
    return(list(coef = Results_coef,  mfx = Results_mfx,  vcov = vcov, optim = mle, pps = pps, ll = ll, R2 = mcfad.rsq))

  #Garbage collection to free-up memory
  gc()
  
}

