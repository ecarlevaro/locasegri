# Run locasegri_ini.R first!
# It loads the necesary libraries.

## FUNCTIONS -----


### Grid ----
# OUTPUTS: a data.table with the grid of a 3 dimensional parameter space. The order of columns is based on the # of steps that each parameter requires such that the parameter with longest steps is last. The data.table keys are the parameter values.
# The min and max value of each parameter is the closest to the required minimin and maximum given the stepSize.
# INPUTS: PARAMS_CFG: tibble with information about each parameter space
#   origin: a 3x1 vector defining the point at which the parameter space will be centred around.
#   startValue = Minimum value for each parameter (the floor)
# rover_MGS() calls this fcn to recover the grid. 
build_grid_p23 <- function(PARAMS_CFG, 
                           origin=NULL, 
                           startValue='automatic') {
  if (is.null(origin)) {
    origin = rep(0, times=NROW(PARAMS_CFG))
    names(origin) = PARAMS_CFG$name
  }
  
  # startValue = 0
  ## TODO: generalise grid construction for N parameters using the do.call() fcn
  # Build sequence of parameter values  in each dimension.
  # The parameter with longest step is the last in the matrix, that's why we NEED TO arrage by N_TICKS
  Ticks <- PARAMS_CFG %>%
    arrange(., N_TICKS)
  lines <- pmap(Ticks, function(...) {
    thisParam <- tibble(...)
    origin <- round(thisParam$iniVal, digits=thisParam$precision)
    
    #if (startValue=='automatic') {
    #  startValue = 0-nJumps*thisParam$stepSize  
    #} else {
    #  startValue = thisParam$minVal-origin[thisParam$name]
    #}
    # END VALUE
    # Force it to be the max possible value
    
    # The start and end values are the closest value to the minVal that is a multiple of stepSize such that a integer number of jumps will reach origin
    startValue = origin - floor((origin-thisParam$minVal)/thisParam$stepSize)*thisParam$stepSize
    endValue = origin + floor((thisParam$maxVal-origin)/thisParam$stepSize)*thisParam$stepSize
    
    {seq(from=startValue, to=endValue, by=thisParam$stepSize)} %>%
      round(., digits=thisParam$precision)
    
  })
  names(lines) = PARAMS_CFG$name
  
  # Build the grid by combining above sequences (lines)
  
  ## Manual code for 2 parameters
  ## # This varies every NROW(beta)*NROW(gamma) rows
  #col1 =  lines$gammaF %x% rep(1, times = NROW(lines$lambda)) 
  ## # This param varies on each row
  #col2 = rep(lines$lambda, times=NROW(lines$gammaF))
  ##View(cbind(col1, col2))
  #Grid <- cbind(col1, col2)
  #colnames(Grid) = PARAMS_CFG$name
  #Grid <- as_tibble(Grid)
  
  dimLines <- map_int(lines, NROW)
  # Manual code for 3 parameters
  Grid <- matrix(NA, 
                 nrow=prod(dimLines), ncol=NROW(PARAMS_CFG),
                 dimnames=list(NULL, PARAMS_CFG$name))
  if (NROW(PARAMS_CFG)==2){
    Grid[, 2] <- rep(lines[[2]], times=prod(dimLines)/length(lines[[2]]))
    Grid[, 1] <- rep(lines[[1]], each=length(lines[[2]]))
  } else if (NROW(PARAMS_CFG)==3) {
    # I think the first column should be the longest
    # Build columns in order, from longest parameter to smallest.
    Grid[,3] <- rep(lines[[3]], times=prod(dimLines)/length(lines[[3]]))
    Grid[,2] <- rep(rep(lines[[2]], each=length(lines[[3]])), times=length(lines[[1]]))
    Grid[,1] <- rep(lines[[1]], each=length(lines[[3]])*length(lines[[2]]))
    colnames(Grid) <- Ticks$name
  }
  
  # Reorder columns as as in PARAMS_CFG
  GridDT <- map_dfc(PARAMS_CFG$name, function(colName) {
    list(Grid[, colName]) %>%
      setNames(., colName)
  }) %>%
    mutate(., 'S'=as.double(NA), 'qLLStab'=as.double(NA), 'c2_hat'=as.double(NA)) %>%
    as.data.table()
  setkeyv(GridDT, PARAMS_CFG$name)
  
  GridDT
  
  ## Manual code for 3 parameters
  ## # This varies every NROW(delta)*NROW(gamma) rows
  #col1 =  lines$rhoR %x% rep(1, times = NROW(lines$phiPi)*NROW(lines$phiX)) 
  ## # This varies every NROW(delta) rows
  #col2 = lines$phiPi %x% rep(1, times=NROW(lines$phiX))
  ## # This param varies on each row
  #col3 = rep(1, times=NROW(lines$rhoR)*NROW(lines$phiPi)) %x% lines$phiX
  #View(cbind(col1, col2, col3))
  #Grid <- cbind(col1, col2, col3)
  #colnames(Grid) = PARAMS_CFG$name
  #Grid <- as_tibble(Grid) 
  
}

# OUTPUTS: a data.table with the grid of a 3 dimensional parameter space. The order of columns is based on the # of steps that each parameter requires such that the parameter with longest steps is last. The data.table keys are the parameter values.
# The min and max value of each parameter is the closest to the required minimin and maximum given the stepSize.
# INPUTS: PARAMS_CFG: tibble with information about each parameter space
#   origin: a 3x1 vector defining the point at which the parameter space will be centred around.
#   startValue = Minimum value for each parameter (the floor)
# rover_MGS() calls this fcn to recover the grid. 
# Requires acces to global objects: X
build_grid_p236 <- function(PARAMS_CFG) {
  # startValue = 0
  ## TODO: generalise grid construction for N parameters using the do.call() fcn
  # Build sequence of parameter values  in each dimension.
  # The parameter with longest step is the last in the matrix, that's why we NEED TO arrange by N_TICKS
  Ticks <- PARAMS_CFG %>%
    arrange(., N_TICKS)
  lines <- pmap(Ticks, function(...) {
    #thisParam <- Ticks[3,]
    thisParam <- tibble(...)
    origin <- round(thisParam$INIVAL_STAGE2, digits=WANTPRECISION[thisParam$name])
    # The start and end values are the closest value to the minVal that is a multiple of stepSize such that a integer number of jumps will reach origin
    startValue = origin - floor((origin-thisParam$minVal)/thisParam$stepSize)*thisParam$stepSize
    endValue = origin + floor((thisParam$maxVal-origin)/thisParam$stepSize)*thisParam$stepSize
    
    {seq(from=startValue, to=endValue, by=thisParam$stepSize)} %>%
      round(., digits=WANTPRECISION[thisParam$name])
    
  })
  names(lines) = Ticks$name
  # Build the grid by combining above sequences (lines)
  
  ## Manual code for 2 parameters
  ## # This varies every NROW(beta)*NROW(gamma) rows
  #col1 =  lines$gammaF %x% rep(1, times = NROW(lines$lambda)) 
  ## # This param varies on each row
  #col2 = rep(lines$lambda, times=NROW(lines$gammaF))
  ##View(cbind(col1, col2))
  #Grid <- cbind(col1, col2)
  #colnames(Grid) = PARAMS_CFG$name
  #Grid <- as_tibble(Grid)
  
  dimLines <- map_int(lines, NROW)
  # Manual code for 3 parameters
  gc()
  print(str_c('The Grid matrix has ', prod(dimLines), 'rows.'))
  Grid <- matrix(NA_real_ , 
                 nrow=prod(dimLines), ncol=NROW(PARAMS_CFG),
                 dimnames=list(NULL, names(lines)))
  if (NROW(PARAMS_CFG)==2){
    Grid[, 2] <- rep(lines[[2]], times=prod(dimLines)/length(lines[[2]]))
    Grid[, 1] <- rep(lines[[1]], each=length(lines[[2]]))
  } else if (NROW(PARAMS_CFG)==3) {
    # I think the first column should be the longest
    # Build columns in order, from longest parameter to smallest.
    Grid[,3] <- rep(lines[[3]], times=prod(dimLines)/length(lines[[3]]))
    Grid[,2] <- rep(rep(lines[[2]], each=length(lines[[3]])), times=length(lines[[1]]))
    Grid[,1] <- rep(lines[[1]], each=length(lines[[3]])*length(lines[[2]]))
    colnames(Grid) <- Ticks$name
  } else if (NROW(PARAMS_CFG)==6) {
    # I think the first column should be the longest
    # Build columns in order, from longest parameter to smallest.
    Grid[,6] <- rep(lines[[6]], 
                    each=prod(length(lines[[5]]), length(lines[[4]]), length(lines[[3]]), length(lines[[2]]), length(lines[[1]])) )
    gc()
    Grid[,5] <- rep(lines[[5]], 
                    each=prod(length(lines[[4]]), length(lines[[3]]), length(lines[[2]]), length(lines[[1]])),
                    times=length(lines[[6]]))
    gc()
    Grid[,4] <- rep(lines[[4]], 
                    each=prod(length(lines[[3]]), length(lines[[2]]), length(lines[[1]])),
                    times=prod(length(lines[[6]]), length(lines[[5]])))
    gc()
    Grid[,3] <- rep(lines[[3]], 
                    each=prod(length(lines[[2]]), length(lines[[1]])),
                    times=prod(length(lines[[6]]), length(lines[[5]]), length(lines[[4]])))
    gc()
    Grid[,2] <- rep(lines[[2]], 
                    each=prod(length(lines[[1]])),
                    times=prod(length(lines[[6]]), length(lines[[5]]), length(lines[[4]]),
                               length(lines[[3]])))
    gc()
    Grid[,1] <- rep(lines[[1]], 
                    times=prod(length(lines[[6]]), length(lines[[5]]), length(lines[[4]]),
                               length(lines[[3]]), length(lines[[2]])))
  } else {
    stop("I don't know how to build a grid with this number of parameters!")
  }
  gc()
  # Reorder columns as as in PARAMS_CFG
  GridStcParams <- map_dfc(PARAMS_CFG$name, function(colName) {
    list(Grid[, colName]) %>%
      setNames(., colName)
    })
  rm(Grid)
  gc()
  
  GridEstValues = matrix(as.double(NA), 
                         ncol = (2+NCOL(X)), nrow=1, 
                   dimnames=list(NULL, 
                            c('S', 'qLLStab', 
                              str_c(rep("c2_hat", times=2), 1:NEQS)) )) %>%
    as_tibble()

  GridDT <- bind_cols(GridStcParams, GridEstValues, 'ROW_NUMBER'=NA) %>%
    as.data.table()
  rm(GridStcParams, GridEstValues)
  gc()
  
  setkeyv(GridDT, PARAMS_CFG$name)
  GridDT[, ROW_NUMBER := 1:NROW(GridDT)]
  
  GridDT
  
}

# This functions resumes the search by function Stage2_rover_Perseverance() using the "Explored_revovery" files created by that function
# It modifies the Grid in the global environment by reference
# It outputs a vector of paramter values (a data.table) that can then be used as iniVal in Stage2_rover_Perseverance()
# INPUTDIR: chr string.  path to the recovery files
recover_exp <- function(INPUTPATH = "") {
  log_print("Recovering from previous search")

    theFiles <- list.files(path=INPUTPATH, pattern='^Explored_recovery_.*')
  if (length(theFiles) == 0) { 
    log_print("No files to recover were found! I keep initial initial values")
    return(PARAMS_CFG$INIVAL_STAGE2)
    }
  # list all files in the directory and select those that start with "Explored_recovery_*"
  sS <- PARAMS_CFG$stepSize
  names(sS) = PARAMS_CFG$name
  
  # Update in the Grid the values already computed.
  walk(theFiles, function(aFile) {
    # aFile  <- theFiles[[1]]
    log_print(paste('Opening file: ', aFile))
    
    thisExp <-    readRDS(paste0(INPUTPATH, aFile))[, ] %>%
      setkeyv(., c('rhoR', 'phiPi', 'phiX', 'rhoTao', 'psiB', 'psiX')) %>%
      unique(.)
    
    # Update Grid by reference. Look up in the Grid using the row number instead of the index
    # (don't use merge() since it updates by not by reference)
    ESTVALUESNAMES <- c("S", "qLLStab",  "c2_hat1",  "c2_hat2")
    #Grid[thisExp[, ROW_NUMBER], ]
    # Updates these rows with these values
    Grid[thisExp[, ROW_NUMBER], (ESTVALUESNAMES) := thisExp[, c("S", "qLLStab",  "c2_hat1",  "c2_hat2")] ]
    log_print(paste('The following file has been updated in the Grid: ', aFile))
    
  })
  # Update nextThetas to be values with S>SCRITICAL, local to to the last exploration and in the Grid
  # The last explored rows are
  log_print(paste0('Opening the last file to compute nextRows. File is: ', INPUTPATH, theFiles[[length(theFiles)]]))
  
  nextRows <- readRDS(paste0(INPUTPATH, theFiles[[length(theFiles)]]))[, ROW_NUMBER] 
  # The new rows are...
  nextRows <- Grid[nextRows, ][S<SCRITICAL, ][, 
                                              list('rhoR' = c(rhoR+sS['rhoR'],		rhoR,             rhoR,           rhoR,               rhoR,           rhoR,  rhoR-sS['rhoR'],		rhoR,             rhoR,           rhoR,               rhoR,           rhoR),                      
                                                   'phiPi' = c(phiPi,               phiPi+sS['phiPi'],	phiPi,          phiPi,              phiPi,          phiPi,	phiPi,               phiPi-sS['phiPi'],	phiPi,          phiPi,              phiPi,          phiPi),                   
                                                   'phiX' = c(phiX,                phiX,             phiX+sS['phiX'],	phiX,               phiX,           phiX,	phiX,                phiX,             phiX-sS['phiX'],	phiX,               phiX,           phiX),                       
                                                   'rhoTao' = c(rhoTao,              rhoTao,           rhoTao,         rhoTao+sS['rhoTao'],	rhoTao,         rhoTao, rhoTao,              rhoTao,           rhoTao,         rhoTao-sS['rhoTao'],	rhoTao,         rhoTao),                    
                                                   'psiB' = c(psiB,                psiB,             psiB,           psiB,               psiB+sS['psiB'],	psiB, 	psiB,                psiB,             psiB,           psiB,               psiB-sS['psiB'],	psiB),                        
                                                   'psiX' = c(psiX,                psiX,             psiX,           psiX,               psiX,           psiX+sS['psiX'], psiX,                psiX,             psiX,           psiX,               psiX,           psiX-sS['psiX']))] %>%
    unique(.) %>%
    {Grid[., list(S, ROW_NUMBER), on=c('rhoR', 'phiPi', 'phiX', 'rhoTao', 'psiB', 'psiX'), nomatch=NULL][
      is.na(S), list(ROW_NUMBER)]$ROW_NUMBER}
  
  # it has to ouput param values, not row numbers
  Grid[nextRows, c('rhoR', 'phiPi', 'phiX', 'rhoTao', 'psiB', 'psiX')]
  
  
}


# AUXILIARY FUNCTIONS ----

# Compute the inverse of input matrix M and then its square root (which is unique for a positive semidefinite matrix)
# INPUT: matrix M.
#       inv: TRUE/FALSE whether you want to compute the inverse of the sq root
# OUTPUT: the matrix N, the square root of M from Singular Value Decomposition such that M = N %*% N
mtx_inv_sqrt <- function(M) {
  s <- svd(M)
  
  # Inverse and square root
  ( s$v %*% diag( sqrt((1/s$d)) ) %*%  t(s$u) )
  
}

# Standard GMM estimation of an IV system
# y: is the vector with observations of the dependant variable
# X: T x k_x matrix of all variables in the structural equation (second-stage)
# Z: T x K_z matrix with all instruments for X (it includes exogenous variables that appear in X, that is, they are their own instrument)
# Output: a vector of (reduced-form) residuals to be used in the second stage.
# It uses as W_T the outer product of Z
# EXAMPLE
# est1$uHat = GMMIV_1st_step(est1$y, est1$Xbar, est1$Zbar)
GMMIV_1st_stage <- function(y,X,Z) {
  # This implementation is the same as setting W_T = Z Z^{T} as suggested as initial W_T in Hall (2005), p60.
  # Projection matrix (projects onto the space spanned by Z)
  PZ = Z%*% solve((t(Z)%*%Z)) %*% t(Z)
  
  # 2SLS equation (first-step GMM estimation)
  thetaHat1 <- solve(t(X) %*% PZ %*% X) %*% (t(X) %*% PZ %*% y)
  
  # uHat
  (y - X%*%thetaHat1) %>%
    as.vector(.)  
}

# Standard GMM estimation of an IV system. Second-stage
# INPUT:
# uHat: T x 1 vector of residuals from the first stage GMMIV_1st_step()
# y: is the vector with observations of the dependant variable
# X: T x k_x matrix of all variables in the structural equation (second-stage)
# Z: T x K_z matrix with all instruments for X (it includes exogenous variables that appear in X, that is, they are their own instrument)
# Output: a vector of (reduced-form) residuals to be used in the second stage.
# EXAMPLE
# thetaHat = GMMIV_2nd_stage(est1$y, est1$Xbar, est1$Zbar)
GMMIV_2nd_stage <- function(uHat, y, X, Z, R, WTlags = NULL) {
  T = NROW(uHat)
  # Moment condition  
  gT = (1/T)*(Z * uHat) # T x K
  WT <- lm(gT ~ 1) %>%
    # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
    sandwich::NeweyWest(., lag=WTlags, prewhite=FALSE, sandwich = FALSE)
  # Impose the restrictions (if any)
  WT <- R * WT  # elemet-by-element multiplication
  WT <- solve(WT)
  
  #thetaHat
  solve(t(X) %*% Z%*%WT%*%t(Z) %*%X) %*% (t(X)%*% Z%*%WT%*%t(Z) %*%y) %>%
    .[,1]
  
}

# CORE TEST FUNCTIONS ----

comp_S_simple <- function(theta0) {
    #theta0 <- PARAMS_CFG$iniVal
    # theta0 <- c('beta1' = -0.71)
    #, 'beta2'=0.6)
     VffLAGS=NULL
    b <- get_b(theta0)
    if(anyNA(b)) { stop(paste0("get_b(theta0) returns NAs in comp_S(). 
                               Make sure you get_b() fcn does what it meant. Theta0 is:", 
                               theta0))}
    Yb = Y %*% b
    
    #real <- c(sam$y1_t, sam$y2_t)
    #mean(Yb - real)
    uHat <- (Mx %*% Yb) %>%
      as.vector()
    
    # First-stage estimator
    fT = Z * uHat # (T x Kz+Kx). Dot product (element by element)
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=NULL, prewhite=FALSE, sandwich = FALSE)
    
    FT = (1/T)*(Ztrans %*% uHat) # (Kz+Kx x 1)
     
    if (KX >0){
      # Two-stage estimator
      # only necessary if X is not-null
      H = Z %*% solve(Vff) %*% Ztrans
      c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
      uHat = as.vector((Yb - X%*%c2_hat)) 
      FT = (1/T)*(Ztrans %*% uHat) # (Kz+Kx x 1)
      fT = Z * uHat # (T x Kz+Kx)
      Vff <- lm(fT ~ 1) %>%
        # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
        sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE) 
      
    }
    
    # S-value from the second-step estimator
    thisW <- diag(x=1, nrow=NROW(FT), ncol=NROW(FT))
    (t(FT) %*% thisW %*% FT)
    (t(FT) %*% FT)
    
  }

# This function is used to find the minumum S value (GMM value) during the optimization process (First step). This function does 2 things: first compute the residuals (uHat) using a 2-step GMM estimator. Then computes the S value. You can provide an specific fcn to compute the residuals.
# INPUTS
  # THETA0: a named vector with parameter values
  # get_b(): a fcn that outputs a named vector for b vector in Yb-Xc
  # UHAT_FCN: a function that computes the residuals uHat
  # DATA: the data to compute the residuals from. It is passed to UHAT_FCN
  # VffLAGS: how many lags to use when computing the variance of the moment conditions.
comp_S <- function(THETA0, get_b=get_b, 
                   UHAT_FCN=NULL, DATA=NULL, 
                   VffLAGS=NULL) {
  #THETA0 <- PARAMS_CFG$iniVal
  # VffLAGS=NULL
  if (hasArg(UHAT_FCN)) {
    objs <- UHAT_FCN(THETA0, DATA)
    uHat <- objs$uHat
    Yb <- uHat
    Z <- objs$Z
    Ztrans <- t(Z)
  } else {
    # Linear separable moment condition
    b <- get_b(THETA0)
    if(anyNA(b)) { stop(paste0("get_b(THETA0) returns NAs in comp_S(). 
                             Make sure you get_b() fcn does what it meant. THETA0 is:", 
                               THETA0))}
    Yb = Y %*% b
    uHat <- (Mx %*% Yb) %>%
      as.vector()
    
  }

  # First-stage estimator
  FT = Ztrans %*% uHat # (Kz+Kx x 1)
  fT = Z * uHat # (T x Kz+Kx). Dot product (element by element)
  Vff <- lm(fT ~ 1) %>%
    # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
    sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE)
  # Impose the restrictions (if any)
  Vff <- R * Vff  # elemet-by-element multiplication
  
  if (KX >0){
    # Two-stage estimator
    # only necessary if X is not-null
    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = as.vector((Yb - X%*%c2_hat)) 
    FT = Ztrans %*% uHat # (Kz+Kx x 1)
    fT = Z * uHat # (T x Kz+Kx)
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE) 
    # Impose the restrictions (if any)
    Vff <- R * Vff  # elemet-by-element multiplication
    
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
  } else {
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt
    Atrans = t(A)  
  }
  
  M_VGamma = diag(KZ) - A %*% solve(Atrans%*%A) %*% Atrans # symmetric matrix
  # Compact SVD: L are the left-hand vectors associated with non-null singular values. These vectors span the matrix M_VGamma
  L = svd(M_VGamma)$u[ , (1:KZMKX)]
  
  (1/T) * ((t(Yb)%*% (Z%*%VffInvSqrt %*% L)) %*% (t(L) %*% (VffInvSqrt%*%Ztrans %*% Yb)))
  
  # S-value from the second-step estimator
  #thisW <- diag(x=1, nrow=NROW(FT), ncol=NROW(FT))
  # (1/T) * t(FT) %*% thisW %*% FT
  
  
}

# This function computes residuals (uHat) using the form Yb - Xc and then computes the S value. It is used to find the minumum S value (GMM value) during the optimization process (First step) when the moment equations are separable in their parameters and their nuisance parameters. For more involved moment equations that share the structural parameters and/or the nuisance parameters you may want to compute the residuals using your own function outside, see comp_S(). Ideally this fcn is called from First_Stage().
comp_uHat_and_S <- function(THETA0, this_b=get_b, 
                   VffLags=NULL) {
  #THETA0 <- PARAMS_CFG$iniVal
  # this_b = GET_B
  # VffLags=3
  
  # Linear separable moment condition
  b <- this_b(THETA0)
  Yb = Y %*% b
  # a GT x 1 vector
  uHat <- (Mx %*% Yb) %>%
    as.vector()
  
  #cuts = seq(from=0, to=2*T, by=T)
  #fT <- map2(cutss, stcEqs, function(pos, thisEq) {
  #  uHat[]
  #})
  
  fT = cbind(Z1 * uHat[1:T], Z2*uHat[(T+1):(2*T)]) # T x K
  
  # fT is a T x (k1+k2+...+kG) matrix where G is the total number of instruments for all equations
  FT = apply(X=fT, MARGIN=2, FUN=sum)
  Vff <- lm(fT ~ 1) %>%
    # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
    sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)
  # Impose the restrictions (if any)
  Vff <- R * Vff  # elemet-by-element multiplication
  
  # First-stage estimator
  #FTMatricial = Ztrans %*% uHat # (K x 1)
  #fT = Z * uHat # (T x 7)
  #Vff <- lm(fT ~ 1) %>%
  # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
  #  sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE)
  
  if (KX >0){
    # Two-stage estimator
    # only necessary if X is not-null
    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = (Yb - X%*%c2_hat) %>% as.vector()
    
    fT = cbind(Z1 * uHat[1:T], Z2*uHat[(T+1):(2*T)]) # T x K
    FT = apply(X=fT, MARGIN=2, FUN=sum) # K x 1
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)
    # Impose the restrictions (if any)
    Vff <- R * Vff  # elemet-by-element multiplication
    
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
  } else {
    # One-stage estimator
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt
    Atrans = t(A)  
  }
  
  M_VGamma = diag(KZ) - A %*% solve(Atrans%*%A) %*% Atrans # symmetric matrix
  # Compact SVD: L are the left-hand vectors associated with non-null singular values. These vectors span the matrix M_VGamma
  L = svd(M_VGamma)$u[ , (1:KZMKX)]
  
  (1/T) * ((t(Yb)%*% (Z%*%VffInvSqrt %*% L)) %*% (t(L) %*% (VffInvSqrt%*%Ztrans %*% Yb)))
  
  
}

# Compute the S-test and the qLL-Stability part for a given theta0.
# Outpust a 2x1 vector with the S value in the first element and the qLLStability in the second
# INPUT: theta0: a named vector with the parameter values under the null
#        T: a integer with the size of the sample (this avoid recomputing on each iteration)
#         get_b: a function which inputs the vector theta0 and outputs the value of the vector b(theta0) such that Y%*%b is the moment equation. It requires from the global environment the following objects:
# STCEQS, Y, Z, Ztrans, T, GET_B, Mx, R
comp_tests <- function(THETA0, VffLags=NULL) {
  #THETA0 <-  minsS$NelderMead$par
  c2_hat= NA
  
  # Compute uHat
  b <- GET_B(THETA0)
  Yb = Y %*% b
  # a GT x 1 vector
  uHat <- (Mx %*% Yb) %>%
          as.vector()
  
  #cuts = seq(from=0, to=2*T, by=T)
  #fT <- map2(cutss, stcEqs, function(pos, thisEq) {
  #  uHat[]
  #})
  
  # fT is a T x (k1+k2+...+kG) matrix where G is the total number of instruments for all equations
  # TODO: optimize this for NEQS equations
  fT = cbind(Z1 * uHat[1:T], Z2*uHat[(T+1):(2*T)]) # T x K
  FT = apply(X=fT, MARGIN=2, FUN=sum)
  Vff <- lm(fT ~ 1) %>%
    # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
    sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)
  # Impose the restrictions (if any)
  Vff <- R * Vff  # elemet-by-element multiplication
  
  if (KX >0){
    # Two-stage estimator
    # only necessary if X is not-null
    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = (Yb - X%*%c2_hat) %>% as.vector()
    
    fT = cbind(Z1 * uHat[1:T], Z2*uHat[(T+1):(2*T)]) # T x K
    FT = apply(X=fT, MARGIN=2, FUN=sum) # K x 1
    Vff <- lm(fT ~ 1) %>%
      sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)
    # Impose the restrictions (if any)
    Vff <- R * Vff  # elemet-by-element multiplication
    
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
  } else {
    # One-stage estimator
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt
    Atrans = t(A)  
  }
  ### S-stat ----
  M_VGamma = diag(KZ) - A %*% solve(Atrans%*%A) %*% Atrans # symmetric matrix
  # Compact SVD: L are the left-hand vectors associated with non-null singular values. These vectors span the matrix M_VGamma
  L = svd(M_VGamma)$u[ , (1:KZMKX)]
  
  theS = (1/T) * ((t(Yb)%*% (Z%*%VffInvSqrt %*% L)) %*% (t(L) %*% (VffInvSqrt%*%Ztrans %*% Yb)))

  ### qLL-Stab (qllStilde) ----
  # Magnusson Mavroeidis 2014, Appendix p17
  # Standarside moments (F hat in the Appendix)
  Fstd_hat = fT %*% VffInvSqrt
  # H_hat
  DeltafT  <- rbind(Fstd_hat[1, ], 
                    base::diff(Fstd_hat, differences=1))
  r_T = cumprod(r * rep(1, times=T)) # (T x kz)
  R = c(1, r_T[1:(T-1)]) %>%
    toeplitz(.) %>%
    {. * (lower.tri(., diag = TRUE))}
  H_hat = R %*% DeltafT # T x kz
  # G_hat
  r_Ttrans = t(r_T)
  # T x T
  M_rT <- diag(T) - r_T %*% solve(r_Ttrans %*% r_T ) %*% r_Ttrans
  # T x kz
  G_hat = M_rT %*% H_hat
  
  # TSSR_Ghat
  # For each column (instrument) sum the square of observations. 
  # Then, sum over columns (instruments)
  TSSR_Ghat = G_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)
  # TSSR_Nhat
  # N_hat (T x kz)
  N_hat = M_ones_T %*% Fstd_hat
  TSSR_Nhat = N_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)
  
  c('S' = theS,
            'qLLStab' = TSSR_Nhat - r * TSSR_Ghat,
            'c2_hat' = c2_hat)
  # Note that c2_hat is a vector, that is why we combine it with other scalars using c()
}
# Compute the only the qLL-Stability part for a given theta0.
# Output a scalar with the qLLStab statistic
# INPUT: theta0: a named vector with the parameter values under the null
#        T: a integer with the size of the sample (this avoid recomputing on each iteration)
#        GET_B: a function which inputs the vector theta0 and outputs the value of the vector b(theta0) such that Y%*%b is the moment equation. 
# It requires from the global environment the following objects:
# STCEQS, Y, Z, T, GET_B, Mx, R
comp_qLLStab <- function(THETA0, typeVarF, VffLags=NULL) {
  #THETA0 = Grid[1, c('alpha'=alpha, 'beta'=beta, 'chi'=chi)]
  #typeVarF = 'const'
  
  # Compute moment function
  b <- GET_B(THETA0)
  Yb = Y %*% b # this is the moment fcn
  # a GT x 1 vector
  
  # fT is a T x (k1+k2+...+kG) matrix where G is the total number of instruments for all equations
  # TODO: optimize this for NEQS equations
  fT = Yb
  FT = apply(X=fT, MARGIN=2, FUN=sum)
  #	Variance of f_t
  if (typeVarF == 'HAC')
  {
    Vff = lm(fT ~ 1) %>%
      sandwich::vcovHAC(., sandwich=FALSE, adjust=TRUE)
  }
  if (typeVarF == 'HC3' || typeVarF=='const')
  {
    Vff <- lm(fT ~ 1) %>% 
      sandwich::vcovHC(., type=typeVarF, sandwich=FALSE)
  }
  if (typeVarF == 'NeweyWest')
  {
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)
  }
  # Impose the restrictions (if any)
  Vff <- RS * Vff  # elemet-by-element multiplication
  if (is_scalar_vector(Vff)) {
    VffInvSqrt = (1/sqrt(Vff))
  } else {
    # One-stage estimator
    VffInvSqrt = mtx_inv_sqrt(Vff)
  }

  ### qLL-Stab (qllStilde) ----
  # Magnusson Mavroeidis 2014, Appendix p17
  # Standarside moments (F hat in the Appendix)
  Fstd_hat = fT %*% VffInvSqrt
  # H_hat
  DeltafT  <- rbind(Fstd_hat[1, ], 
                    base::diff(Fstd_hat, differences=1))
  r_T = cumprod(r * rep(1, times=T)) # (T x kz)
  R = c(1, r_T[1:(T-1)]) %>%
    toeplitz(.) %>%
    {. * (lower.tri(., diag = TRUE))}
  H_hat = R %*% DeltafT # T x kz
  # G_hat
  r_Ttrans = t(r_T)
  # T x T
  M_rT <- diag(T) - r_T %*% solve(r_Ttrans %*% r_T ) %*% r_Ttrans
  # T x kz
  G_hat = M_rT %*% H_hat
  
  # TSSR_Ghat
  # For each column (instrument) sum the square of observations. 
  # Then, sum over columns (instruments)
  TSSR_Ghat = G_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)
  # TSSR_Nhat
  # N_hat (T x kz)
  N_hat = M_ones_T %*% Fstd_hat
  TSSR_Nhat = N_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)
  
  # the S 
  S_T <- mean(fT) * VffInvSqrt * mean(fT)
  
  c('qLLStab' = TSSR_Nhat - r * TSSR_Ghat,
    'S' = S_T)
  
}

# get_deriv_B: a function that inputs the current value of theta (theta0) and output a the matrix derivative of b(theta). b(theta) is a row (r x 1) vector and theta is a (p x 1) vector. 
# The derivative of b(theta) with respect to theta is a (r x p) matrix where each element is the derivative of the element r_i in b(theta) with respect to the element p_i in theta.
comp_grad <- function(theta0, this_b) {
  #theta0 = PARAMS_CFG$iniVal
  # theta0 = c('lambda'=0.015, 'gammaF'=0.591)
  # Get it from the glboal environment. It's hard to pass parameters to optim
  derivB=get_deriv_B
  
  VffLAGS = NULL
  
  b <- this_b(theta0)
  Yb = Y %*% b
  uHat <- (Mx %*% Yb) %>%
    as.vector()
  # First-stage estimator
  FT = Ztrans %*% uHat # (7 x 1)
  fT = Z * uHat # (T x 7)
  Vff <- lm(fT ~ 1) %>%
    # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
    sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE)
  
  if (KX >0){
    # Two-stage estimator
    # only necessary if X is not-null
    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = (Yb - X%*%c2_hat) %>% as.vector()
    FT = Ztrans %*% uHat # (7 x 1)
    fT = Z * uHat # (T x 7)
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=VffLAGS, prewhite=FALSE, sandwich = FALSE) 
    # The summetric square root of the inverse of Vff
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
  } else {
    # One-stage estimator
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt
    Atrans = t(A)  
  }
  
  partialB <- derivB(theta0)
  #(2*(1/T)^2) * ( t(L) %*% VffInvSqrt %*% Ztrans %*% Y %*% partialB )
  
  (2*(t(FT)*(1/T))) %*% solve(Vff) %*% ((1/T)*(Ztrans%*%Y %*% partialB))
  
}

# GLOBAL FUNCTIONS ----

# Find minimum value for the GMM function starting on an initial value.
# Rotate the initial value if the minimium is not in the parameter space.
# The minimim value is found using simplex method Nelder-Mead by default, see ?optim
Stage1 <- function(method='NelderMead', gr_T=NULL, derivB=get_deriv_B, 
                        data=NULL, uHat_fcn=NULL, SIV=NULL, WTLags, this_b=GET_B) {
  # SIV=stdIVobjs
  # method = 'Closed'
  # WTlags = NULL
  thetaIni <- PARAMS_CFG$iniVal
  if (WTLags == -1) {
    thisVffLags <- NULL  
  } else {
    thisVffLags <- WTLags
  }
  
  tic()
  # Find minimum
  # TODO: set many initial values in parallel. They should all
  # converge to the same minima
  i = 0
  repeat {
    #
    # OPTIMIZATION
    #
    # TODO: these objects are built on a case-by-case basis. Could be
    # generalised.
    if (method=='Closed') {
      uHat <- GMMIV_1st_stage(SIV$y, SIV$X, SIV$Z)
      # this thetaHat contains the nuisance parameters estimate
      thetaHat <- GMMIV_2nd_stage(uHat, SIV$y, SIV$X, SIV$Z, SIV$R, WTlags)
      #  Remove the nuisance param estimate (if it exists)
      if (exists('X', mode='numeric')) {
        thetaHat <- thetaHat[-NROW(thetaHat)]
      }  
      names(thetaHat) <- PARAMS_CFG$name
      optimRes <- list('par' = thetaHat,
                       'value' = comp_S(thetaHat),
                       'counts' = c('function'=1, 'gradient'=0),
                       'convergence' = 0,
                       'message' = NA)
    }  else if (method=='LBFGSB') {
      #comp_grad(c('lambda'=0.017, 'gammaF' = 0.491))
      optimRes <- optim(thetaIni, comp_uHat_and_S,
                        method="L-BFGS-B",
                        lower=PARAMS_CFG$minVal, upper=PARAMS_CFG$maxVal,
                        gr=gr_T,
                        this_b=GET_B, VffLags=thisVffLags)
    } else {
      optimRes <- optim(thetaIni, comp_uHat_and_S, this_b=GET_B, VffLags=thisVffLags,
                        method='Nelder-Mead',
                        control = list('maxit' = 1500))
      optimRes$message <- NA
    }
    optimRes$VffLags <- as.character(thisVffLags)

    # thetaIni must be a named vector
    log_print(str_c("Initial parameter vector: ", thetaIni,
                collapse=', '))
    #
    # ROTATION
    #
    message(paste0("The minimum for the S fcn is: ", optimRes$value))
    log_print(optimRes$par)
    Sys.sleep(6)
    paste0("Rounding to ", WANTPRECISION, " decimals.")
    optimRes$par <- map2_dbl(optimRes$par, WANTPRECISION, round)
    if (optimRes$convergence == 0) { 
      log_print(str_c('Convergence achieved at S: ', 
                  round(optimRes$value, 4),
                  collapse=', '))
      log_print('The optimal point is: ')
      log_print(optimRes$par)
    } else {
      log_print(paste0("WARNING: Convergence not achieved. Method: ", method))
    }
    if((optimRes$value < SCRITICAL) & 
       prod(optimRes$par >= PARAMS_CFG$minVal) &
       prod(optimRes$par <= PARAMS_CFG$maxVal)) {
      log_print('Minimum of S function below critical value. Continue exploration.')
      break
    } else {
      # Select offending parameters. Substract -1 because I need TRUEs on the offendig parameters
      offParams <- as.logical((optimRes$par > PARAMS_CFG$minVal)*(optimRes$par < PARAMS_CFG$maxVal)-1) %>%
                    PARAMS_CFG[., ]
      offParams
      log_print(paste0(offParams$name, " is outside the parameter region!."))
      log_print(str_c('The minimum of the S (GMM) function is ', round(optimRes$value, 4),
                  '. The critical value is ', SCRITICAL, 
                  collapse=', '))
      log_print('The optimal point is: ')
      log_print(optimRes$par)
      if (i>9){
        break
      }
      log_print('Trying a poor-man rotation of the initial value.')
      i = i+1
      # Rotate initial value
      angle = ((2*pi)/10)*i
      Rot <- diag(NPARAMS)
      # Only rotate the first 2 paramters, up to you if you want other rotations
      Rot[c(1,2), c(1,2)] <- rbind(c(cos(angle), -sin(angle)),
                                 c(sin(angle), cos(angle)))
      thetaIni <- {Rot %*% thetaIni} %>% c(.)
      names(thetaIni) <- PARAMS_CFG$name
      log_print(thetaIni)
    }
  } # close repeat
  optimRes$tSeconds <- toc() %>%
    {.$toc - .$tic}
  
  optimRes
  
}

# All these variables should be defined beforehand
#Y, Z, Ztrans: matrices with data
#T: integer, the number of observations
# get_b: a function which accepts the vector theta0 and outputs the value of the vector b(theta0) such that Y%*%b is the moment equation.
# Requires access to global objects: STCPARAMNAMES, ESTVALUESNAMES, SCRITICAL, NPARAMS, GET_B, nextThetas
Stage2_rover_Perseverance <- function(iniVal, WTLags ) {  
  # DEBUG
  # iniVal <- PARAMS_CFG$INIVAL_STAGE2
  # iniVal <- IniVals
  # WTLags = 3
  #browser()
  
  if (WTLags == -1) {
    thisVffLags <- NULL  
  } else {
    thisVffLags <- WTLags
  }
  
  # Character vectors to select columns in the stupid Grid (which is a data.table)
  STCPARAMNAMES <- PARAMS_CFG$name
  # Column names with tests and nuisance parameters
  ESTVALUESNAMES <- c("S", "qLLStab",  "c2_hat1",  "c2_hat2")
  sS <- PARAMS_CFG$stepSize
  names(sS) = PARAMS_CFG$name
  
   
  
  if (is.double(iniVal)) { 
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                     substr(Sys.time(), start=15, stop=16),
                     ". Ini value is a vector. Compute the space of vector local to this."))
    
    # Compute values in the neighbourhood of iniVal
    Nadj = NCORES*STEPSPERCORE
    StepsM = rbind( (1:Nadj) %x% diag(PARAMS_CFG$stepSize),
                    -(1:Nadj) %x% diag(PARAMS_CFG$stepSize))
    mNrows = NPARAMS*Nadj*2 # Size fo the m matrix below
    wantPrecisionMax = max(WANTPRECISION)
    
    # Round iniVal to desired precision
    iniVal <- map2_dbl(iniVal, WANTPRECISION, function(i, w) {
      round(i, digits=w)    })
    nextRows <- matrix(iniVal, nrow=mNrows, ncol=NPARAMS, byrow=TRUE,
                       dimnames=list(c(),
                                     PARAMS_CFG$name)) %>%
      {(.) + StepsM} %>%
      # Best way to convert a matrix to a list
      as_tibble() %>%
      # DT requires a list
      {Grid[as.list(.), , nomatch=NULL, which=TRUE]} %>%
      unique(.)
    
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                     substr(Sys.time(), start=15, stop=16),
                     ". nextRows contains", NROW(nextRows), " rows"))
    
    
  }  else if (is.data.table(iniVal)) {
    
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                     substr(Sys.time(), start=15, stop=16),
                     "iniVal is a data table."))
    
    nextRows <- Grid[iniVal, nomatch=NULL, which=TRUE] %>%
      unique(.)
    rm(iniVal)
    
  } else {
    
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                     substr(Sys.time(), start=15, stop=16),
                     "ERROR. iniVal has to be double or a data table"))
    
    stop("iniVal has to be double (a point) or a tibble with candidate points.")
  }
  if (!is.data.table(Grid)) { stop('Object Grid does not exist or it is not a data.table')}
  
  # Start explorer
  log_print("Calling garbage collector before repeat to start exploration")
  gc() %>% log_print()

  repeat {
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                 substr(Sys.time(), start=15, stop=16),
                 "(top of repeat loop). Starting an exploration"))
    # Store data on perfomance
    PerfExp <- tibble('time' = Sys.time(),
                      'length_minutes' = NA,
                      'npoints' = NA,
                      'minutes_per_1000points' = NA)
    
    # Compute test values for each row in nextRows. For debugging purposes you DON'T want "future_"
    nRows = NROW(nextRows)
    if (nRows > 2e6) {
      
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                   substr(Sys.time(), start=15, stop=16),
                   "hs. There are ", (nRows) , " rows to explore. I split it in baches of 200,000 rows."))
      
      width = 200000
      nIter = floor((nRows) / width)
      pos <- cbind('start' =  (0:(nIter-1))*width + (0:(nIter-1) +1),
                   'end' = (0:(nIter-1) +1)*width + (0:(nIter-1) +1))
      if (pos[nIter, 'end'] < nRows) {
        pos <- rbind(pos,
                     c('start'= pos[nIter, 'end'] + 1, 'end'=  nRows))
      } else if(pos[nIter, 'end'] > nRows) {
        pos[nIter, 'end'] = nRows
      }
    } else {
      pos <- cbind('start' =  1,
                   'end' = nRows)
    }
    
    # ITERATION # : START : END
    # 1 : 1 : 3800967 - DONE
    # 2 : 3800968 : 2*3800967
    for (i in 1:NROW(pos)){
      sptS = pos[i, 'start']
      sptE = pos[i, 'end']
      
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                   substr(Sys.time(), start=15, stop=16),
                   "hs. (inside for loop). Exploring ", (sptE-sptS), " points. The first point is [", 
                   paste(Grid[nextRows[sptS], 1:NPARAMS], collapse=', '),
                   "]. The last point is [",
                   paste(Grid[nextRows[sptE], 1:NPARAMS], collapse=', '),
                   "]."))
    
      testValues <- as.data.frame(t(future_apply(Grid[nextRows[sptS:sptE], 1:NPARAMS], 1, comp_tests, VffLags = NULL)))
    
      #as.data.frame(t(future_apply(Grid[nextRows[sptS:sptE], 1:NPARAMS], 1, comp_tests, VffLags = NULL,
      #                             future.globals = c() )))
      #future({comp_tests(as.vector(Grid[1, 1:NPARAMS]), VffLags=NULL)})
      #testValues <- future_pmap_dfr(Grid[nextRows[sptS:sptE], 1:NPARAMS], function(...) { 
       # comp_tests(c(...), VffLags=thisVffLags)
      #}) 
      
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                   substr(Sys.time(), start=15, stop=16),
                   "hs. (inside for loop). Finished computing test values for this batch. Now about to update the Grid"))
      
      # Modification by reference (make sure setNumericRounding(2))
      Grid[nextRows[sptS:sptE],  (ESTVALUESNAMES) := testValues]
      
      #Grid[nextRows[sptS:sptE], (ESTVALUESNAMES) := testValues]
      if(.Last.updated != NROW(testValues)) { 
        stop(message='Possible loss of information. Not all values computed in testValues are in the Grid.')
      }
      
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                   substr(Sys.time(), start=15, stop=16),
                   "hs. (inside for loop). Saving explored rows to file."))
      
      saveRDS(Grid[nextRows[sptS:sptE]], 
              file=here(str_c(OUTPATH, 'Explored_recovery_', datetime_Stamp(), '.rds')))
      
      rm(testValues)
      log_print("Calling garbage collector after finishing this batch in the 
                for loop ")
      gc() %>% log_print()
      
    } # close for loop
  
    # COMPUTE FURTHER VALUES TO EXPLORE
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":", 
                 substr(Sys.time(), start=15, stop=16),
                 "hs. (inside repeat loop). Computing further values to explore."))
    # TODO: this search step requires the names of the parameters. 
    # TODO: When data.table v is available you can improve the beginnig of this query by only requestin the columns you need (STCPARAMNAMES and S):
    # Grid[nextRows, j, env=list(j = as.list(c(STCPARAMNAMES, 'S'))), verbose=TRUE]
    
    nextRows <- Grid[nextRows, ][S<SCRITICAL, ][, 
                                                list('rhoR' = c(rhoR+sS['rhoR'],		rhoR,             rhoR,           rhoR,               rhoR,           rhoR,  rhoR-sS['rhoR'],		rhoR,             rhoR,           rhoR,               rhoR,           rhoR),                      
                                                     'phiPi' = c(phiPi,               phiPi+sS['phiPi'],	phiPi,          phiPi,              phiPi,          phiPi,	phiPi,               phiPi-sS['phiPi'],	phiPi,          phiPi,              phiPi,          phiPi),                   
                                                     'phiX' = c(phiX,                phiX,             phiX+sS['phiX'],	phiX,               phiX,           phiX,	phiX,                phiX,             phiX-sS['phiX'],	phiX,               phiX,           phiX),                       
                                                     'rhoTao' = c(rhoTao,              rhoTao,           rhoTao,         rhoTao+sS['rhoTao'],	rhoTao,         rhoTao, rhoTao,              rhoTao,           rhoTao,         rhoTao-sS['rhoTao'],	rhoTao,         rhoTao),                    
                                                     'psiB' = c(psiB,                psiB,             psiB,           psiB,               psiB+sS['psiB'],	psiB, 	psiB,                psiB,             psiB,           psiB,               psiB-sS['psiB'],	psiB),                        
                                                     'psiX' = c(psiX,                psiX,             psiX,           psiX,               psiX,           psiX+sS['psiX'], psiX,                psiX,             psiX,           psiX,               psiX,           psiX-sS['psiX']))] %>%
      unique(.) %>%
      {Grid[., list(S, ROW_NUMBER), on=c('rhoR', 'phiPi', 'phiX', 'rhoTao', 'psiB', 'psiX'), nomatch=NULL][
        is.na(S), list(ROW_NUMBER)]$ROW_NUMBER}
    
    log_print("Calling garbage collector after computing nextRows")
    gc() %>% log_print()
    
    # Update perfomance data and print it to log
    PerfExp <- add_row(PerfExp, 
                       tibble('time' = Sys.time(),
                              'length_minutes' = as.double(difftime(Sys.time(),PerfExp[[nrow(PerfExp), 'time']], units='mins')),
                              'npoints' = sptE-sptS,
                              'points_per_minute' = npoints / length_minutes))
    log_print(str_c("nextRows has ", NROW(nextRows), " after unique()"))
    log_print(PerfExp)
    
    
    # nextRows can be empty after the second set of filters
    if (NROW(nextRows) == 0) { 
      break() 
    }
  }   # close repeat 

  log_print('Perseverance has landed.')
  
  c('Perseverance has landed.')
  
} # close fcn rover_Perseverance

# Mars Global Surveyor
# Builds a grid and explores the whole parameter space (a global survey)
Stage2_rover_MGS <- function() {
  
  tic()
  
  Grid <- build_grid_p23(PARAMS_CFG, 
                        origin=PARAMS_CFG$INIVAL_STAGE2,
                        startValue=0)
  
  plan(multisession, workers=NCORES)
  
  theExp <- future_pmap_dfr(Grid, function(...) { 
    comp_tests(c(...), VffLAGS=3)
    })
  
  Exp <- bind_cols(Grid, theExp)
  
  timer <- toc()
  teleM <- tibble('ELAPSED_SECONDS' = timer$toc - timer$tic,
              'CORES' = NCORES)
  
  list('Exp'=Exp, 'teleM'=teleM)
  

} # </rover_MGS>


# DEBUG and other functions ----

# Used to print a plot during the search in compS. It used for debugging or showcasing purposes
print_plot_iteration <- function(explored, Grid, SCRITICAL) {
  exploredClean <- filter(explored, !if_all(.fns=is.na)) %>%
    round(., digits=3)
  pData <- left_join(Grid, exploredClean, by=c('beta', 'gamma')) %>%
    mutate('Colour' = case_when(
      is.na(S) ~ 'black',
      S<SCRITICAL ~ 'yellow',
      TRUE ~ 'orange'
    ))
  
  # All points in exploredCleam should have a match in pData, this should be empty
  #anti_join(exploredClean, pData,  by=c('beta', 'gamma'))
  # This works!
  table <- tibble('Iteration #' = i,
                  '# of explored points' = sum(!is.na(explored$S)),
                  '# of points to explore' = NROW(nextThetas))
  
  pTable <- gridExtra::tableGrob(table, rows=NULL)
  
  #                    theme=ttheme_default(base_size=8, padding=unit(c(0.1,0.1), 'mm'),
  #                                        core=list(fg_params=list(hjust=0, x=0))))
  
  pGrid <- ggplot2::ggplot(pData, mapping = aes(x=`beta`, y=`gamma`, fill=`Colour`)) + 
    labs(title= 'Confidence set', 
         subtitle=paste0('Dimension parameter space: ', NROW(Grid) ), 
         caption='') +
    theme(panel.grid=element_line(colour='#999999', linetype='14'), 
          panel.grid.minor=element_line(colour='white'), 
          text=element_text(size=20), 
          axis.title.x = element_text(colour='red'),
          axis.title.y = element_text(colour='blue'),
          axis.text.x = element_text(angle=90, vjust=0.5)) + 
    scale_fill_identity() + 
    scale_x_continuous(name=latex2exp::TeX(r'($\beta$)'), limits=c(Grid[[1, 1]], Grid[[NROW(Grid), 1]] )) + 
    #breaks=seq(-1.5, 1.5, by=0.20)) +
    scale_y_continuous(name=latex2exp::TeX(r'($\gamma$)'), limits=(c(Grid[[1, 2]], Grid[[NROW(Grid), NCOL(Grid)]] ))) +
    #breaks=seq(-1.5, 1.5, by=0.20)) + 
    #geom_raster(colour='white', na.rm=TRUE)
    geom_tile(colour='white', na.rm=TRUE)
  
  #plots = marrangeGrob(list(pGrid, pTable), ncol=1, nrow=2)
  plots <- cowplot::plot_grid(pGrid, pTable,
                              nrow=2, rel_heights = c(9/10, 1/10))
  ggsave(paste0('CS_', i, '.pdf'),
         plot=plots, width=18, height=18, units='cm')
  
  plots
  
}


