library(logr)
library(data.table)
library(future.apply)


logFileName <- paste0(OUTPATH, 'log_', datetime_Stamp())
log_open(logFileName, logdir=FALSE)
log_print(paste0("Log initialized. RESULTID is ", RESULTID))

# GENERATE OBJECTS ----

# This function create a series of objects that are shared by all search functions.
# The output of this fcn is placed in the global environment. 
# Y  and Z should already exist

## Verify global objects ----
NPARAMS = NROW(PARAMS_CFG)
T  = NROW(Y)
if (!exists('NEQS', mode='numeric')) {
  stop('How many equatiosn?') }

if (!exists('Y', mode='numeric')) {
  stop('Object Y should exist before calling efisegri_ND_fcns.R') }

# Requirements for the IV case
if (exists('X', mode='numeric')) {
  # Dimensions
  if (!(NROW(Z)==NROW(Y))) {
    stop('The number of rows of X, Y and Z must coincide')
  }
  if (!(NROW(X) == NROW(Y) & (NROW(Z)==NROW(X)))) {
    stop('Nomber of rows of X must coincide with Z and Y')
  }
  if (!(NROW(Z) == NEQS*T)) {
    stop('The number of rows of Z does not coincide with the number of moment equations and observations.')
  }
  KZ = NCOL(Z)
  # Order condition
  if(KZ < NPARAMS) {
    stop('THere are less instruments than parameters. Order condition not satisfied')
  }
} # IV case

# Initial checks
iniV <- PARAMS_CFG$iniVal
names(iniV) <- PARAMS_CFG$name
if(anyNA(GET_B(iniV))) {
  stop(paste0("get_b(THETA0) returns NAs in comp_S().  Make sure your GET_B() fcn does what it meant. THETA0 is:", PARAMS_CFG$iniVal))}

## Verify MODELEQS ----
walk(MODELEQS, function(thisEq) {
  if (!exists('Y', mode='numeric',  where=thisEq)) {
    stop('Objects Y should exist before calling efisegri_ND_fcns.R') }
  # oNLY NECESARY IF iv ESTIMATION
  if (exists('X', mode='numeric', where=thisEq)) {
    # Dimensions
    if (!(NROW(thisEq$Z)==NROW(thisEq$Y))) {
      stop('The number of rows of X, Y and Z must coincide')
    }
    if (!(NROW(thisEq$X) == NROW(thisEq$Y) & (NROW(thisEq$Z)==NROW(thisEq$X)))) {
      stop('Nomber of rows of X must coincide with Z and Y')
    }
    if (!(NROW(thisEq$Z) == T)) {
      stop('The number of rows of Z does not coincide with the number of moment equations and observations.')
    }
  }
}) # Verify MODELEQS


## Generate global objects ----

# X may not exist
if (exists('X', mode='numeric')) {
  I_T = diag(NROW(X))
  Xtrans = t(X)
  Mx = I_T - X %*% solve(t(X) %*% X) %*% t(X)
  KX = NCOL(X)
  KZMKX = KZ - KX
  SdF <- (NCOL(Z)-NCOL(X))
} else {
  Mx = diag(NROW(Y))
  KX = 0
  KZMKX = KZ - KX
  Sdf <- ifelse(exists('Z', mode='numeric'), NCOL(Z), 0)
  Ztrans = NA
}

# For Qll computation
ones_T = rep(1, times=T)
ones_Ttrans = t(ones_T)
M_ones_T = diag(T) - ones_T %*% solve((ones_Ttrans %*% ones_T)) %*% ones_Ttrans
# c10 = 10
r = 1-(10/T)

# Critical value for S statistic
# P(ChiSq>5.99) = 0.025 with 2 degrees of freedom 
SCRITICAL <- qchisq(SPVALUE, df=SdF)
# kzmkx = # of instruments (total, size of Z) - # of included instruments (size X)

# Desired level of precision for each parameter
WANTPRECISION = str_match(PARAMS_CFG$stepSize, "\\.(\\d+)")[, 2] %>%
  nchar(.)
# NAs occur if there is no decimal place in stepSize
WANTPRECISION[anyNA(WANTPRECISION)] = 0
names(WANTPRECISION) = PARAMS_CFG$name




