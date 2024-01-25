# GENERATE OBJECTS ----

# This function create a series of objects that are shared by all search functions.
# The output of this fcn is placed in the global environment. 
# Y  and Z should already exist

## Verify global objects ----
NPARAMS = NROW(PARAMS_CFG)
if (!exists('NEQS', mode='numeric')) {
  stop('Howe many equatiosn?') }

if (!exists('Y', mode='numeric')) {
  stop('Objects Y and X should exist before calling efisegri_ND_fcns.R') }
# Dimensions
if (!(NROW(Z)==NROW(Y))) {
  stop('The number of rows of X, Y and Z must coincide')
}
if (exists('X', mode='numeric')) {
  if (!(NROW(X) == NROW(Y) & (NROW(Z)==NROW(X)))) {
    stop('Nomber of rows of X must coincide with Z and Y')
  }
}
if (!(NROW(Z) == NEQS*T)) {
  stop('The number of rows of Z does not coincide with the number of moment equations and observations.')
}
KZ = NCOL(Z)
# Order condition
if(KZ < NPARAMS) {
  stop('THere are less instruments than parameters. Order condition not satisfied')
}

# Initial checks
if(anyNA(GET_B(PARAMS_CFG$iniVal))) {
  stop(paste0("get_b(THETA0) returns NAs in comp_S().  Make sure your GET_B() fcn does what it meant. THETA0 is:", PARAMS_CFG$iniVal))}

## Verify MODELEQS ----
walk(MODELEQS, function(thisEq) {
  if (!exists('Y', mode='numeric',  where=thisEq)) {
    stop('Objects Y and X should exist before calling efisegri_ND_fcns.R') }
  # Dimensions
  if (!(NROW(thisEq$Z)==NROW(thisEq$Y))) {
    stop('The number of rows of X, Y and Z must coincide')
  }
  if (exists('X', mode='numeric', where=thisEq)) {
    if (!(NROW(thisEq$X) == NROW(thisEq$Y) & (NROW(thisEq$Z)==NROW(thisEq$X)))) {
      stop('Nomber of rows of X must coincide with Z and Y')
    }
  }
  if (!(NROW(thisEq$Z) == T)) {
    stop('The number of rows of Z does not coincide with the number of moment equations and observations.')
  }
}) # Verify MODELEQS


## Generate global objects ----
# Other useful matrices
I_T = diag(NROW(X))
# X may not exist
if (exists('X', mode='numeric')) {
  Xtrans = t(X)
  Mx = I_T - X %*% solve(t(X) %*% X) %*% t(X)
  KX = NCOL(X)
  KZMKX = KZ - KX
  SdF <- (NCOL(Z)-NCOL(X))
} else {
  Mx = I_T
  KX = 0
  KZMKX = KZ - KX
  Sdf <- NCOL(Z)
}
Ztrans = t(Z)

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




