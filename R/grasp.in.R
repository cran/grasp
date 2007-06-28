"grasp.in" <-
function(Ymat = YYY, Xmat = XXX, Xpred = XXXpred, Xlut = NULL)
{
cat("#", "\n")
cat("# FUNCTION: grasp.in ", "\n")
cat("# (by A. Lehmann )", "\n")
cat("# initializes the GRASP objects", "\n")
cat("#", "\n")
# REQUIRED LIBRARIES
require(gam)
library(mda)

if(!is.null(Ymat)) {
      assign("YYY", Ymat, env = .GlobalEnv)
      cat("creates YYY", "\n")
      # matrix of dependent variables Ys with column names
      MODELCALLS <- list()
      for(yi in 1:length(names(YYY)))
            MODELCALLS[[names(YYY)[yi]]] <- names(YYY)[yi]
            # list of GAM models
            assign("MODELCALLS", MODELCALLS, env = .GlobalEnv)
            gr.modmask.local <- matrix(rep(TRUE, dim(YYY)[1] * dim(YYY)[2]), ncol = dim(YYY)[2])
            cat("creates gr.modmask", "\n")
            assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
}

# e.g. matrix of independant variables to model from (continues or as factors) with names for each column
if(!is.null(Xlut)) {
      assign("XXXlut", Xlut, env = .GlobalEnv)
}
assign("XXX", Xmat, env = .GlobalEnv)
cat("creates XXX", "\n")

# e.g. matrix of independant variables to predict from (same variable names as Xmat)
assign("XXXpred", Xpred, env = .GlobalEnv)
cat("creates XXXpred", "\n")

# matrix of independent variables Xs used for predictions with column names identic to Xmat
gr.predmat <- as.data.frame(matrix(as.single(rep(-99.9, dim(XXXpred)[1] * dim(YYY)[2])), ncol = dim(YYY)[2]))
cat("creates gr.predmat", "\n")
assign("gr.predmat", gr.predmat, env = .GlobalEnv)
gr.predmask.local <- matrix(rep(TRUE, dim(XXXpred)[1] * dim(YYY)[2]), ncol = dim(YYY)[2])
cat("creates gr.predmask", "\n")
assign("gr.predmask", gr.predmask.local, env = .GlobalEnv)
XXX.local <- XXX
XXXpred.local <- XXXpred
assign("XXX", XXX.local, env = .GlobalEnv)
assign("XXXpred", XXXpred.local, env = .GlobalEnv)
assign("gr.selX", 2, env = .GlobalEnv)
# remove contribution tables previously used 
if (exists("DROP.CONTRIB")) rm(DROP.CONTRIB, envir = .GlobalEnv)
if (exists("ALONE.CONTRIB")) rm(ALONE.CONTRIB, envir = .GlobalEnv)
if (exists("MODEL.CONTRIB")) rm(MODEL.CONTRIB, envir = .GlobalEnv)
if (exists("gr.lastcontrib1")) rm(gr.lastcontrib1, envir = .GlobalEnv)
if (exists("gr.lastcontrib2")) rm(gr.lastcontrib2, envir = .GlobalEnv)
if (exists("gr.lastcontrib3")) rm(gr.lastcontrib3, envir = .GlobalEnv)
if (exists("contribution1")) rm(contribution1, envir = .GlobalEnv)
if (exists("contribution2")) rm(contribution2, envir = .GlobalEnv)
if (exists("contribution3")) rm(contribution3, envir = .GlobalEnv)

cat("INFO: old contributions removed", "\n")
if(exists("VALIDATION")) {
      # remove contribution tables previously used 
      rm(VALIDATION, envir = .GlobalEnv)
      cat("INFO: old VALIDATION removed", "\n")
}
if(exists("OPTIONS")) {
      # remove option list previously used 
      rm(OPTIONS, envir = .GlobalEnv)
      cat("INFO: old OPTIONS removed", "\n")
}
grasp.options()
if(exists("WEIGHTS")) {
      rm(WEIGHTS, envir = .GlobalEnv)
      cat("INFO: old WEIGHTS removed", "\n")
}
WEIGHTS.local <- as.data.frame(matrix(rep(1, dim(YYY)[1] * dim(YYY)[2]), ncol = dim(YYY)[2]))
assign("WEIGHTS", WEIGHTS.local, env = .GlobalEnv)
cat("INFO: old datafilter removed", "\n")
assign("DATAFILTER", gr.modmask.local, env = .GlobalEnv)
SELXCOR <- NULL
cat("**********************************************************************", "\n")
cat("********** REUSE GRASP.IN EACH TIME YOU MODIFY YOUR DATA *************", "\n")
cat("**********************************************************************", "\n")
"INPUT OK"
}

