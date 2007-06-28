"grasp.step.full" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE saves results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.step.full", "\n")
cat("# (by A. Lehmann)", "\n")
cat("# build a full model that explains a response variable by all preselected predictors","\n")
cat("#", "\n")
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
MODELCALLS.local <- MODELCALLS
# print(gr.modmask[1:10,  ])
gr.modmask.local <- gr.modmask
MODELCALLS.local[[gr.Yi]]$gamcall <- NULL
MODELCALLS.local[[gr.Yi]]$residualcall <- NULL
cat("\n")
cat(" vvvvvvvvvv GRASP FULL MODEL vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
cat("####################################", "\n")
cat("FULL MODEL:", "\n")
cat("####################################", "\n")
gr.modmask.local <- gr.modmask
for(xi in gr.selXCOR[[gr.Yi]])
if(is.factor(XXX[, xi]) & length(levels(XXX[, xi])) < 2) {
      gr.selXCOR[[gr.Yi]] <- gr.selXCOR[[gr.Yi]][ - match(xi, gr.selXCOR[[gr.Yi]])]
      print(paste("factor", names(XXX)[xi], "was removed from the potential predictors of",names(YYY)[gr.Yi], "because it has less than two levels"))
}
START <- grasp.step.start(gr.selXCOR[[gr.Yi]])
# generates a starting formula with all variables
SUBSET <- paste("gr.modmask[,", gr.Yi, "]")
WEIGHTS.df <- paste("WEIGHTS[,", gr.Yi, "]")
gam.start <- eval(parse(text = paste("gam(", as.character(START[2]),"~", as.character(START[3]), ",data = XXX, family = ",OPTIONS$FAM, ",weights = ", WEIGHTS.df, ", bf.maxit = 1000,bf.epsilon=0.001,na.action = na.omit, subset = ",SUBSET, " )")))
# caculate the full model with all variables
print(gam.start)
cat("Null Deviance: ", zapsmall(gam.start$null.deviance), "\n")
cat("D2: ", zapsmall((gam.start$null.deviance - gam.start$deviance)/
gam.start$null.deviance), "\n")
cat("", "\n")
cat("####################################", "\n")
cat("", "\n")
MODELCALLS.local[[gr.Yi]]$call <- gam.start$call
names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
MODELCALLS.local[[gr.Yi]]$gamcall <- MODELCALLS.local[[gr.Yi]]$call
assign("MODELCALLS", MODELCALLS.local, env = .GlobalEnv)
cat("", "\n")
cat("INFO: !!! Model formula saved in MODELCALLS[[", gr.Yi, "]]",
sep = "", "\n")
cat("\n")
cat(" ********** GRASP FULL MODEL END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
      return()
}

