"grasp.step" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE saves results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.step", "\n")
cat("# (by A. Lehmann and J.McC. Overton)", "\n")
cat("# selects a statistically significant model that explains a response variable by stepwise procedure","\n")
cat("# based either on AIC criteria or on ANOVA tests with Chisq or F tests","\n")
cat("#", "\n")
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
MODELCALLS.local <- MODELCALLS
# print(gr.modmask[1:10,  ])
gr.modmask.local <- gr.modmask
MODELCALLS.local[[gr.Yi]]$gamcall <- NULL
MODELCALLS.local[[gr.Yi]]$residualcall <- NULL
cat("\n")
cat(" vvvvvvvvvv GRASP STEP vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
notNA <- !is.na(YYY[, gr.Yi])
if((OPTIONS$FAM == "quasibinomial") & ((max(YYY[notNA, gr.Yi]) > 1) | (min(YYY[notNA, gr.Yi]) < 0))) {
      stop("DATA OUT OF RANGE [0,1] TO USE A BINOMIAL MODEL !")
}
mkt.keep.AIC <- function(object, AIC){
      # defines a function to keep results from AIC selection
      list(df.resid = object$df.resid, deviance = object$deviance,
      term = as.character(object$formula)[3], AIC = AIC)
}
mkt.keep.BIC <- function(object, BIC){
      # defines a function to keep results from BIC selection
      list(df.resid = object$df.resid, deviance = object$deviance,
      term = as.character(object$formula)[3], BIC = BIC)
}
mkt.keep.p <- function(object, pvalue){
      # defines a function to keep results from ANOVA selection
      list(df.resid = object$df.resid, deviance = object$deviance,
      term = as.character(object$formula)[3], pvalue = pvalue)
}
gr.modmask.local <- gr.modmask
for(xi in gr.selXCOR[[gr.Yi]])
      if(is.factor(XXX[, xi]) & length(levels(XXX[, xi])) < 2) {
            gr.selXCOR[[gr.Yi]] <- gr.selXCOR[[gr.Yi]][ - match(xi, gr.selXCOR[[gr.Yi]])]
            print(paste("factor", names(XXX)[xi], "was removed from the potential predictors of",names(YYY)[gr.Yi],  "because it has less than two levels"))
      }
# gr.selXCOR[[gr.Yi]] <- intersect(OPTIONS$STARTWITH, gr.selXCOR[[gr.Yi]])
STEPscope <- grasp.step.scope(gr.selXCOR[[gr.Yi]])
# generates gam scope with all variables
if(!(OPTIONS$TEST == "BRUTO")) {
      START <- grasp.step.start(gr.selXCOR[[gr.Yi]])
      # generates a starting formula with all variables
      SUBSET <- paste("gr.modmask[,", gr.Yi, "]")
      WEIGHTS.df <- paste("WEIGHTS[,", gr.Yi, "]")
      # uses gr.modmask
      # MODELCALLS.local[[gr.Yi]]$FAM_OPTIONS$FAM 
      gam.start <- eval(parse(text = paste("gam(", as.character(START[2]), "~", as.character(START[3]), ",data = XXX, family = ", OPTIONS$FAM, ",weights = ",WEIGHTS.df, ", bf.maxit = 1000,bf.epsilon=0.001,na.action = na.omit, subset = ",SUBSET, " )")))
      # caculate the starting model with all variables
      cat("####################################", "\n")
      cat("STEPWISE SELECTION:", "\n")
      cat("####################################", "\n")
      cat("DIRECTION: ", OPTIONS$DIRECTION, "\n")
      cat("", "\n")
      cat("*******************", "\n")
      cat("STARTING GAM MODEL:", "\n")
      cat("*******************", "\n")
      print(gam.start)
      cat("Null Deviance: ", zapsmall(gam.start$null.deviance),"\n")
      cat("D2: ", zapsmall((gam.start$null.deviance - gam.start$deviance)/gam.start$null.deviance), "\n")
}
cat("", "\n")
cat("####################################", "\n")
cat("", "\n")
if(OPTIONS$TEST == "BRUTO") {
      cat("####################################", "\n")
      cat("BRUTO SELECTION:", "\n")
      cat("####################################", "\n")
      # tests if BRUTO selection is to be used
      cat("BRUTO SELECTION:", "\n")
      ModelYi <- grasp.step.bruto(gr.Yi, gr.selXCOR[[gr.Yi]])
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}
if(OPTIONS$TEST == "CROSS") {
      # tests if CROSS VALIDATION selection is to be used
      cat("CROSS SELECTION:", "\n")
      ModelYi <- grasp.step.cross(gam.start, scope = STEPscope,
      keep = mkt.keep.BIC, trace = OPTIONS$SHOWDETAIL)
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}
if(OPTIONS$TEST == "AIC") {
      # tests if AIC stepwise method is to be used
      cat("AIC SELECTION:", "\n")
      ModelYi <- step.gam(gam.start, scope = STEPscope, keep= mkt.keep.AIC, trace = OPTIONS$SHOWDETAIL, direction= OPTIONS$DIRECTION)
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}
if(OPTIONS$TEST == "BIC") {
      cat("BIC SELECTION:", "\n")
      # tests if BIC stepwise method is to be used
      ModelYi <- grasp.step.bic(gam.start, scope = STEPscope, keep = mkt.keep.BIC, trace = OPTIONS$SHOWDETAIL, direction = OPTIONS$DIRECTION)
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}
if(OPTIONS$TEST == "Chisq" | OPTIONS$TEST == "F") {
      # tests if ANOVA stepwise method is to be used
      cat("CHI or F SELECTION:", "\n")
      ModelYi <- grasp.step.anova(gam.start, scope = STEPscope,keep = mkt.keep.p, trace = OPTIONS$SHOWDETAIL, direction = OPTIONS$DIRECTION)
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}

if(OPTIONS$PERCONT) {
      cat("CONTINUING WITH MINIMUM PERCENT CONTRIBUTION SELECTION:","\n")
      # tests if PERCONT stepwise method is to be used
      term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call),XXX[1:10,  ], se.fit = FALSE, type = "terms")
      mod.terms <- dimnames(term.matrix)[[2]]
      step.list <- as.list(mod.terms)
      names(step.list) <- mod.terms
      for(Xi in mod.terms) {
            junk <- c("1", Xi)
            junk <- eval(parse(text = paste("~", paste(junk, collapse = "+"))))
            step.list[[Xi]] <- junk
      }
      ContribScope <- step.list
      gam.start2 <- eval(ModelYi$call)
      ModelYi2 <- grasp.step.contrib(gam.start2, scope = 
      ContribScope, keep = mkt.keep.BIC, trace = OPTIONS$
      SHOWDETAIL, direction = "backward")
      MODELCALLS.local[[gr.Yi]]$call <- ModelYi2$call
      names(MODELCALLS.local)[gr.Yi] <- names(YYY)[gr.Yi]
}
cat("", "\n")
cat("######## SELECTION SUMMARY #########", "\n")
print(ModelYi$anova)
cat("####################################", "\n")
cat("", "\n")
cat("", "\n")
cat("*******************", "\n")
cat("SELECTED GAM MODEL:", "\n")
cat("*******************", "\n")
print(MODELCALLS.local[[gr.Yi]]$call)
cat("", "\n")
cat("Distribution family: ", OPTIONS$FAM, "\n")
cat("Selection method: ", OPTIONS$TEST, "\n")
if(OPTIONS$TEST == "F" | OPTIONS$TEST == "Chisq")
      cat("p-value: ", OPTIONS$P.limit, "\n")
if(OPTIONS$PERCONT)
      cat("Minimum Model Contribution (%): ", OPTIONS$MINCONTRIB,"\n")
cat("Null Deviance: ", zapsmall(ModelYi$null.deviance), "\n")
cat("Explained Deviance: ", zapsmall(ModelYi$null.deviance - ModelYi$deviance), "\n")
cat("D2: ", zapsmall((ModelYi$null.deviance - ModelYi$deviance)/ModelYi$null.deviance), "\n")
cat("COR: ", zapsmall(cor(ModelYi$y, ModelYi$fitted.values)), "\n")
if(!OPTIONS$RESETLIM) {
      cat("Limits on: ", names(XXX)[OPTIONS$SELXLIM], "\n")
}
if(!OPTIONS$RESETCOR) {
      cat("Max Correlation btw predicotrs: ", OPTIONS$CORLIM, "\n")
}
if(OPTIONS$FAM == "binomial") {
      cat("Number of presences: ", sum(YYY[gr.modmask[, gr.Yi],gr.Yi] == 1), "\n")
      cat("Number of absences: ", sum(YYY[gr.modmask[, gr.Yi], gr.Yi] == 0), "\n")
      cat("Prevalence: ", sum(YYY[gr.modmask[, gr.Yi], gr.Yi] == 1)/sum(gr.modmask[, gr.Yi]), "\n")
}
if(OPTIONS$RECALCULATEWEIGHTS) {
      cat("Weights of presences: ", WEIGHTS[YYY[, gr.Yi] == 1, gr.Yi][1], "\n")
      cat("Weights of absences: ", WEIGHTS[YYY[, gr.Yi] == 0, gr.Yi][1], "\n")
      cat("Weighted prevalence: ", zapsmall(sum(WEIGHTS[YYY[, gr.Yi] ==1 & gr.modmask[, gr.Yi], gr.Yi])/(sum(WEIGHTS[gr.modmask[, gr.Yi], gr.Yi])), 2), "\n")
}
cat("", "\n")
MODELCALLS.local[[gr.Yi]]$gamcall <- MODELCALLS.local[[gr.Yi]]$call
assign("MODELCALLS", MODELCALLS.local, env = .GlobalEnv)
cat("INFO: !!! Model formula saved in MODELCALLS[[", gr.Yi, "]]",
sep = "", "\n")
cat("\n")
cat(" ********** GRASP STEP END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
return()
}

