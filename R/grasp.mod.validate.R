"grasp.mod.validate" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]

if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("valid",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE saves results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.mod.validate", "\n")
cat("# (by A. Lehmann)", "\n")
cat("# calculates and plots validation and cross-validation statistics","\n")
cat("#", "\n")
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: validation cannot be calculated on EMPTY models","\n")
      sink()
      return("")
}
OPTIONS.local <- OPTIONS
SUBSETS <- NULL
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
if(exists("VALIDATION"))
      validation <- VALIDATION
else validation <- YYY[1,  ]
cat("\n")
cat(" vvvvvvvvvv GRASP VALIDATE vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
par(mfrow = c(2, 2))
validation[, gr.Yi] <- 0
# GENERAL STATISTICS
validation["null.dev", gr.Yi] <- ModelYi$null.deviance
# Total deviance
validation["resid.dev", gr.Yi] <- ModelYi$deviance
# Residual deviance
validation["D2", gr.Yi] <- (ModelYi$null.deviance - ModelYi$deviance)/
ModelYi$null.deviance
# Squared deviance: D2
validation["df.residual", gr.Yi] <- ModelYi$df.residual
# number of DF used
validation["n", gr.Yi] <- length(gr.modmask[gr.modmask[, gr.Yi], gr.Yi])
# number of observations
# CROSS VALIDATION
#
subsets <- rep(1:OPTIONS$CVGROUPS, length.out = length(XXX[, 1]))
# subsets <- round(runif(length(XXX[, 1]), 0.5, OPTIONS$CVGROUPS + 0.5))
if(!is.null(OPTIONS$JAKESUB)) {
      subsets <- OPTIONS$JAKESUB
      OPTIONS.local$CVGROUPS <- length(unique(OPTIONS$JAKESUB))
      assign("OPTIONS", OPTIONS.local, env = .GlobalEnv)
}
# assign grouping according to some expert decision making 
temppredict <- rep(NA, length(XXX[, 1]))
# initializes a prediction vector
# generates random integer values bewtween 1 and the number of desired groups
for(Nloop in 1:OPTIONS$CVGROUPS) {
      # loops through the number of groups
      SUBSETS$sub1 <- subsets != Nloop
      # First subset with group number = Nloop
      if(OPTIONS$CVGROUPS == 1) 
            SUBSETS$sub1 <- !SUBSETS$sub1
      # tests if there is only one group (special case)
      # reverses subset 1
      SUBSETS$sub1 <- (as.logical(gr.modmask[, gr.Yi]) & SUBSETS$sub1)
      # to take gr.modmask into account 
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      tempmodel <- update(ModelYi, subset = SUBSETS$sub1)
      # calculates the model without the Nloop group
      if(OPTIONS$CVGROUPS == 1) 
            SUBSETS$sub2 <- SUBSETS$sub1 
      else 
            SUBSETS$sub2 <- (as.logical(gr.modmask[, gr.Yi]) & !SUBSETS$sub1)
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      temppredict[SUBSETS$sub2] <- predict.gam(tempmodel, XXX[SUBSETS$sub2,  ], type = "response")
}
temp1 <- YYY[!is.na(temppredict), gr.Yi]
temp2 <- temppredict[!is.na(temppredict)]
plot(temp1, temp2, ylab = "cross-predicted", xlab = paste("observed ",Yname), pch = "*")
abline(lsfit(temp1, temp2))
# plots the total result
notNA <- !is.na(YYY[, gr.Yi])
BIN <- all(YYY[notNA, gr.Yi] == 0 | YYY[notNA, gr.Yi] == 1)
if(BIN) {
      nbgroups <- 10
      cvROC <- zapsmall(grasp.step.rocplot(temp2, temp1), 3)
      cvCOR <- zapsmall(cor(temp1, temp2), 3)
      cat("cv ROC auc: ", cvROC, "\n")
      cat("cv COR: ", cvCOR, "\n")
      legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2),paste("N : ", OPTIONS$CVGROUPS, ", cvCOR=", cvCOR),cex = 0.7)
      title("CROSS-VALIDATION", cex = 0.5)
      #plot AUC 
      plot(1 - gr.EVA.ROC[, "sp"], gr.EVA.ROC[, "se"], xlab = "1-specificity", ylab = "sensitivity")
      lines(1 - gr.EVA.ROC[, "sp"], gr.EVA.ROC[, "se"])
      abline(0, 1)
      legend(0.4, 0.2, paste("N : ",OPTIONS$CVGROUPS, ", cvROC=", cvROC), cex = 0.7)
      title("CROSS-VALIDATION", cex = 0.5)
}
else {
      print(paste("cor:", cor(temp1, temp2)))
      cvCOR <- zapsmall(cor(temp1, temp2), 3)
      legend(0.2*max(temp1), 0.2*max(temp2), paste("N groups : ", OPTIONS$CVGROUPS, ", cvCOR =",cvCOR), cex = 0.7)
      title("CROSS-VALIDATION", cex = 0.5)
}
# VALIDATION
# 
CVGROUPS <- 1
subsets <- rep(1, length.out = length(XXX[, 1]))
temppredict <- rep(NA, length(XXX[, 1]))
# initializes a prediction vector
# generates random integer values bewtween 1 and the number of desired groups
for(Nloop in 1:CVGROUPS) {
      # loops through the number of groups
      SUBSETS$sub1 <- subsets != Nloop
      # First subset with group number = Nloop
      if(CVGROUPS == 1) SUBSETS$sub1 <- !SUBSETS$sub1
      # tests if there is only one group (special case)
      # reverses subset 1
      SUBSETS$sub1 <- (as.logical(gr.modmask[, gr.Yi]) & SUBSETS$sub1)
      # to take gr.modmask into account 
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      tempmodel <- update(ModelYi, subset = SUBSETS$sub1)
      # calculates the model without the Nloop group
      if(CVGROUPS == 1) 
            SUBSETS$sub2 <- SUBSETS$sub1 
      else 
            SUBSETS$sub2 <- (as.logical(gr.modmask[, gr.Yi]) &!SUBSETS$sub1)
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      temppredict[SUBSETS$sub2] <- predict.gam(tempmodel, XXX[SUBSETS$sub2,  ], type = "response")
}      
temp1 <- YYY[!is.na(temppredict), gr.Yi]
temp2 <- temppredict[!is.na(temppredict)]
plot(temp1, temp2, ylab = "predicted", xlab = paste("observed ", Yname), pch = "*")
abline(lsfit(temp1, temp2))
# plots the total result
if(BIN) {
      nbgroups <- 10
      COR <- zapsmall(cor(temp1, temp2), 3)
      ROC <- zapsmall(grasp.step.rocplot(temp2, temp1), 3)
      cat("ROC auc: ", ROC, "\n")
      cat("COR: ", COR, "\n")
      legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2),paste("N : ", CVGROUPS, ", COR=", COR), cex = 0.7)
      validation["ROC", gr.Yi] <- ROC
      validation["cvROC", gr.Yi] <- cvROC
      validation["COR", gr.Yi] <- COR
      validation["cvCOR", gr.Yi] <- cvCOR
      title("VALIDATION", cex = 0.5)
      #plot AUC 
      plot(1 - gr.EVA.ROC[, "sp"], gr.EVA.ROC[, "se"], xlab = "1-specificity", ylab = "sensitivity")
      lines(1 - gr.EVA.ROC[, "sp"], gr.EVA.ROC[, "se"])
      abline(0, 1)
      legend(0.4, 0.2, paste("N : ",
      CVGROUPS, ", ROC=", ROC), cex = 0.7)
      title("VALIDATION", cex = 0.5)
}
else {
      COR <- zapsmall(cor(temp1, temp2), 3)
      legend(0.2*max(temp1), 0.2*max(temp2),
      paste("N groups : ", CVGROUPS, ", COR =", COR), cex = 0.7)
      validation["COR", gr.Yi] <- COR
      validation["cvCOR", gr.Yi] <- cvCOR
      title("VALIDATION", cex = 0.5)
}
if(!BIN) {
      qqnorm(resid(ModelYi), main="RESIDUALS: fitted values")
      # plots the residual from selected model 
}
assign("VALIDATION", validation, env = .GlobalEnv)
print(as.data.frame(VALIDATION[-1,  c(1,gr.Yi)]))
par(mfrow = c(1, 1))
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP VALIDATE END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

