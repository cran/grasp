"grasp.mod.anova" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.mod.anova", "\n")
cat("# (by A. Lehmann", "\n")
cat("# calculates the ANOVA table of a selected model by droping each term one after the other form the full model","\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: Anova table cannot be calculated on EMPTY models","\n")
      cat("\n")
      sink()
      return("")
}
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
if(length(grep("quasi", as.character(ModelYi$call))) > 0)
      QUASI <- TRUE
else QUASI <- FALSE
cat("\n")
cat(" vvvvvvvvvv GRASP ANOVA vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("Selected Model for: ", Yname, "\n")
print(ModelYi$call)
cat("Null Deviance: ", zapsmall(ModelYi$null.deviance), "\n")
cat("Residual Deviance: ", zapsmall(ModelYi$deviance), "\n")
cat("Residual Df: ", zapsmall(ModelYi$df.residual), "\n")
cat("D2: ", zapsmall((ModelYi$null.deviance - ModelYi$deviance)/
ModelYi$null.deviance), "\n")
cat("", "\n")
# test if contribution the contribution data frame already exist
termtemp <- predict.gam(ModelYi, XXX[1:10,  ], se.fit = FALSE, type = "terms")
tempterms <- dimnames(termtemp)[[2]]
anovarow <- NULL
for(eachterm in tempterms) {
      testedmodel <- update(ModelYi, eval(paste("~.-", eachterm)))
      if(QUASI) {
            thenanova <- anova(ModelYi, testedmodel, test = "F")
            newrow <- (cbind("Droping"=paste("-",eachterm),(as.data.frame((thenanova[2,])))))
      }
      else {
            thenanova <- anova(ModelYi, testedmodel, test = "Chi")
            newrow <- (cbind("Droping"=paste("-",eachterm),(as.data.frame((thenanova[2,])))))
      }
      anovarow <- rbind(anovarow, newrow)
}
#anovarow$TERMS<-tempterms
print(anovarow)
cat("\n")
cat(" ********** GRASP ANOVA END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

