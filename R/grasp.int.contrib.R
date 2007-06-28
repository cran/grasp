"grasp.int.contrib" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.int.contrib", "\n")
cat("# (by A. Lehmann", "\n")
cat("# calculates the contributions (DROP, MODEL and ALONE) of the selected variables","\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: contribution cannot be calculated on EMPTY models","\n")
      cat("\n")
      sink()
      return("")
}
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
cat("\n")
cat(" vvvvvvvvvv GRASP CONTRIBUTION vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
# test if contribution the contribution data frame already exist
if(!exists("ALONE.CONTRIB")) {
      # tests whether a contribution matrix has already been calculated 
      cat("INFO: new data", "\n")
      contribution1 <- YYY[1,  ]
      # will contain DROP contributions
      contribution2 <- YYY[1,  ]
      # will contain MODEL contributions
      contribution3 <- YYY[1,  ]
}
else {
      # reuse existing contributions
      contribution1 <- ALONE.CONTRIB
      contribution2 <- DROP.CONTRIB
      contribution3 <- MODEL.CONTRIB
}
contribution1[, gr.Yi] <- NA
contribution2[, gr.Yi] <- NA
contribution3[, gr.Yi] <- NA
count <- 0
# Starts a loop among the selected variables to calculate both DROP and ALONE contributions
SUBSET <- paste("as.logical(gr.modmask[,", gr.Yi, "])")
countX <- 1
term.matrix <- predict.gam(ModelYi, XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
cat(mod.terms, "\n")
cat("PREDICTORS: ")
for(Xiterm in mod.terms) {
      cat(Xiterm, " / ")
      submodel1 <- update(ModelYi, eval(paste("~ . -", Xiterm)))
      submodel2 <- update(ModelYi, formula = eval(parse(text = paste("YYY[,gr.Yi]~", Xiterm))))
      if(length(grep("quasi", as.character(ModelYi$call))) > 0){
            # if scaled (useful for over dispersed models using a quasi family)
            nullmodel <- update(ModelYi, formula = eval(parse(
            text = paste("YYY[,gr.Yi]~", 1))))      
            contribution1[Xiterm, gr.Yi] <- anova(submodel1, ModelYi, test = "F")[2, "Deviance"]
            contribution2[Xiterm, gr.Yi] <- anova(nullmodel, submodel2, test = "F")[2, "Deviance"]
      }
      else {
            contribution1[Xiterm, gr.Yi] <- submodel1$deviance -ModelYi$deviance
            contribution2[Xiterm, gr.Yi] <- submodel2$null.deviance - submodel2$deviance
      }
      contribution3[Xiterm, gr.Yi] <- as.vector(apply(predict.gam(
      ModelYi, type = "terms"), 2, max) - apply(predict.gam(
      ModelYi, type = "terms"), 2, min))[countX]
      countX <- countX + 1
}
# for
cat("\n\n")
VARcontrib <- cbind(contribution1[, gr.Yi], contribution2[, gr.Yi],
contribution3[, gr.Yi])
row.names(VARcontrib) <- row.names(contribution1)
names(VARcontrib) <- c("drop", "model", "alone")
print(VARcontrib[mod.terms,  ])
assign("ALONE.CONTRIB", contribution2, env = .GlobalEnv)
assign("DROP.CONTRIB", contribution1, env = .GlobalEnv)
assign("MODEL.CONTRIB", contribution3, env = .GlobalEnv)
cat("\n")
cat(" ********** GRASP CONTRIBUTION END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

