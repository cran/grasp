"grasp.pred" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE results are saved to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.pred (by A. Lehmann using SPlus predict.gam function)","\n")
cat("# predicts the the response variable from a new dataframe containing the independant variables to predict from","\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: Predictions cannot be calculated on EMPTY models","\n")
      sink()
      return("")
}
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
#tests if extra variables must be included in XXX such as INTER, AUTOCOR and TREND
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
gr.predmat.local <- gr.predmat
cat("\n")
cat(" vvvvvvvvvv GRASP PREDICTION vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
print(ModelYi)
prediction <- rep(1, length(XXXpred[, 1]))
# initializes a prediction vector with 1 values
prediction[gr.predmask[, gr.Yi] == FALSE] <- -99.900000000000006
#  replace values out of the mask by -99.9
prediction[prediction == 1] <- predict.gam(ModelYi, XXXpred[prediction ==1,  ], type = "response")
# predicts only on values within the mask
gr.predmat.local[, gr.Yi] <- as.single(zapsmall(prediction, 4))
assign("gr.predmat", gr.predmat.local, env = .GlobalEnv)
# saves prediction in gr.predmat
cat("INFO: prediction saved into gr.predmat", "\n")
cat("\n")
cat(" ********** GRASP PREDICTION END ********** ", "\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

