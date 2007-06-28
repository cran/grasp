"grasp.exp.RvsP" <-
function(gr.Yi, sX = gr.selX)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.RvsP", "\n")
cat("# (by A. Lehmann, requested by M. Ausin", "\n")
cat("# calculates the distribution of the response variable (RV) on the histograms of Predictor Variables","\n")
cat("#", "\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP RESPONSE VS PREDICTORS vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
par(mfrow = OPTIONS$PLOTPAR)
first <- TRUE
for(xi in sX) {
      plot(XXX[gr.modmask[, gr.Yi], xi], YYY[gr.modmask[, gr.Yi],gr.Yi], xlab = names(XXX)[xi], ylab = names(YYY)[gr.Yi], col=OPTIONS$abscol)
      points(XXX[gr.modmask[, gr.Yi] & YYY[,gr.Yi]>0, xi], YYY[gr.modmask[, gr.Yi] & YYY[,gr.Yi]>0,gr.Yi],col=OPTIONS$prescol)
      if(!is.factor(XXX[, xi])) {
            selrows <- (!is.na(XXX[, xi]) & gr.modmask[, gr.Yi] &!is.na(YYY[, gr.Yi]))
            lines(smooth.spline(XXX[selrows, xi], YYY[selrows,gr.Yi], w = WEIGHTS[selrows, gr.Yi], df = OPTIONS$DF2[xi]))
      }
      if((min(WEIGHTS[, gr.Yi]) != 1) | (max(WEIGHTS[, gr.Yi]) !=1))
            weighted <- " (weighted)"
      else weighted <- ""
      if(first)
            title(paste(OPTIONS$TITLE, " ", names(YYY[gr.Yi]),weighted))
      first <- FALSE
}
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP RESPONSE VS PREDICTORS END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

