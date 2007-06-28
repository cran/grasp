"grasp.step.crossplot" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if (!OPTIONS$PNG) get(getOption("device"))()
else {png(filename = paste("pred",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE save results in a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.int.crossselplot", "\n")
cat("# (by A. Lehmann)", "\n")
cat("# Plot of cross selection statistics", "\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP CROSS PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
plotcross <- as.data.frame(cbind(step = 1:(sum(!is.na(PLOTCROSS.VAL[, gr.Yi]))), val = PLOTCROSS.VAL[!is.na(PLOTCROSS.VAL[, gr.Yi]), gr.Yi], cval = PLOTCROSS.CVAL[!is.na(PLOTCROSS.CVAL[, gr.Yi]), gr.Yi]))
plot(plotcross$step, plotcross$val, ylim = c(0, 1), ylab = "statistics", xlab = "steps", pch = " ")
lines(plotcross$step, plotcross$val, lty = 4, col = 2, lwd = 2)
lines(plotcross$step, plotcross$cval, lty = 3, col = 3, lwd = 2)
legend(1,0.15,legend = "validation",col=2,bty="n",lty=4, , lwd = 2)
legend(1,0.1,legend =  "cross validation",col=3,bty="n",lty=3, lwd = 2)
legend(1,0.05,legend =  "best model",col=1,bty="n", lty=2, lwd = 3)
nbest <- plotcross$step[plotcross$cval == max(plotcross$cval)]
bestval <- zapsmall(plotcross$val[nbest], 3)
bestcval <- zapsmall(plotcross$cval[nbest], 3)
abline(v = nbest, lty = 2, lwd = 3)
title(paste(Yname, ", val:", bestval, "cval:", bestcval))
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP CROSS PLOT END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

