"grasp.exp.distry" <-
function(gr.Yi, nbar = OPTIONS$NBAR)
{
Yname <- names(YYY)[gr.Yi]
if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("distY",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.distry", "\n")
cat("# (by A. Lehmann)", "\n")
cat("# plot an histogram of the distribution of the Response Variable (RV)","\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv HISTOGRAM OF RESPONSE vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
tempy <- YYY[, gr.Yi]
if(OPTIONS$FAM != "binomial" & OPTIONS$FAM != "quasibinomial"){
      den.tempy <- density(tempy, width = 4, na.rm = TRUE)
      hist(tempy, ylab = "COUNT", xlab = Yname , xlim= c(min(tempy), max(tempy)), main=" ", col=OPTIONS$prescol)
      den.tempy$y <- den.tempy$y * length(tempy) * 2
      # multiply density by length of series and width of histogram bar 
      lines(den.tempy)
}
if(OPTIONS$FAM == "binomial" | OPTIONS$FAM == "quasibinomial") {
      barplot(cbind(c(sum(tempy==0),0),c(0,0),c(0,0),c(0,sum(tempy==1))), ylab = "COUNT", xlab = Yname , col=c(OPTIONS$abscol,OPTIONS$prescol))
      NbP <- sum(YYY[gr.modmask[, gr.Yi], gr.Yi])
      NbT <- sum(gr.modmask[, gr.Yi])
      NbA <- NbT - NbP
      title(paste("Prevalence of", Yname, ":", zapsmall(NbP/NbT,2)))
      mtext(paste("Absences:", NbA ," Presences:", NbP))
}
if(OPTIONS$FAM == "poisson" | OPTIONS$FAM == "quasipoisson")  {
      # title(paste("GOF:",  ))
}
if(OPTIONS$FAM == "gaussian") {
      title(paste("GOF:",  ))
      test <- chisq.gof(YYY[gr.modmask[, gr.Yi], gr.Yi], n.param.est = 2)
      mtext(test$alternative)
}
if (OPTIONS$PNG) dev.off()
cat(" ********** RESPONSE HISTOGRAM END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

