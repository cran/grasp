"grasp.exp.cormat" <-
function(gr.Yi, cols = gr.selX, thin = OPTIONS$CORTHIN, corplot = TRUE)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.cormat", "\n")
cat("# (adapted from Mike Austin and Simon Barry, CSIRO, Australia)","\n")
cat("#  plot a correlation matrix of the Predictor Variables", "\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP CORMAT vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
gr.selXCOR.local<- gr.selXCOR
if(corplot) {
          data <- XXX[gr.modmask[, gr.Yi], ]
          panel <- function(x, y) {
          ind <- YYY[, gr.Yi] == 1
          points(x, y,col=OPTIONS$abscol)
          points(x[ind], y[ind], col = OPTIONS$prescol)
corr<-zapsmall(cor(x,y),2)
      text(min(x),max(y),cex=1.2,as.character(corr),adj=0)
}
    ndata <- thin * nrow(data)
    index <- ((1:ndata) * 1)/thin
    thindata <- data[index, ]
    pairs(thindata[, cols], panel = panel)
    text(0,1.17,paste("GRASP: ", " ", Yname), adj=0,cex = 0.7)
    if (OPTIONS$PNG) dev.off()
}
gr.selXcor.local <- NULL
gr.selXfac.local <- NULL
for(i in gr.selX) {
      if(!is.factor(XXX[, i]))
            gr.selXcor.local <- c(gr.selXcor.local, i)
            else gr.selXfac.local <- c(gr.selXfac.local, i)
}
gr.selXorder <- match(gr.selXcor.local, gr.selXcor.local)
cortemp <- cor(XXX[gr.modmask[, gr.Yi], gr.selXcor.local])
cortemp[cortemp == 1] <- 0
cat("STARTING MATRIX:", "\n")
print(cortemp)
cat(names(XXX)[gr.selXcor.local], "\n")
while(max(abs(cortemp)) > OPTIONS$CORLIM) {
      cormax <- abs(cortemp) == max(abs(cortemp))
      selCor <- apply(cormax, 2, sum) * gr.selXorder
      gr.selXcor.local <- gr.selXcor.local[ - match(max(selCor),selCor)]
      gr.selXorder <- gr.selXorder[ - match(max(selCor), selCor)]
      cortemp <- cor(XXX[gr.modmask[, gr.Yi], gr.selXcor.local])
      cortemp[cortemp == 1] <- 0
}
cat("UNCORRELATED MATRIX:", "\n")
print(cortemp)
gr.selXCOR.local[[gr.Yi]] <- c(gr.selXcor.local, gr.selXfac.local)
if(!OPTIONS$RESETCOR)
      assign("gr.selXCOR", gr.selXCOR.local, env = .GlobalEnv)
cat("\n")
cat(" ********** GRASP CORMAT END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

