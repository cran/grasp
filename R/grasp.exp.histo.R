"grasp.exp.histo" <-
function(gr.Yi, sX = gr.selX, nbar = OPTIONS$NBAR)
{
Yname <- names(YYY)[gr.Yi]
if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("histo",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.histo ", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann)", "\n")
cat("# calculates the distribution of the Response Variable (RV) on the histograms of Predictor Variables","\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP HISTOGRAM vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
y <- YYY[gr.modmask[, gr.Yi], gr.Yi]
XX <- XXX[gr.modmask[, gr.Yi],  ]
par(mfrow = OPTIONS$PLOTPAR)
first <- TRUE
prop <- length(y[y > 0])/length(y)
# calculates the proportion of presence vs. absence
for(i in sX) {
      if(is.numeric(XX[, i])) {
            # for continuous variables
            par(cex = 0.5)
            Xmin <- round(min(XX[!is.na(XX[, i]), i]), 2)
            Xlag <- (max(XX[!is.na(XX[, i]), i]) - Xmin)/nbar
            Xbreaks <- as.character(Xmin)
            for(nb in 1:(nbar - 1)) {
                  Xnew <- round(Xmin + (nb * Xlag), 2)
                  Xbreaks <- c(Xbreaks, as.character(Xnew))
            }
            temp <- table(factor(cut(y[!is.na(XX[, i])], c(-1,1.0e-005, 500))), cut(XX[!is.na(XX[, i]), i], nbar))
            loc <- barplot(temp, xlab = names(XX[i]), ylab = "count", names = Xbreaks, col=c(OPTIONS$abscol, OPTIONS$prescol))
            if (max(YYY[,gr.Yi]) <= 1) {
                  text(loc, apply(temp, 2, sum) + par("cxy")[2], temp[2,  ], cex = 0.8)
            }
            if(first)
                  title(paste(OPTIONS$TITLE, " ", names(YYY[gr.Yi])),cex=1)
            first <- FALSE
            nbp <- as.character(length(y[y > 0]))
            if (max(YYY[,gr.Yi]) <= 1) {
                  par(new = TRUE)
                  temp[1, temp[1,  ] == 0] <- 1e-008
                  fred2 <- (temp[2,  ]/(temp[1,  ] + temp[2,]))/prop
                  barplot(fred2, axes = FALSE, axisnames= FALSE, density = NULL, space = 0, lwd = 0.5,col="transparent")
                  abline(h = 1, lty = 2)
            }
      }
      if(is.factor(XXX[, i])) {
      # for factors
            temp <- table(factor(cut(y[!is.na(XX[, i])], c(-1,1.0e-005, 500))), XX[!is.na(XX[, i]), i])
            loc <- barplot(temp, xlab = names(XX[i]), ylab = "count", names = levels(XX[, i]), col=c(OPTIONS$abscol, OPTIONS$prescol))
            if (max(YYY[,gr.Yi]) <= 1) {
                  text(loc, apply(temp, 2, sum) + par("cxy")[2], temp[2,  ], cex = 0.8)
            }
            if(first)
                  title(paste(OPTIONS$TITLE, " ", names(YYY[gr.Yi])),cex=1.5)
            first <- FALSE
            if (max(YYY[,gr.Yi]) <= 1) {
                  par(new = TRUE)
                  temp[1, temp[1,  ] == 0] <- 1e-008
                  fred3 <- (temp[2,  ]/(temp[1,  ] + temp[2,]))/prop
                  barplot(fred3, axes = FALSE, xlab = "", ylab = "",density = -1, space = 0, lwd = 0.5,col="transparent")
                  abline(h = 1, lty = 2)
            }
      }
}
if (OPTIONS$PNG) dev.off()
cat(" ********** GRASP HISTOGRAM END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

