"grasp.exp.xpredplot" <-
function(gr.Xi)
{
cat("#", "\n")
cat("# FUNCTION: grasp.int.xpredplot ", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann)", "\n")
cat("# maps the spatial predictors contained in XXXpred", "\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP XXXpred PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
Xname <- names(XXX)[gr.Xi]
if (!OPTIONS$PNG) get(getOption("device"))()
else {png(filename = paste("xmap",Xname,".png",sep=""), width = 800, height = 800, res=150)}
resolution <- OPTIONS$RESOLUTION
maxX <- (ceiling(max(XXXpred$CX)/resolution) * resolution) + resolution
minX <- (ceiling(min(XXXpred$CX)/resolution) * resolution)
maxY <- (ceiling(max(XXXpred$CY)/resolution) * resolution) + resolution
minY <- (ceiling(min(XXXpred$CY)/resolution) * resolution)
half <- resolution/2
Nrow <- (maxX - minX)/resolution+1
Ncol <- (maxY - minY)/resolution+1
if((Nrow * Ncol) > 10000000) {
      cat("ERROR: too many pixels (", Nrow * Ncol, ")>> increase your resolution in the options", "\n")
      return(invisible())
}
cat("maxX: ", maxX, "\n")
cat("minX: ", minX, "\n")
cat("maxY: ", maxY, "\n")
cat("minY: ", minY, "\n")
cat("Nrow: ", Nrow, "\n")
cat("Ncol: ", Ncol, "\n")
par(mai = c(0.25, 0.25, 0.5, 0.25))
fred0 <- !is.na(XXXpred[, gr.Xi])
fred1 <- XXXpred$CX
fred2 <- XXXpred$CY
fred1 <- ceiling((fred1[fred0] - (minX - half))/resolution)
fred1<-ceiling(fred1 + (1-min(fred1)))
fred2 <- ceiling((fred2[fred0] - (minY + half))/resolution)
fred2<-ceiling(fred2 + (1-min(fred2)))
fred3 <- XXXpred[fred0, gr.Xi]
temp <- as.data.frame(cbind(fred1, fred2, fred3))
noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, sep = "x"))))
if(noduplicates)
      cat("There are no duplicated X and Y coordinates in prediction set !","\n")
else if(dim(temp)[1] < 10000) {
            temp <- aggregate(temp, by = list(temp$fred1, temp$fred2), mean)
            # only with small datasets
            cat("AGGREGATION: The mean of predictor value found for each combination of Xs and Ys is returned !!!","\n")
      }
      else cat("AGGREGATION (>10000 values): Only the first predicted value found for each combination of Xs and Ys is returned !!!","\n")
fred1 <- temp$fred1
fred2 <- temp$fred2
fred3 <- temp$fred3
fred3 <- ((fred3-min(fred3))/max(fred3) * 255) + 1
map <- matrix(0, nrow = Nrow, ncol = Ncol)
map[cbind(fred1, fred2)] <- fred3
# assign("MAP", map, env = .GlobalEnv)
plot(fred1, fred2, pch = " ", ylim = c(0, 1), xlim = c(0, 1),mgp = c(3, 0, 0), bty = "n")
image(map, zlim = range(c(min(fred3), max(fred3))), add = TRUE, col= terrain.colors(100))
title(paste(OPTIONS$TITLE, " ", Xname, "\n Grid resolution : ", 
OPTIONS$RESOLUTION), cex = 0.6)
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP XXXpred PLOT END ********** ", "\n")
cat("\n")
cat("\n")
}

