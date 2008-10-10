"grasp.pred.plot" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]


if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("pred",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
# if TRUE saves results to a text file
path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = ""
)
sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.pred.plot ", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann)", "\n")
cat("# maps the spatial predictions", "\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP PRED PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
# names of RV
resolution <- OPTIONS$RESOLUTION
maxX <- (ceiling(max(XXXpred$CX)/resolution) * resolution) + 
resolution
minX <- (floor(min(XXXpred$CX)/resolution) * resolution)
maxY <- (ceiling(max(XXXpred$CY)/resolution) * resolution) + 
resolution
minY <- (floor(min(XXXpred$CY)/resolution) * resolution)
half <- resolution/2
Nrow <- (maxX - minX)/resolution+1
Ncol <- (maxY - minY)/resolution+1
if((Nrow * Ncol) > 10000000) {
cat("ERROR: too many pixels (", Nrow * Ncol, 
")>> increase your resolution in the options", "\n")
return(invisible())
}
cat("maxX: ", maxX, "\n")
cat("minX: ", minX, "\n")
cat("maxY: ", maxY, "\n")
cat("minY: ", minY, "\n")
cat("Nrow: ", Nrow, "\n")
cat("Ncol: ", Ncol, "\n")
rangeX <- maxX - minX
rangeY <- maxY - minY
#ratiorange <- rangeY/rangeX
par(mai = c(0.25, 0.25, 0.5, 0.25))
fred0 <- (gr.predmat[, gr.Yi] > 0)
fred1 <- XXXpred$CX
fred2 <- XXXpred$CY
fred1 <- ceiling((fred1[fred0] - (minX - half))/resolution)
fred1<-ceiling(fred1 + (1-min(fred1)))
fred2 <- ceiling((fred2[fred0] - (minY + half))/resolution)
fred2<-ceiling(fred2 + (1-min(fred2)))
fred3 <- gr.predmat[fred0, gr.Yi]
temp <- as.data.frame(cbind(fred1, fred2, fred3))
print(temp[1:10,  ])
noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, sep = 
"x"))))
if(noduplicates)
cat("There are no duplicated X and Y coordinates in prediction set !",
"\n")
else if(dim(temp)[1] < 10000) {
temp <- aggregate(temp, by = list(temp$fred1, temp$fred2),
mean)
# only with small datasets
cat("AGGREGATION: The mean of predicted value found for each combination of Xs and Ys is returned !!!",
"\n")
}
else cat("AGGREGATION (>10000 predictions): Only the first predicted value found for each combination of Xs and Ys is returned !!!",
"\n")
fred1 <- temp$fred1
fred2 <- temp$fred2
fred3 <- temp$fred3
# print(temp[1:10,  ])
map <- matrix(NA, nrow = Nrow, ncol = Ncol)
map[cbind(fred1, fred2)] <- fred3
plot(fred1, fred2, pch = " ", ylim = c(0, 1), xlim = c(0,1), mgp = c(
3, 0, 0), bty = "n")
image(map, zlim = range(c(min(fred3), max(fred3))), add = TRUE,col= topo.colors(100))
# invisible(image.legend(map, x = c(0.29999999999999999 * Nrow, 0.80000000000000004 * Nrow), y = c(1.1000000000000001 * Ncol,1.800000000000001 * Ncol), nint = 10))
# lines(NZcoast$easting * 5, NZcoast$northing * 5)
title(paste(OPTIONS$TITLE, " ", Yname, "\n Grid resolution : ", 
OPTIONS$RESOLUTION), cex = 0.59999999999999998)
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP PRED PLOT END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
sink()
cat("<=>")
}
else cat("\n")
}

