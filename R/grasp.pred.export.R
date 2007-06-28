"grasp.pred.export" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.pred.export", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann)", "\n")
cat("# export the prediction as ASCII file readable by Arcview", "\n")
cat("#", "\n")
cat("\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: predictions cannot be exported from EMPTY models","\n")
      sink()
      return("")
}
cat(" vvvvvvvvvv GRASP EXPORT vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
#tests if extra variables must be included in XXX such as INTER, AUTOCOR and TREND
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
print(ModelYi)
resolution <- OPTIONS$RESOLUTION
maxX <- (ceiling(max(XXXpred$CX)/resolution) * resolution) + resolution
minX <- (floor(min(XXXpred$CX)/resolution) * resolution)
maxY <- (ceiling(max(XXXpred$CY)/resolution) * resolution) + resolution
minY <- (floor(min(XXXpred$CY)/resolution) * resolution)
half <- resolution/2
Nrow <- (maxX - minX)/resolution
Ncol <- (maxY - minY)/resolution
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
filename <- paste(OPTIONS$PATHLUT, "pred_", names(YYY[gr.Yi]), ".asc",sep = "")
# create a path where the prediction will be saved
map <- matrix(-99.900000000000006, nrow = Nrow, ncol = Ncol)
fred1 <- round((XXXpred$CX - (minX - half))/resolution)
fred2 <- round(Ncol - ((XXXpred$CY - (minY + half))/resolution))
fred3 <- zapsmall(gr.predmat[, gr.Yi], 4)
temp <- as.data.frame(cbind(fred1, fred2, fred3))
#print(temp[1:10,  ])
noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, sep = "x"))))
if(noduplicates)
      cat("INFO: There are no duplicated X and Y coordinates in prediction set !","\n")
else if(dim(temp)[1] < 10000) {
      temp <- aggregate(temp, by = list(temp$fred1, temp$fred2),mean)
      # only with small datasets
      cat("AGGREGATION: The mean of predicted value found for each combination of Xs and Ys is returned !!!","\n")
      }
      else cat("AGGREGATION (>10000 predictions): Only the first predicted value found for each combination of Xs and Ys is returned !!!","\n")
fred1 <- temp$fred1
fred2 <- temp$fred2
fred3 <- temp$fred3
map[cbind(fred1, fred2)] <- fred3
write(paste("ncols", Nrow), file = filename)
write(paste("nrows", Ncol), file = filename, append = TRUE)
write(paste("xllcorner", minX), file = filename, append = TRUE)
write(paste("yllcorner", minY), file = filename, append = TRUE)
write(paste("cellsize", resolution), file = filename, append = TRUE)
write("NODATA_value -99.9", file = filename, append = TRUE)
write(map, file = filename, ncol = Ncol, append = TRUE)
cat("prediction exported to: ", "\n")
cat(filename, "\n")
cat("\n")
cat(" ********** GRASP EXPORT END ********** ", "\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

