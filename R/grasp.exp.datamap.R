"grasp.exp.datamap" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("map",Yname,".png",sep=""), width = 800, height = 800, res=150)}

if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.datamap", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann)", "\n")
cat("# maps the Predictor Variable(RV) point surveys for a spatial check of input data","\n")
cat("#", "\n")
cat("\n")
cat(" vvvvvvvvvv GRASP DATAMAP vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
# names of RV
longitude <- XXX$CX
latitude <- XXX$CY
rangex <- max(longitude) - min(longitude)
rangey <- max(latitude) - min(latitude)
par( mai = c(0.5, 0.25, 0.75, 0.5))
XMIN <- min(longitude) - 0.1 * rangex
XMAX <- max(longitude) + 0.1 * rangex
YMIN <- min(latitude) - 0.1 * rangey
YMAX <- max(latitude) + 0.1 * rangey
plot(longitude[YYY[, gr.Yi] == 0], latitude[YYY[, gr.Yi] == 0], pch = "*", xlim = c(XMIN, XMAX), ylim = c(YMIN, YMAX), mgp = c(3, 0, 0), bty = "n", cex = 0.5,col= OPTIONS$abscol)
# plots data greater than 0 : presences
points(longitude[YYY[, gr.Yi] > 0][gr.modmask[, gr.Yi]], latitude[YYY[, gr.Yi] > 0][gr.modmask[, gr.Yi]], pch = "+", col = OPTIONS$prescol, cex = 1)
# plots data equal to 0 : absences
if(exists("CONTOURS")) {
      lines(CONTOURS$CX, CONTOURS$CY, lwd = 0.2,col = 9)
}
par(cex = 1)
nbp <- as.character(length((YYY[(YYY[, gr.Yi] > 0) & (gr.modmask[, gr.Yi]), gr.Yi])))
nba <- as.character(length((YYY[(YYY[, gr.Yi] == 0) & (gr.modmask[, gr.Yi]), gr.Yi])))
# legend(x = c(XMIN , YMIN ), y = c(XMAX, YMAX), legend = c(paste(">0s: ",nbp), paste("0s: ", nba)), pch = c("+ . "), bty ="n")
# adds a legend with number of presences and absences
title(main = paste(OPTIONS$TITLE, " ", Yname), cex = 0.6)
if(OPTIONS$FAM == "binomial" | OPTIONS$FAM == "quasibinomial") {
      NbP <- sum(YYY[gr.modmask[, gr.Yi], gr.Yi])
      NbT <- sum(gr.modmask[, gr.Yi])
      NbA <- NbT - NbP
      mtext(paste("Presences (+):", NbP, " Absences (*):", NbA))
}
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP DATAMAP END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

