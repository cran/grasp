"grasp.int.plot" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]

if (!OPTIONS$PNG) get(getOption("device"))()
else {png(filename = paste("plot",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE results are saved to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.int.plot ", "\n")
cat("# (by A. Lehmann using Splus gam.plot function)", "\n")
cat("# plots the partial response curves of each response variable for each selected Predictor","\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: Models cannot be plotted on EMPTY models","\n")
      cat("\n")
      sink()
      return("")
}
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
cat("\n")
cat(" vvvvvvvvvv GRASP PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
par(mfrow = OPTIONS$PLOTPAR)
plot(ModelYi, se = OPTIONS$STDERROR, rugplot = TRUE)
# plots the partial repsonse curves
par(mfrow = c(1, 1))
mtext(paste(OPTIONS$TITLE, " ", Yname), line = 3, outer = FALSE, side = 3,cex = 1)
mtext(paste("formula = ", ModelYi$call[2], ", family = ", ModelYi$call[3], sep = ""), line = 4.5, outer = FALSE, side = 1, cex =0.5)
if (OPTIONS$PNG) dev.off()
cat("\n")
cat(" ********** GRASP PLOT END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

