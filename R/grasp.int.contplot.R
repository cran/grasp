"grasp.int.contplot" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]

if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("cont",Yname,".png",sep=""), width = 800, height = 800, res=150)}
if(OPTIONS$SINK) {
      # if TRUE save results in a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.int.contplot", "\n")
cat("# (by A. Lehmann)", "\n")
cat("# Barplots of predictor contributions", "\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: contribution cannot be plotted on EMPTY models","\n")
      cat("\n")
      sink()
      return("")
}
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
cat("\n")
cat(" vvvvvvvvvv GRASP CONT PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
par(mfrow = c(1, 3))
if(OPTIONS$CONTPLOT == "histo") {
      # Histogram
      barplot(DROP.CONTRIB[mod.terms, gr.Yi], horiz = TRUE)
      title("SELECTION (DROP)", cex = 0.5)
      barplot(MODEL.CONTRIB[mod.terms, gr.Yi], horiz = TRUE, names = mod.terms)
      title("MODEL (INSIDE)", cex = 0.5)
      # mean contribution by droping each variable from the selected models
      barplot(ALONE.CONTRIB[mod.terms, gr.Yi], horiz = TRUE)
      title("POTENTIAL (ALONE)", cex = 0.5)
}
if(OPTIONS$CONTPLOT == "boxplot") {
      # Boxplot
      boxplot(as.data.frame(t(DROP.CONTRIB[mod.terms, gr.Yi])),horiz = TRUE)
      title("DROP CONTRIBUTION", cex = 0.5)
      # same as upper
      boxplot(as.data.frame(t(ALONE.CONTRIB[mod.terms, gr.Yi])),horiz = TRUE)
      title("ALONE CONTRIBUTION", cex = 0.5)
}
par(mfrow = c(1, 1))
mtext(paste(OPTIONS$TITLE, Yname), line = 4.5, outer = FALSE, side = 1,
cex = 0.8)
# mtext(paste("formula = ", ModelYi$call[2], ", family = ", ModelYi$call[3], sep = ""), line = 4.5, outer = F, side = 1, cex = 0.5)
if(OPTIONS$PNG) {dev.off()}
cat("\n")
cat(" ********** GRASP CONT PLOT END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

