"grasp.exp.summary" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE saves results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.exp.summary", "\n")
cat("# (by A. Lehmann using Splus summary function)", "\n")
cat("# calculate a summary of selected Ys and Xs", "\n")
cat("#", "\n")
gr.Yi <- gr.Yi
assign("gr.Yi", gr.Yi, env = .GlobalEnv)
cat("\n")
cat(" vvvvvvvvvv GRASP SUMMARY vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("Y:", "\n")
cat("RESPONSE NAME: ", Yname, "\n")
selection <- gr.modmask[, gr.Yi]
print(summary(YYY[selection, gr.Yi]))
cat("\n")
cat("XXX:", "\n")
print(summary(XXX[selection, gr.selX]))
cat(" ********** GRASP SUMMARY END ********** ", "\n")
cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

