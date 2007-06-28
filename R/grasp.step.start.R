"grasp.step.start" <-
function(sX)
{
cat("#", "\n")
cat("# FUNCTION: grasp.step.start (by A. Lehmann)", "\n")
cat("# generates automatically a starting model formula", "\n")
cat("#", "\n")
sX <- intersect(sX, intersect(OPTIONS$STARTWITH, gr.selXCOR[[gr.Yi]]))
matchinter <- match("INTER", names(XXX)[sX])
if(!is.na(matchinter))
      sX <- sX[ - matchinter]
#starting variables
vnames <- names(XXX)[c(1, sX)]
model.formula <- NULL
for(Xi in sX) {
      vname <- names(XXX)[Xi]
      # loops through independant variable names
      if(is.factor(XXX[[vname]])) model.formula <- c(model.formula,vname) else model.formula <- c(model.formula,
      paste(OPTIONS$SMOOTHER, "(", vname, ",", OPTIONS$DF2[Xi], ")"))
}
model.formula <- c(model.formula)
if(length(model.formula) > 1) {
      # tests that there is more than one variables to generate the formula by collapsing the names
      model.formula <- eval(parse(text = paste("YYY$", names(YYY)[gr.Yi], "~", paste(model.formula, collapse = "+"))))
}
else {
      if(length(sX) == 0) {
            # tests if there is 0 variable
            model.formula <- eval(parse(text = paste("YYY$", names(YYY)[gr.Yi], "~", "0")))
      }
      else {
            # tests if there is 1 variable
            model.formula <- eval(parse(text = paste("YYY$", names(YYY)[gr.Yi], "~", model.formula)))
      }
}
model.formula
}

