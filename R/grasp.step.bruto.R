"grasp.step.bruto" <-
function(gr.Yi, selectedX)
{
Yname <- names(YYY)[gr.Yi]
cat("#", "\n")
cat("# FUNCTION: grasp.step.bruto", "\n")
cat("# (by J. Leathwick, J. Elith, adapted by A. Lehmann)", "\n")
cat("# select a gam model that explains a response variable by selected predictors using T. Hastie bruto library",
"\n")
cat("#", "\n")
#the function sent to me by John:
bruto.gam <- function(bruto.y, bruto.x, max.smooth = max(OPTIONS$DF2), sp.no = 1)
      {
      #
      # j leathwick, j elith - 1 October 2004
      #
      # version 1.6
      #
      # calculates a bruto/gam object in which spline functions are calculated
      # using an initial bruto model with either a single or multiple responses 
      # data for a nominated species is then fitted as a binomial gam using the 
      # level of predictor smoothing identified by bruto 
      #
      # updated 28/12/04 to vary degrees of freedom assigned to each gam smooth
      # according to smoothing parameter assigned by bruto
      #
      # takes as input args selecting dataframe, x and y columns
      #
      # require(mda) http://www.stats.ox.ac.uk/pub/MASS4/Software.html#Windows
      if(!exists("bruto")) {
            library(mda)
            if(!exists("bruto")) {
                  print("install library mda from: http://www.stats.ox.ac.uk/pub/MASS4/Software.html#Windows")
                  return()
            }
      }
      print(names(XXX)[bruto.x])
      # setup input data and assign to position one
      # get the dataframe name
      #
      #dataframe.name <- deparse(substitute(dat))
      n.preds <- length(bruto.x)
      sp.names <- eval(names(YYY)[bruto.y])
      target.sp <- sp.names[sp.no]
      #
      # now fit the bruto model
      #
      print("fitting initial bruto model", quote = FALSE)
      df.max <- rep(max.smooth, n.preds)
      #this sets the max df for smooths
      notNA <- gr.modmask[, gr.Yi]
      bruto.object <- bruto(x = eval(XXX[notNA, bruto.x]), y = eval(
      YYY[notNA, bruto.y]), w = WEIGHTS[notNA, gr.Yi], dfmax = df.max)
      pred.shapes <- bruto.object$type
      dfs <- bruto.object$df
      pred.names <- names(dfs)
      n.preds <- length(pred.shapes)
      #
      #now construct the gam call
      #
      model.call <- paste("gam(YYY$", target.sp, " ~ ", sep = "")
      print(paste("fitting a gam for", target.sp), quote = FALSE)
      first.term <- TRUE
      for(i in (1:n.preds)) {
            #this adds the first model terms w/o a "+"
            if((first.term) & (pred.shapes[i] == "linear")) {
                  model.call <- paste(model.call, pred.names[i], sep = "")
                  first.term <- FALSE
                  start.term <- i + 1
            }
            if((first.term) & (pred.shapes[i] == "smooth")) {
                  model.call <- paste(model.call, " s(", pred.names[i], ",", round(dfs[i],1), ")", sep = "")
                  first.term <- FALSE
                  start.term <- i + 1
            }
      }
      for(i in (start.term:n.preds)) {
            #then the remainder with "+"s
            if(pred.shapes[i] == "linear") {
                  model.call <- paste(model.call, " + ", pred.names[i], sep = "")
            }
            if(pred.shapes[i] == "smooth") {
                  model.call <- paste(model.call, " + s(", pred.names[i], ",", round(dfs[i],1), ")", sep = "")
            }
      }
      # this has to have the dataframe name hardwired if subsequent
      # calls to predict.gam in gam.cv are to work 
      # alternatively could save the dataframe and not delete at the end
      # but that seems risky
      model.call <- paste(model.call, ",data = XXX, family =", OPTIONS$FAM, ",subset=gr.modmask[,gr.Yi],weights=WEIGHTS[,gr.Yi])",sep = "")
      # now fit the gam
      model.gam <- eval(parse(text = model.call))
      return(model.gam$formula)
}# bruto.gam


assign("gr.Yi", gr.Yi, env = .GlobalEnv)
MODELCALLS.local <- MODELCALLS
gr.modmask.local <- gr.modmask
MODELCALLS.local[[gr.Yi]]$gamcall <- NULL
MODELCALLS.local[[gr.Yi]]$residualcall <- NULL
cat("\n")
cat(" vvvvvvvvvv GRASP BRUTO MODEL vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")


###########
bruto.scope <- function(startgam, selX)
{
      term.matrix <- predict.gam(startgam, XXX[1:10,  ], se.fit = FALSE,type = "terms")
      mod.terms <- dimnames(term.matrix)[[2]]
      step.list <- list()
      for(name in names(XXX)[selX]) {
            term <- mod.terms[grep(name, mod.terms)]
            if(length(term) > 0) {
                  if(!is.factor(XXX[[name]]))
                        junk <- c(term)
                  else 
                        junk <- c(1, name) 
            junk <- eval(parse(text = paste("~", paste(junk, collapse = "+"))))
            # collapse list of variables to create a scope line
            step.list[[name]] <- junk
            }
      }
      return(step.list)
}

cat("####################################", "\n")
cat("BRUTO MODEL:", "\n")
cat("####################################", "\n")
gr.modmask.local <- gr.modmask
continuousX <- gr.selXCOR[[gr.Yi]]
for(xi in gr.selXCOR[[gr.Yi]]) {
      if(is.factor(XXX[, xi]) & length(levels(XXX[, xi])) < 2) {
            gr.selXCOR[[gr.Yi]] <- gr.selXCOR[[gr.Yi]][ - match(xi, gr.selXCOR[[gr.Yi]])]
      }
      if(is.factor(XXX[, xi])) {
            continuousX <- continuousX[ - match(xi, continuousX)]
            print(paste("factor", names(XXX)[xi], "was momentally excluded from the potential predictors of",names(YYY)[gr.Yi], "to run BRUTO selection"))
      }
}
factorX <- gr.selXCOR[[gr.Yi]]
factorX <- factorX[ - match(continuousX, factorX)]
BRUTO <- bruto.gam(gr.Yi, continuousX)
# generates a starting formula with all variables
SUBSET <- paste("gr.modmask[,", gr.Yi, "]")
WEIGHTS.df <- paste("WEIGHTS[,", gr.Yi, "]")
gam.bruto <- eval(parse(text = paste("gam(", as.character(BRUTO[2]),"~", as.character(BRUTO[3]), ",data = XXX, family = ",OPTIONS$FAM, ",weights = ", WEIGHTS.df, ", bf.maxit = 1000,bf.epsilon=0.001,na.action = na.omit, subset = ",SUBSET, " )")))
if(length(factorX) > 0) {
      bruto.formula <- eval(parse(text = paste("~.+", paste(names(XXX)[factorX], collapse = "+"))))
      gam.bruto <- update(gam.bruto, bruto.formula)
      cat("# Testing factor variables #", "\n")
      FactorScope <- bruto.scope(gam.bruto, gr.selXCOR[[gr.Yi]])
      print(FactorScope)
      gam.bruto <- step.gam(gam.bruto, scope = FactorScope,
      trace = TRUE, direction = "both")
}
# caculate the full model with all variables
print(gam.bruto)
cat("Null Deviance: ", zapsmall(gam.bruto$null.deviance), "\n")
cat("D2: ", zapsmall((gam.bruto$null.deviance - gam.bruto$deviance)/
gam.bruto$null.deviance), "\n")
cat("", "\n")
cat("####################################", "\n")
cat("", "\n")
print(names(YYY)[gr.Yi])
cat("", "\n")
cat("INFO: !!! Model formula saved in MODELCALLS[[", gr.Yi, "]]",
sep = "", "\n")
cat("\n")
cat(" ********** GRASP BRUTO MODEL END ********** ", "\n")
cat("\n")
return(gam.bruto)
}

