"grasp" <-
function(selected.responses, selected.predictors, title = "GRASP: ", path = "", save.outputs = FALSE, save.PNG = FALSE, 
gr.fam = "binomial", weights = FALSE, set.limits = FALSE, use.limits = FALSE, reset.lim = TRUE, selxlim = gr.selX, npast = 10,
make.summary = FALSE, plot.maps = FALSE, plot.distry = FALSE, plot.histograms = FALSE, plot.respvspred = FALSE, plot.xpred = FALSE,
plot.correlation = FALSE, corlim = 100, use.correlation = FALSE, reset.cor = TRUE, full.models = FALSE, stepwise.models = FALSE, 
 test = "F", plimit = 0.05, direction = "both", df1 = 0, df2 = 4, startwith = gr.selX, resetstart = TRUE,
contributions = FALSE, plot.contributions = FALSE, plot.models = FALSE, response.curves = FALSE, model.anova = FALSE, validate.models = FALSE, 
predictions = FALSE, export.predictions = FALSE, plot.predictions = FALSE, lookup.tables = FALSE, 
plotpar = c(3, 3), prescol= "black", abscol="grey", nbar = 10, show.details = FALSE, StdError = TRUE, cvgroups = 5, resolution = as.integer((max(XXXpred$CX) - min(XXXpred$CX)) / (length(unique(XXXpred$CX)) - 1)) )
{
#################################################################################
# FUNCTION: grasp (by A. Lehmann)
# Generalized Regression Analysis and Spatial Prediction for R
# Copyright: Landcare Research New Zealand & Swiss Centre for Faunal Cartography, 1999-2005
# REF: A. Lehmann, J.R. Leathwick & J.McC. Overton, 2002. GRASP. Ecological Modelling, 157: 189-207
# USER MANUAL and UPDATES at: WWW.UNIGE.CH/IA/CLIMATE/GRASP 
# 
# GRASP is a general method for making spatial predictions of response variables (RV) using point 
# surveys of the RV and spatial coverages of Predictor variables (PV).  GRASP-R consists of a set
# of functions working within R environment (under continued development) that:
# - initializes the variables: grasp.in()
# - calculates the environmental envelop containing the RV: grasp.msk.limits(), 
# - calculate a summary of selected Ys and Xs: grasp.exp.summary(), 
# - maps the RV point surveys: grasp.exp.datamap(), 
# - calculates the distribution of the RV on the histograms of Predictor Variables: grasp.exp.histo(), 
# - plots the distribution of the RV against the Predictor Variables: grasp.exp.RvsP(), 
# - plots a correlation matrix of the Predictor Variables: grasp.exp.cormat(), 
# - generates automatically a starting model formula: grasp.step.start(), 
# - generates automatically a model scope: grasp.step.scope(), 
# - selects a statistically significant model that explains a RV by PVs: grasp.step(), 
# - plots the partial response curves of each RVs for each selected PVs: grasp.int.plot(), 
# - plots a cross-validation graph and a qqplot of model residuals to check their distribution: grasp.mod.validate()  
# - calculates the contribution of the selected variables: grasp.int.contrib()
# - plots the contribution of each PVs: grasp.int.contplot()
# - plots combined response curves of selected RVs along selected PV gradients: grasp.int.response(), 
# - predicts the RV from a new dataframe containing the PVs to predict from: grasp.pred(), 
# - export the prediction as ASCII file readable by Arcview: grasp.pred.export()
# - plot the predictions within Splus: grasp.pred.plot()
# - export the predictions as a look up table in a text file: grasp.pred.lookup().
#
# Originally, these were conceived and developed to analyse, model and predict vegetation distribution over 
# New Zealand.  Thus the functions are best suited for this purposes, and the general application to 
# other RVs may require further development
#
#################################################################################


if(!exists("gam")) { # test for gam package
      library(gam)
      if(!exists("gam")) {
            print("!!! Install gam package from CRAN !!!")
            return()
      }
}
gr.selY <- selected.responses
gr.selX <- selected.predictors

if (is.character(gr.selY)) { gr.selY <- match(gr.selY,names(YYY)) }
if (is.character(gr.selX)) { gr.selX <- match(gr.selX,names(XXX)) }

   ####################### GRASP OPTIONS ##########################
grasp.options(title = title, path = path, plotpar = plotpar, npast = npast, selxlim = selxlim, corlim = corlim, nbar = nbar,  df1 = df1, df2 = df2, test = test, plimit = plimit, direction = direction, weights = weights, mincont = 0, startwith = startwith, resetstart = resetstart, cvgroups = cvgroups, resolution = resolution, StdError = StdError)

if(all.equal(as.character(XXX[, 1]), as.character(YYY[, 1])) != TRUE) {
      stop("DIFFERENT INDICES IN COLUMN 1 OF YYY and XXX, CHECK THE ORDER OF YYY and XXX !")
}
if(all.equal(dimnames(XXX)[2], dimnames(XXXpred)[2]) != TRUE) {
      stop("Different variable names in XXX and XXXpred !")
}
if(dim(XXX)[1] != dim(YYY)[1]) {
      stop("Different number of cases in XXX and YYY !")
}
OPTIONS.local <- OPTIONS
# assigning variables locally
FAM<-gr.fam
# "gaussian" for normally distributed responses (e.g. biomass)
# "quasibinomial" or "binomial" for presence absence data, cover percentage (e.g. species occurences)
# "quasipoisson" or "poisson" for counts (species richness)
if (length(grep("quasi",FAM)>0)) OPTIONS.local$TEST <- "F" 
OPTIONS.local$FAM <- FAM
# gr.modmask <- convert.col.type(target = gr.modmask, column.spec = 1:(dim(YYY)[2]), column.type = "logical")
gr.modmask.local <- gr.modmask
# print(gr.modmask[1:10,  ])
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
gr.selXCOR <- NULL
assign("gr.selY", gr.selY, env = .GlobalEnv)
assign("gr.selX", gr.selX, env = .GlobalEnv)
cat("\n")
cat("\n")
OPTIONS.local$abscol <- abscol
OPTIONS.local$prescol <- prescol
OPTIONS.local$PNG <- save.PNG
wildcard <- ""
assign("wc", wildcard, env = .GlobalEnv)
assign("OPTIONS", OPTIONS.local, env = .GlobalEnv)
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("########################################################################################", "\n")
cat("GRASP: Generalized Regression Analysis and Spatial Prediction for R, v2.0", "\n")
cat("Copyrights: Landcare Research New Zealand & Swiss Centre for Faunal Cartography, 1999-2005", "\n")
cat("Ref: A. Lehmann, J.R. Leathwick & J.McC. Overton, 2002. GRASP. Ecological Modelling, 157: 189-207", "\n")
cat("########################################################################################", "\n")
print(version)
cat(date(), "\n")
cat("========== SELECTED VARIABLES ===========")
cat("\n")
cat("selected responses: ", gr.selY)
cat("\n")
cat("selected predictors: ", gr.selX)
cat("\n")
cat("============================================")
cat("\n")
cat("========== SET OPTIONS ===========")
cat("\n")
cat(OPTIONS.local$TITLE)
cat("\n")
for(i in (1:length(OPTIONS.local))) {
cat(paste(names(OPTIONS.local)[i], ": ", OPTIONS.local[i]))
cat("\n")
}
cat("\n")
cat("=========================================")
cat("\n")
OPTIONS.local$RESETLIM <- reset.lim
# if TRUE resets limits to T for selected Ys at each grasp session
OPTIONS.local$RESETCOR <- reset.cor
# if TRUE resets tolerated correlated predictors to gr.selX for each selected Ys at each grasp session
OPTIONS.local$details <- show.details
# set the principal options
OPTIONS.local$SINK <- save.outputs
if(OPTIONS.local$SINK) { # if TRUE creates a log file
      for(yi in gr.selY) {
            path <- paste(OPTIONS.local$PATHLOG, "log_", names(YYY)[yi], ".txt", sep = "")
            sink(path, append = TRUE)
            cat("\n")
            cat("=========== SET OPTIONS ==========")
            cat("\n")
            cat(OPTIONS.local$TITLE)
            cat("\n")
            for(i in (1:length(OPTIONS.local))) {
                  cat(paste(names(OPTIONS.local)[i], ": ", OPTIONS.local[i]))
                  cat("\n")
            }
            cat("\n")
            cat("=========================================")
            cat("\n")
            sink()
      }
}
# whether results should be saved in a text file
cat("\n")
graphname <- function(thename){
      name <- thename
      i <- 1
      newname <- paste(name, i, sep = "")
      }
if(OPTIONS.local$RESETSTART) {
      OPTIONS.local$STARTWITH <- gr.selX
}
#  
if(OPTIONS.local$RESETLIM) {
      for(i in gr.selY)
      gr.modmask.local[, i] <- rep(TRUE, dim(YYY)[1])
}      
for(yi in gr.selY) {
      gr.modmask.local[, yi] <- gr.modmask.local[, yi] & !is.na(YYY[, yi])
}
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
assign("OPTIONS", OPTIONS.local, env = .GlobalEnv)
# print(gr.modmask[1:10,  ]) #tests fo NAs in Ys and set them to FALSE in gr.modmask
# print(gr.modmask[1:10,  ]) #  
if(exists("DATAFILTER")) {
      gr.modmask.local <- as.matrix(gr.modmask.local & DATAFILTER)
      cat("INFO: !!! Applying DATAFILTER !!!", "\n")
      cat("Number of observations used applying datafilter:", "\n")
      cat(names(YYY)[gr.selY], "\n")
if(length(gr.selY) > 1)
      cat(apply(DATAFILTER[, gr.selY], 2, sum), "\n")
else cat(sum(DATAFILTER[, gr.selY]), "\n")
      cat("\n")
}
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
# print(gr.modmask[1:10,  ])
before.modmask <- gr.modmask.local
#  
if(set.limits) {
      # calculates the environmental envelop containing the RV
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating limits VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.msk.limits)
      cat(date(), "\n")
      cat("\n")
      cat("############ limits calculated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
# print(gr.modmask[1:10,  ])
gr.modmask.local <- gr.modmask
# print(gr.modmask[1:10,  ])
if(OPTIONS.local$RESETLIM) {
      for(i in gr.selY)
            gr.modmask.local[, i] <- rep(TRUE, dim(YYY)[1])
}
if(exists("DATAFILTER")) {
      # reaasigning DATAFILTER
      gr.modmask.local <- as.matrix(gr.modmask.local & DATAFILTER)
}
#  
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
after.modmask <- gr.modmask.local
# print(gr.modmask[1:10,  ])
if(!use.limits) {
      gr.modmask.local <- before.modmask
}
else {
      cat("\n")
      cat("INFO:", "!!! Applying environmental limits for data exploration !!!", "\n")
      cat("Number of observations used applying limits and datafilter:", "\n")
      cat(names(YYY)[gr.selY], "\n")
      if(length(gr.selY) > 1)
            cat(apply(gr.modmask.local[, gr.selY], 2, sum), "\n")
      else cat(sum(gr.modmask.local[, gr.selY]), "\n")
cat("\n")
}
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
# 
WEIGHTS.local <- WEIGHTS
if(OPTIONS.local$RECALCULATEWEIGHTS) {
      for(Ys in gr.selY) {
            SUMof1 <- sum(WEIGHTS.local[gr.modmask.local[, Ys] & YYY[, Ys] == 1, Ys])
            SUMof0 <- sum(YYY[gr.modmask.local[, Ys], Ys] == 0)
            WEIGHTof0 <- SUMof1/SUMof0
            WEIGHTS.local[gr.modmask.local[, Ys] & YYY[, Ys] == 0, Ys] <- WEIGHTof0
            if(Ys == gr.selY[1])
                  cat("INFO: !!! WEIGHTS: sum of weights of 0s = sum of weights of 1s", "\n")
            cat(paste("Weights of 0s for ", names(YYY)[Ys], "=", zapsmall(WEIGHTof0)), "\n")
      }
}
# end of if RECALCULATE WEIGHTS = TRUE
if(!OPTIONS.local$RECALCULATEWEIGHTS) {
      for(Ys in gr.selY) {
            WEIGHTS0 <- 1
            WEIGHTS.local[YYY[!is.na(YYY[, Ys]), Ys] == 0, Ys] <- WEIGHTS0
      }
}
# end of if RECALCULATE WEIGHTS = FALSE
assign("WEIGHTS", WEIGHTS.local, env = .GlobalEnv)
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
if(make.summary) {
      # print(gr.modmask[1:10,  ]) # calculates summary of selecected Ys and Xs
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating summaries VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.summary)
      cat(date(), "\n")
      cat("\n")
      cat("############ summaries calculated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
#  
if(plot.maps) {
      # maps the RV point surveys
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting maps VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.datamap)
      cat(date(), "\n")
      cat("\n")
      cat("############ map produced ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
#  
if(plot.distry) {
      # histo of response varaible
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV histogram of response VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.distry)
      cat(date(), "\n")
      cat("\n")
      cat("############ histogram produced ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
#  
if(plot.histograms) {
      # calculates the distribution of the RV on the histograms of Predictor Variables
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting histograms VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.histo)
      cat(date(), "\n")
      cat("\n")
      cat("############ histograms produced ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
# 
if(plot.respvspred) {
      # print(gr.modmask[1:10,  ]) 
      # plot the distribution of the RV against Predictor Variables
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting response versus predictors VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.RvsP)
      cat(date(), "\n")
      cat("\n")
      cat("############ response versus predictors produced ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(OPTIONS.local$RESETCOR | !is.null("gr.selXCOR")) {
      gr.selXcor <- list(gr.selX)
      for(gr.Yi in gr.selY)
            gr.selXcor[[gr.Yi]] <- gr.selX
      gr.selXCOR <- gr.selXcor
}
assign("gr.selXCOR", gr.selXCOR, env = .GlobalEnv)
#  
if(plot.correlation) {
      # plot a correlation matrix of the Predictor Variables
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting correlation matrix VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.exp.cormat)
      cat(date(), "\n")
      cat("\n")
      cat("############ correlation matrix plotted ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(plot.xpred) {
# maps predictors from XXXpred
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV mapping XXXpred predictors VVVVVVVVVVVV", "\n")
      lapply(gr.selX, grasp.exp.xpredplot)
      cat(date(), "\n")
      cat("\n")
      cat("############ XXXpred mapped ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
gr.modmask.local <- after.modmask
if(!use.limits) {
      gr.modmask.local <- before.modmask
}
else {
      cat("INFO:", "!!! Applying environmental limits for model selection and interpretation !!!", "\n")
      cat("Number of observations used applying limits and datafilter:", "\n")
      cat(names(YYY)[gr.selY], "\n")
      if(length(gr.selY) > 1)
            cat(apply(gr.modmask.local[, gr.selY], 2, sum), "\n")
      else cat(sum(gr.modmask.local[, gr.selY]), "\n")
      cat("\n")
}
if(full.models) {
      # selects a full model that explains a RV by all PVs
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV estimating models VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.step.full)
      cat(date(), "\n")
      cat("\n")
      cat("############ models estimated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(stepwise.models) {
      # selects a statistically significant model that explains a RV by PVs
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV estimating models VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.step)
      cat(date(), "\n")
      cat("\n")
      cat("############ models estimated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(model.anova) {
      # calculates anova table for selected model
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating anova VVVVVVVVVVVV", "\n")
      cat(OPTIONS.local$TITLE, "\n")
      lapply(gr.selY, grasp.mod.anova)
      cat(date(), "\n")
      cat("############ anova calculated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(validate.models) {
      # plots a cross-validation graph and a qqplot of model residuals to check their distribution
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV validating models VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.mod.validate)
      cat(date(), "\n")
      cat("\n")
      cat("############ validations plotted ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(contributions) {
      # calculates the contribution of the selected variables
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating contributions VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.int.contrib)
      cat(date(), "\n")
      cat("\n")
      cat("INFO: !!! CONTRIBUTIONS saved in ALONE.CONTRIB, MODEL.CONTRIB and DROP.CONTRIB", sep = "", "\n")
      cat("\n")
      cat("############ contributions calculated ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(plot.contributions) {
      # plots the mean or median contribution of each PVs
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting contributions VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.int.contplot)
      cat(date(), "\n")
      cat("\n")
      cat("############ contributions plotted ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(plot.models) {
      # plots the partial response curves of each RVs for each selected PVs
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting models VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.int.plot)
      cat(date(), "\n")
      cat("\n")
      cat("############ models plotted ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(response.curves) {
      # plots combined response curves of selected RVs along selected PV gradients
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting response curves VVVVVVVVVVVV", "\n")
      grasp.int.response(gr.selY, gr.selX)
      cat(date(), "\n")
      cat("\n")
      cat("############ response curves plotted ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(predictions) {
      # predicts the RV from a new dataframe containing the PVs to predict from
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating predictions VVVVVVVVVVVV", "\n")
      cat(OPTIONS.local$TITLE, "\n")
      lapply(gr.selY, grasp.pred)
      cat(date(), "\n")
      cat("############ predictions saved ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(export.predictions) {
      # exports the predictions to a ascii file readable by Arcview
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV exporting predictions VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.pred.export)
      cat(date(), "\n")
      cat("############ predictions exported ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(plot.predictions) {
      # maps the predictions
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV plotting predictions VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.pred.plot)
      cat(date(), "\n")
      cat("\n")
      cat("############ predictions map produced ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
if(lookup.tables) {
      # exports the predictions as a lookup table in a text file
      cat("\n")
      cat("\n")
      cat("\n")
      cat("VVVVVVVVVVVV calculating lookup tables VVVVVVVVVVVV", "\n")
      lapply(gr.selY, grasp.pred.lookup)
      cat(date(), "\n")
      cat("############ lookup table exported ############", "\n")
      cat("\n")
      cat("\n")
      cat("\n")
}
gr.modmask.local <- after.modmask
assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
assign("OPTIONS", OPTIONS.local, env = .GlobalEnv)
cat("\n")
cat("\n")
cat("\n")
cat("############################################", "\n")
cat("############# YOU GRASPED IT ! #############", "\n")
cat("############################################", "\n")
cat("####### WWW.UNIGE.CH/IA/CLIMATE/GRASP ######", "\n")
cat("############################################", "\n")
cat("\n")
return()
}

