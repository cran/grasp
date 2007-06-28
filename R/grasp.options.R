"grasp.options" <-
function(title = "GRASP: ", path = "c:/temp", plotpar = c(3, 3), npast = 0, selxlim = gr.selX, corlim = 100, corplot = TRUE, nbar = 10, df1 = 0, df2 = 4, test = "F", plimit = 0.05, direction = "both", weights = FALSE, mincont = 0, startwith = gr.selX, resetstart = TRUE, showdetail = FALSE, cvgroups = 5, resolution = (max(XXXpred$CX) - min(XXXpred$CX))/(length(unique(XXXpred$CX)) - 1), StdError = TRUE)
{
cat("#", "\n")
cat(" # FUNCTION: grasp.options (by A. Lehmann)", "\n")
cat(" # Sets the OPTIONS that are used by the different grasp functions", "\n")
cat("#", "\n")
cat("####################### GRASP OPTIONS ##########################", "\n")
if (is.character(selxlim))selxlim <- match(selxlim, names(XXX))
if (is.character(startwith))startwith <- match(startwith, names(XXX))

# GENERAL GRASP options:
TITLE <- title
TEST <- test
#
# Put your own title
PATH <- paste(path, sep = "")
# where predictions, lookup tables, contributions, model calls and log files will be saved 
PATHPNG <- paste(PATH, sep = "")
PATHLOG <- paste(PATH, sep = "")
PATHLUT <- paste(PATH, sep = "")
PLOTPAR <- plotpar
#
# parameters spliting the graph window of grasp.exp.histo, grasp.exp.RvsP, grasp.int.plot, grasp.int.response
################################################################
# GRASP.LIMITS options:
NPAST <- npast
# number of cases past the limits given by last positive values along predictors gradients
LIM <- "and"
# "and" or "or" or "mixte" = different ways of combining predictor limits
SELXLIM <- selxlim
# variables used to calculate the limits, default = gr.selX (selected.predictors) 
################################################################
# grasp.exp.cormat options: 
CORLIM <- corlim/100
# maximum tolerated correlations between predictors
CORTHIN <- 1
# percentage between 0 and 1 of value kept to plot correlation matrix in order to avoid huge dataset
CORPLOT <- corplot
# if TRUE plot the correlation matrix using CORTHIN percentage of data
################################################################
# GRASP.HISTO options:
NBAR <- nbar
# number of bars in histograms
################################################################
# GRASP.STEP options:

SMOOTHER <- "s"
# "s" or "lo" :choice of smoother s = spline and lo = loess
DF1 <- rep(round(df1), dim(XXX)[2])
# 1st. degrees of freedom for smoother: NA,1,1.5,2,3,4,6,8.... (optional !)
if(round(df1) == 0) DF1 <- rep(NA, dim(XXX)[2])
# 1st. degrees of freedom for smoother: NA,1,1.5,2,3,4,6,8.... (optional !)
DF2 <- rep(round(df2), dim(XXX)[2])
# 2nd. degrees of freedom: 1,1.5,2,3,4,6,8.... (necessary !)
# DF2 and DF1 are choices of DFs for the stepwise procedure of predictor selections. 
# The procedure will go through one or two levels of DFs. 
# DF = 1 corresponds to a linear fit, where DF =4 is Splus default.

MINCONTRIB <- mincont
if(mincont > 0) PERCONT <- TRUE
else PERCONT <- FALSE
P.limit <- plimit
# the higher it gets, the more variables are kept in models
# Chisq and F test limit for eliminating or reentering variables using "Chisq" or "F"
DIRECTION <- direction
#specify selection direction (default = both)
RECALCULATEWEIGHTS <- weights
# Weights O observations in order that the sum of wieghts of 0s equal the sum of weights of 1s
SHOWDETAIL <- showdetail
STARTWITH <- startwith
RESETSTART <- resetstart
STDERROR <- StdError
#
################################################################
# grasp.int.contrib options:
################################################################
# GRASP.CONTPLOT options:
CONTPLOT <- "histo"
# "histo" or "boxplot": choice of outputs
################################################################
# GRASP.VALIDATE options:
CVGROUPS <- cvgroups
# number of random groups created from the data to cross-validate the models
################################################################
# grasp.pred.export options:
RESOLUTION <- resolution
# grid resolution to export ascii file to Arcview
################################################################
# Saving options
FAM <- "binomial"
if(exists("OPTIONS$FAM")) FAM <- OPTIONS$FAM
OPTIONS <- list(TITLE = TITLE, PATH = PATH, PATHPNG = PATHPNG, PATHLOG = PATHLOG, PATHLUT = PATHLUT, PLOTPAR = PLOTPAR, NBAR = NBAR, NPAST = NPAST, LIM = LIM, SELXLIM = SELXLIM, CORLIM = CORLIM, RECALCULATEWEIGHTS = RECALCULATEWEIGHTS, CORTHIN = CORTHIN, CORPLOT = CORPLOT, DF1 = DF1, DF2 = DF2, SMOOTHER = SMOOTHER, TEST = TEST, PERCONT = PERCONT, DIRECTION = DIRECTION, P.limit = P.limit, MINCONTRIB = MINCONTRIB, STARTWITH = STARTWITH, RESETSTART = RESETSTART, SHOWDETAIL = SHOWDETAIL, CONTPLOT = CONTPLOT, CVGROUPS = CVGROUPS, RESOLUTION = RESOLUTION, STDERROR = STDERROR, FAM = FAM)
assign("OPTIONS", OPTIONS, env = .GlobalEnv)
cat("\n")
cat("========== SET OPTIONS ===========")
cat("\n")
cat(OPTIONS$TITLE)
cat("\n")
for(i in (1:length(OPTIONS))) {
      cat(paste(names(OPTIONS)[i], ": ", OPTIONS[i]))
      cat("\n")
}
cat("\n")
cat("=========================================")
cat("\n")
return(OPTIONS)
}

