\name{grasp}
\alias{grasp}
\title{ Generalized Regression Analysis and Spatial Prediction }
\description{
  GRASP is a general method for making spatial predictions of response variables (RV) using point surveys of the RV and spatial coverages of Predictor variables (PV). GRASP uses Trevor Hastie's gam library.
}
\usage{
grasp(selected.responses, selected.predictors, title = "GRASP: ", path = "", save.outputs = FALSE, save.PNG = FALSE, 
gr.fam = "binomial", weights = FALSE, set.limits = FALSE, use.limits = FALSE, reset.lim = TRUE, selxlim = gr.selX, npast = 10,
make.summary = FALSE, plot.maps = FALSE, plot.distry = FALSE, plot.histograms = FALSE, plot.respvspred = FALSE, plot.xpred = FALSE,
plot.correlation = FALSE, corlim = 100, use.correlation = FALSE, reset.cor = TRUE, full.models = FALSE, stepwise.models = FALSE, 
test = "F", plimit = 0.05, direction = "both", df1 = 0, df2 = 4, startwith = gr.selX, resetstart = TRUE,
contributions = FALSE, plot.contributions = FALSE, plot.models = FALSE, response.curves = FALSE, model.anova = FALSE, validate.models = FALSE, 
predictions = FALSE, export.predictions = FALSE, plot.predictions = FALSE, lookup.tables = FALSE, 
plotpar = c(3, 3), prescol= "black", abscol="grey", nbar = 10, show.details = FALSE, StdError = TRUE, cvgroups = 5, resolution = as.integer((max(XXXpred$CX) - min(XXXpred$CX)) / (length(unique(XXXpred$CX)) - 1)) )
}
\arguments{
  \item{selected.responses}{ Vector of column numbers or column names of YYY table, which contains response variables }
  \item{selected.predictors}{ Vector of column numbers or column names of XXX table, which contains predictor variables }
  \item{title}{ Title used in GRASP outputs }
  \item{path}{ Working directory path }
  \item{save.outputs}{ Send outputs for each response to a specific log file in your working directory }
  \item{save.PNG}{ Save graphs as PNG files in your working directory }
  \item{gr.fam}{ Choice of statistical distribution for Ys: "binomial", "poisson", "quasibinomial", "quasipoisson", "gaussian"}
  \item{weights}{ Recalculate weights of 0s so that their sum is equal to sum of 1s }
  \item{set.limits}{ Calculate and save limits of the environmental envelop of each responses (Ys) }
  \item{use.limits}{ Use the saved limits in anylyses }
  \item{reset.lim}{ Reset saved limits }
  \item{selxlim}{ Vector of column numbers or column names of XXX table used to define limits }
  \item{npast}{ Number of absences (0s) kept after the last presence (1) along each selected predictors used for calculating limits }
  \item{make.summary}{ Calculate the data summary of selected Ys and Xs }
  \item{plot.maps}{ Plot the spatial distribution of Ys }
  \item{plot.distry}{ Histograms of the distribution of Ys }
  \item{plot.histograms}{ Plot the distribution of Ys on histograms of Xs }
  \item{plot.respvspred}{ Plot the relationship between Ys and Xs }
  \item{plot.xpred}{ maps the predictors contained in XXXpred  }
  \item{plot.correlation}{ Calculate the correlations between Xs and eliminate Xs from candidate predictors if maximum correlation has been set with corlim}
  \item{corlim}{ Maximum correlation allowed between predictors, if higher correlation found predictors are withdrawn from Xs candidate list. If correlated, the last selected Xs is removed }
  \item{use.correlation}{ Select a model using only the uncorrelated predictors selected earlier }
  \item{reset.cor}{ resetting candidate predictors (Xs) to all selected Xs  }
  \item{full.models}{ Select a model for each Ys forcing all candidate Xs in the models }
  \item{stepwise.models}{ Select a model for each Ys from candidate Xs using the different possible options below }
  \item{test}{ Choice of criterias (AIC, BIC) or tests (CHI and F (for quasi models))  }
  \item{plimit}{ Probability limits for CHI and F tests }
  \item{direction}{ Set the direction of selection, default is both }
  \item{df1}{ Optional smoother degree of freedom to be tested in variable selection }
  \item{df2}{ First smoother degree of freedom to be tested in variable selection }
  \item{startwith}{ Vector of column numbers of predictors used to start stepwise procedure }
  \item{resetstart}{ Resetting starting predictors (Xs) to all selected Xs }
  \item{contributions}{ Calculate contributions of univariate models (alone), when dropping each predictor from the selected model (drop) and within the model (model) }
  \item{plot.contributions}{ Plot the calculated contributions as bars }
  \item{plot.models}{ Plot response curves of the selected models (using linear predictor scale before transformation into the response scale) }
  \item{response.curves}{ Plot combined response curves of the selected models (using response scale after transformation by the inverse link function) }
  \item{model.anova}{ Calculate an ANOVA table by removing each term from the full model }
  \item{validate.models}{ Validate the selected models for each Ys using both simple validation and cross-validation }
  \item{predictions}{ Calculate predictions for each Ys using selected model and new data (XXXpred) and save them in gr.predmat }
  \item{export.predictions}{ Export saved predictions from gr.predmat to a text file that can be directly imported in Arcview as an ASCII GRID }
  \item{plot.predictions}{ Plot saved predictions from gr.predmat }
  \item{lookup.tables}{ Calculate lookup tables describing partial responses curves at the linear predictor scale for each Ys using minimum and maximum values for each Xs contained in XXXpred. These values are derived from min and max values of the GRIDs that are used to build spatial predictions in Arcview. }
  \item{plotpar}{ number of rows and columns for multiple graphical outputs }
  \item{prescol}{ Color for presences in graph outputs }
  \item{abscol}{ Color for absences in graph outputs }
  \item{nbar}{ Number of bars in histograms }
  \item{show.details}{ Verbose output of stepwise selection }
  \item{StdError}{ Show pointwise error around partial plot }
  \item{cvgroups}{ Number of groups for cross validation }
  \item{resolution}{ Resolution for gids maps }
}
\value{
  \item{MODELCALLS }{Calls of selected models}
  \item{VALIDATION }{Validation statistics for selected models}
  \item{ALONE.CONTRIB }{Predictor alone contributions for selected models}
  \item{DROP.CONTRIB }{Predictor drop contributions for selected models}
  \item{MODEL.CONTRIB }{Predictor model contributions for selected models}
}
\references{ \item A. Lehmann, J.R. Leathwick & J.McC. Overton, 2002. GRASP. Ecological Modelling, 157: 189-207
\item HOMEPAGE: http://www.unige.ch/ia/climate/grasp
\item RELATED: http://www.spacedoor.net
}

\author{Anthony.Lehmann@unige.ch }

\seealso{grasp.in }
\examples{

data(YYY) # reads in YYY,XXX and XXXpred demo dataset
data(XXX)
data(XXXpred)
grasp.in(YYY,XXX,XXXpred) # initialize a new grasp session

grasp(2:3,c(4:6,8:9), title = "GRASP: ", path = "", gr.fam = "binomial", weights = TRUE, make.summary = TRUE, plot.maps = TRUE, plot.distry = TRUE, plot.histograms = TRUE, plot.respvspred = TRUE, plot.xpred = TRUE,plot.correlation = TRUE, stepwise.models = TRUE, test = "AIC", contributions = TRUE, plot.contributions = TRUE, plot.models = TRUE, model.anova = TRUE, validate.models = TRUE, predictions = TRUE, plot.predictions = TRUE)
# run a full grasp analysis on 2 responses with 5 predictors

}
\keyword{models}
