"grasp.pred.lookup" <-
function(gr.Yi)
{
Yname <- names(YYY)[gr.Yi]
gr.Yi.local <- gr.Yi
if(OPTIONS$SINK) {
      # if TRUE results are saved to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.pred.lookup", "\n")
cat("# (by S. Barry using SPlus predict.gam function", "\n")
cat("# adapted by A. Lehmann)", "\n")
cat("# predicts the response variable in a lookup table from an artificial dataframe representing","\n")
cat("# the predictor variables range", "\n")
cat("#", "\n")
if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0) {
      cat("\n")
      cat("### INFO: lut cannot be exported from EMPTY models","\n")
      sink()
      return("")
}

assign("gr.Yi", gr.Yi.local, env = .GlobalEnv)
term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
mod.terms <- dimnames(term.matrix)[[2]]
ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
mod.selX <- NULL
for(pos1 in 2:length(names(XXX)))
if(length(grep(paste(wc, names(XXX)[pos1], wc, sep = ""),mod.terms)) > 0) mod.selX <- c(mod.selX, pos1)
cat("\n")
cat(" vvvvvvvvvv GRASP LOOKUP vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
artificial <- as.data.frame(matrix(rep(1, 200 * length(mod.selX)),200, length(mod.selX)))
if(exists("XXXlut"))print("!!! INFO: lookup tables based on XXXlut")
      else print("!!! INFO: lookup tables based on min and max of XXXpred")
            # dataframe for prediction
flist <- as.list(rep(0,length(names(XXX))))
# factor list
for(i in 1:length(mod.selX))
                  if(!is.factor(XXX[, mod.selX[i]])) {
                        print(names(XXX)[mod.selX[i]])
                        if(exists("XXXlut")) {
                              artificial[, i] <- seq((XXXlut[1, mod.selX[i]]), (XXXlut[2, mod.selX[i]]), length = 200)
                              print("test2")
                              }
                        else {
                              artificial[, i] <- seq(min(XXXpred[, mod.selX[i]]), max(XXXpred[, mod.selX[i]]),length = 200)
                        }
                  }
                  else {
                        print(names(XXX)[mod.selX[i]])
                        # create a  vector from the minimum to the maximum level of each factor
                        cat("INFO: !!! your factor map in Arcview will have to be reclassify according to the factor levels presented here:","\n")
                        # print(summary(XXX[gr.modmask[, gr.Yi], mod.selX[i]]))
                        print(levels(XXX[gr.modmask[, gr.Yi], mod.selX[i]]))
                        print(1:length(levels(XXX[gr.modmask[, gr.Yi], mod.selX[i]])))
            
                        for(f in 1:length(levels(XXX[, mod.selX[i]]))) {
                              if(summary(XXX[gr.modmask[, gr.Yi], mod.selX[i]])[f][1] > 0) {
                                    # test if factor level is represented
                                    artificial[f, i] <- levels(XXX[, mod.selX[i]])[f]
                                    flist[[mod.selX[i]]] <- c(flist[[mod.selX[i]]], f)
                              }
                        }
                        flist[[mod.selX[i]]] <- flist[[mod.selX[i]]][-1]
                        for(f in 1:length(levels(XXX[, mod.selX[i]]))) {
                              if(summary(XXX[gr.modmask[, gr.Yi], mod.selX[i]])[f][1] == 0)
                                    artificial[f, i] <- levels(XXX[, mod.selX[i]])[flist[[mod.selX[i]]][1]]
                        }
                        artificial[ - flist[[mod.selX[i]]], i] <- levels(XXX[, mod.selX[i]])[flist[[mod.selX[i]]][1]]
                        # fills the rest of the 200 values with a known level 
                        artificial[, i] <- as.factor(artificial[, i])
                  }
                  print(flist)

                  dimnames(artificial) <- list(1:200, names(XXX)[mod.selX])
                  cat("LOOKUP TABLE FOR PREDICTION:", "\n")
                  cat("first five predictor values:", "\n")
                  print(artificial[c(1:5),  ])
                  cat("last five predictor values :", "\n")
                  print(artificial[c(196:200),  ])
                  # assign("artificial", artificial, frame = 1)
                  LUT <- artificial
                  term.matrix <- predict.gam(ModelYi, artificial, se.fit = FALSE, type = "terms")
                  # predict using type="terms"
                  #  Set up for output to ARCVIEW using grasp.pred.lookup Avenue script
                  mod.terms <- dimnames(term.matrix)[[2]]
                  j <- 0
                  testglm <- FALSE
                  for(term1 in names(XXX)[mod.selX]) {
                  pos1 <- grep(term1, names(XXX)[mod.selX])
                  # position among selected X for all terms
                  pos3 <- pmatch(term1, names(XXX))
                  # position among XXX terms
                  testx <- "nomatch"
                  if(!is.factor(XXX[, pos3])) {
                        if(!is.na(pmatch(paste("s(", term1, sep = ""), mod.terms)))
                              testx <- paste("s(", term1, sep = "")
                        if(!is.na(pmatch(paste("poly(", term1, sep = ""),mod.terms))) {
                              testx <- paste("poly(", term1, sep = "")
                              testglm <- TRUE
                        }
                  }
                  pos2 <- pmatch(testx, mod.terms)
                  # position among selected X for continuous terms
                  if(!(pmatch(testx, mod.terms, nomatch = "nomatch") == "nomatch")) {
                        # test if the variable is in the model
                        LUT[1:200, pos1] <- zapsmall(term.matrix[1:200, pos2],6)
                        # plot(zapsmall(term.matrix[1:200, pos2], 6),1:200)     
                        j <- j + 1
                  }
                  else {
                        LUT[, pos1] <- (LUT[, pos1])
                        LUT[1:200, pos1] <- rep(0, 200)
                  }
                  if(!is.factor(XXX[, pos3])) {
                        LUT[201, pos1] <- min(XXX[gr.modmask[, gr.Yi], pos3])
                        # adds the minimum value in XXX with limits
                        LUT[202, pos1] <- max(XXX[gr.modmask[, gr.Yi], pos3])
                        # adds the maximum value in XXX with limits
                        LUT[203, pos1] <- 200
                        #adds the number of observations in each vector
                        LUT[204, pos1] <- zapsmall(attr(term.matrix, "constant"), 6)
                        if(exists("XXXlut")) {
                              LUT[205, pos1] <- min(XXXlut[,pos3])
                              LUT[206, pos1] <- max(XXXlut[,pos3])}
                        else {
                              LUT[205, pos1] <- min(XXXpred[gr.predmask[, gr.Yi], pos3])
                              LUT[206, pos1] <- max(XXXpred[gr.predmask[, gr.Yi], pos3])}
                              # adds the minimum value of XXX
                              # adds the maximum value of XXXpred
                        }
                 else {
                  # if factor
                  pos4 <- pmatch(term1, mod.terms)
                  # position among terms in selected model
                  LUT[, pos1] <- as.double(LUT[, pos1])
                  if(!is.na(pmatch(term1, mod.terms))) {
                        # test if the variable is in the model
                        print(term1)
                        # cat("FACTOR IN", "\n") #  cat(paste("j:",j,"pos4:",pos4))
                        LUT[1:200, pos1] <- zapsmall(term.matrix[1:200, pos4], 6)
                        LUT[ - flist[[mod.selX[pos1]]], pos1] <- 0
                        #else fills the vector with 0s
                        j <- j + 1
                        ranklevel <- 1
                 }
                 else {
                        cat(term1, "\n")
                        cat("FACTOR OUT", "\n")
                        LUT[1:200, pos1] <- rep(0, 200)
                  }
                  LUT[201, pos1] <- 1
                  # adds minimum level
                  LUT[202, pos1] <- length(levels(XXX[, pos3]))
                  # adds maximum level
                  LUT[203, pos1] <- length(levels(XXX[, pos3]))
                  # adds maximum level
                  LUT[204, pos1] <- zapsmall(attr(term.matrix, "constant"), 6)
                  LUT[205, pos1] <- 1
                  # adds minimum level
                  LUT[206, pos1] <- length(levels(XXXpred[, pos3]))
                  # adds maximum level
            }
      }
      cat("first five contributions:", "\n")
      print(LUT[c(1:5),  ])
      cat("last five contributions:", "\n")
      print(LUT[c(196:200),  ])
      cat("LUT bootstrapped standard error:", "\n")
      cat("", "\n")
      cat("LUT201: minimum values of XXX if masked:", "\n")
      print(LUT[201,  ])
      cat("", "\n")
      cat("LUT202: maximum values of XXX if masked:", "\n")
      print(LUT[202,  ])
      cat("", "\n")
      cat("LUT203: number of cases used in LUT", "\n")
      print(LUT[203,  ])
      cat("", "\n")
      cat("LUT204: intercept of model", "\n")
      print(LUT[204, 1])
      cat("LUT205: minimum values of XXXpred or XXXlut:", "\n")
      print(LUT[205,  ])
      cat("", "\n")
      cat("LUT206: maximum values of XXXpred or XXXlut:", "\n")
      print(LUT[206,  ])
      cat("", "\n")
      pathlut <- paste(OPTIONS$PATHLUT, Yname, "_lut.txt", sep = "")
      write.table(cbind(1:206,LUT), file = pathlut, sep = "\t", quote=FALSE,col.names=c("row.names",names(LUT)), row.names=FALSE)
      cat("\n")
      cat("!!! INFO: Lookup Table saved in", pathlut, "\n")
      cat("\n")
      cat(" ********** GRASP LOOKUP END ********** ", "\n")
      cat("\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

