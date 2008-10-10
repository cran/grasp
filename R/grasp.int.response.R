"grasp.int.response" <-
function(sY, sX)
{
if (!OPTIONS$PNG) dev.new()
else {png(filename = paste("resp" ,".png",sep=""), width = 800, height = 800, res=150)}

cat("#", "\n")
cat("# FUNCTION: grasp.int.response (by A. Lehmann)", "\n")
cat("# plots combined response curves of selected response variable along selected Predictor gradients","\n")
cat("# using different methods (mean, max, optima, presence, mixte)","\n")
cat("#", "\n")
grasp.pred.lookup2 <- function(gr.Yi)
{
      # predicts the response variable in a lookup table from an artificial dataframe representing 
      # the predictor variables range
      # 
      ModelYi2 <- eval(MODELCALLS[[gr.Yi]]$call)
      term.matrix <- predict.gam(ModelYi2, XXX[1:10,  ], se.fit = FALSE,type = "terms")
      mod.terms <- dimnames(term.matrix)[[2]]
      selX <- NULL
      for(name in names(XXX))
            if(length(grep(name, mod.terms)) > 0) {
                  pos <- pmatch(name, names(XXX))
                  selX <- unique(c(selX, pos))
            }
      assign("gr.selX", selX, env = .GlobalEnv)
      artificial <- as.data.frame(matrix(rep(1, 200 * length(gr.selX)), 200, length(gr.selX)))
      # dataframe for prediction

      flist <- as.list(rep(0,length(names(XXX))))
      # factor list
      for(i in 1:length(gr.selX))
            if(!is.factor(XXX[, gr.selX[i]])) {
                  artificial[, i] <- seq(min(XXX[, gr.selX[i]], na.rm = TRUE), max(XXX[, gr.selX[i]], na.rm = TRUE), length = 200)
            }
            else {
            # create a  vector from the minimum to the maximum level of each factor
                  for(f in 1:length(levels(XXX[, gr.selX[i]]))) {
                        if(summary(XXX[gr.modmask[, gr.Yi], gr.selX[i]])[f][1] > 0) {
                        # test if factor level is represented
                              artificial[f, i] <- levels(XXX[, gr.selX[i]])[f]
                              flist[[gr.selX[i]]] <- c(flist[[gr.selX[i]]], f)
                        }
                  }
                  flist[[gr.selX[i]]] <- flist[[gr.selX[i]]][-1]
                  for(f in 1:length(levels(XXX[, gr.selX[i]]))) {
                        if(summary(XXX[gr.modmask[, gr.Yi], gr.selX[i]])[f][1] == 0)
                              artificial[f, i] <- levels(XXX[, gr.selX[i]])[flist[[gr.selX[i]]][1]]
                  }
                  artificial[ - flist[[gr.selX[i]]], i] <- levels(XXX[, gr.selX[i]])[flist[[gr.selX[i]]][1]]
                  # fills the rest of the 200 values with a known level 
                  artificial[, i] <- as.factor(artificial[, i])
            }

            dimnames(artificial) <- list(1:200, names(XXX)[gr.selX])
            LUT <- artificial
            if(length(grep("~ 1", as.character(ModelYi2$call))) > 0)
                  next
            term.matrix <- predict(ModelYi2, artificial, type = "terms")
            # predict using type="terms"
            #  Set up for output to ARCVIEW using grasp.pred.lookup Avenue script
            mod.terms <- dimnames(term.matrix)[[2]]
            for(term1 in names(XXX)[gr.selX]) {
                  pos1 <- grep(term1, names(XXX)[gr.selX])
                  # position among selected X for all terms
                  testx <- paste("s(", term1, ",", sep = "")
                  pos2 <- pmatch(testx, mod.terms)
                  # position among selected X for smoothed terms
                  if(!is.na(pmatch(testx, mod.terms))) 
                        LUT[1:200, pos1] <- zapsmall(term.matrix[1:200, pos2],6) else LUT[1:200, pos1] <- rep(0,200)
                  pos3 <- match(term1, names(XXX))
                  # position among XXX terms
                  if(!is.factor(XXX[, pos3])) {
                        LUT[201, pos1] <- min(XXX[gr.modmask[, gr.Yi],pos3])
                        # adds the minimum value in XXX with limits
                        LUT[202, pos1] <- max(XXX[gr.modmask[, gr.Yi],pos3])
                        # adds the maximum value in XXX with limits
                        LUT[203, pos1] <- 200
                        #adds the number of observations in each vector
                        LUT[204, pos1] <- zapsmall(attr(term.matrix,"constant"), 6)
                  }
                  else {
                  # if factor
                        pos4 <- match(term1, mod.terms)
                        # position among terms in selected model
                        LUT[, pos1] <- as.single(LUT[, pos1])
                        if(!is.na(match(term1, mod.terms))) {
                        # test if the variable is in the model
                              LUT[1:200, pos1] <- zapsmall(term.matrix[1:200, pos4],6)
                              LUT[ - flist[[gr.selX[pos1]]], pos1] <- 0
                        }
                        else LUT[1:200, pos1] <- rep(0, 200)
                        LUT[201, pos1] <- 1
                        # adds minimum level
                        LUT[202, pos1] <- length(levels(XXX[, pos3]))
                        # adds maximum level
                        LUT[203, pos1] <- length(levels(XXX[, pos3]))
                        # adds maximum level
                        LUT[204, pos1] <- zapsmall(attr(term.matrix,"constant"), 6)
                  }
            }
            return(list(LUT = LUT, artificial = artificial))
}
#      
# of grasp.pred.lookup2
#
cat("\n")
cat(" vvvvvvvvvv GRASP RESPONSE PLOT vvvvvvvvvv ", "\n")
cat(date(), "\n")
par(mfrow = OPTIONS$PLOTPAR)
countX <- 1
continuousX <- 0
for(i in gr.selX) {
      if(!is.factor(XXX[, i]))
            continuousX <- continuousX + 1
}
resplim <- c(round(min(YYY[, sY], na.rm = TRUE)), round(max(YYY[, sY],na.rm = TRUE)))
# calculates maximum observed responses
for(sX in 1:length(gr.selX)) {
      Xi <- gr.selX[sX]
      if(!is.factor(XXX[, Xi])) {
            # loops through each continuous variable to create combined response curves
            N <- 200
            # number of point calcutaled along response curves 
            countY <- 1
            for(gr.Yi in sY) {
                  if(length(grep("~ 1", as.character(MODELCALLS[[gr.Yi]]$call))) > 0)
                        next
                  term.matrix <- predict.gam(eval(MODELCALLS[[gr.Yi]]$call), XXX[1:10,  ], se.fit = FALSE, type = "terms")
                  mod.terms <- dimnames(term.matrix)[[2]]
                  ModelYi <- eval(MODELCALLS[[gr.Yi]]$call)
                  LOOKUP <- grasp.pred.lookup2(gr.Yi)
                  lookX <- LOOKUP$artificial
                  lookCste <- LOOKUP$LUT[204, 1]
                  thefamily <- ModelYi$family
                  if(thefamily[1] == "poisson" | thefamily[1] == "quasipoisson")
                        lookY <- exp(LOOKUP$LUT[1:200,  ] + lookCste)
                  else if(thefamily[1] == "binomial" | thefamily[1] == "quasibinomial" )
                              lookY <- exp(LOOKUP$LUT[1:200,  ] + lookCste)/(1 + exp(LOOKUP$LUT[1:200,  ] + lookCste))
                        else lookY <- LOOKUP$LUT[1:200,  ] + lookCste
                  if(countY == 1) {
                  # prepare plot with first Y
                        plot(lookX[, sX], rep(0, N), pch = " ",ylim = resplim, ylab = "Response", xlab = names(LOOKUP$LUT)[[sX]], type = "n")
                  }
                  thetest1 <- length(grep(paste(names(XXX)[Xi], ",", sep = ""), as.character(ModelYi$call)))
                  if(thetest1 > 0) {
                        cat("Xi:", names(XXX)[Xi])
                        # cat(" , sX:", sX)
                        cat(" , Yi:", names(YYY)[gr.Yi])
                        cat("\n")
                        # tests whether the selected X was contributing to explaining the selected Y
                        lines(lookX[, sX], lookY[, sX], type = "l", pch = countY, lty = countY, lwd = 0.5, cex = 0.4)
                        posonline <- round(countY * (200/length(sY)))
                        points(lookX[posonline, sX], lookY[posonline, sX], pch = countY,cex = 0.7)
                  }# if
                  if(countX == continuousX & countY == length(sY)) {
                  rangeX <- (max(XXX[, Xi], na.rm = TRUE) -min(XXX[, Xi], na.rm = TRUE))
                  legend(x = max(XXX[, Xi], na.rm = TRUE) + 0.2 * rangeX,y = max(YYY[, gr.Yi], na.rm = TRUE), legend= names(YYY)[sY], pch = 1: length(sY), ncol = 1, lwd = 0.5, cex = 0.7, col = 1)
                  #  legend(x = 3, y = 1, names(YYY)[c(3,4,5)], marks = 1:3, ncol = 1, lwd = 0.5, cex = 0.7, col = 1)

                  par(mfrow = c(1, 1))
                  mtext(paste(OPTIONS$TITLE, " combined repsonses curves"),line = 3, outer = FALSE, side = 3,cex = 1)
                  #  mtext(paste("Correlation limit: ", OPTIONS$CORLIM, ", Test: ", OPTIONS$TEST, ", Smoother DF: ", OPTIONS$DF2[Xi], ", ", OPTIONS$DF1[Xi]), line = 4, outer = F, side = 1, cex = 0.5) 
                  # adds a legend on the right of the last graph
                  par(mfrow = OPTIONS$PLOTPAR)
            }
            countY <- countY + 1
      }
      # for gr.Yi
      countX <- countX + 1
      }
}
# for sX
if(OPTIONS$PNG) dev.off()
cat(" ********** GRASP RESPONSE END ********** ", "\n")
cat("\n")
}

