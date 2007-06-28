"grasp.msk.limits" <-
function(gr.Yi, sX = OPTIONS$SELXLIM, npast = OPTIONS$NPAST)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
cat("#", "\n")
cat("# FUNCTION: grasp.msk.limits", "\n")
cat("# (by J.R. Leathwick, adapted by A. Lehmann) ", "\n")
cat("# calculates the environmental envelop containing the response variable (RV)","\n")
cat("#", "\n")
gr.modmask.local <- gr.modmask
gr.predmask.local <- gr.predmask
cat("\n")
cat(" vvvvvvvvvv GRASP LIMITS vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
cat("\n")
response <- YYY[, gr.Yi]
if(length(sX)==0) {
      texterror <- "no limiting predictors were defined in GRASP OPTIONS"
      cat(texterror, "\n")
      return(texterror)
}
if(min(response) != 0) {
      cat("there is no response equal to 0, limits can not be calculated !","\n")
}
else {
      if(length(sX) > 1) {
            predictor <- XXX[, sX]
            predictor[, length(sX) + 1] <- 1:length(XXX[, 1])
      }
      else {
            predictor <- XXX[, c(sX, sX - 1)]
            predictor[, length(sX) + 1] <- 1:length(XXX[, 1])
      }
      n.numerics <- 0
      n.factors <- 0
      max.levels <- 0
      limitlabels <- NULL
      countlabels <- NULL
      for(i in 1:(length(sX))) {
            if(is.numeric(predictor[, i])) {
            # continuous variables
                  limitlabels <- append(limitlabels, names(predictor)[i], n.numerics)
                  n.numerics <- n.numerics + 1
            }
            else if(is.factor(predictor[, i])) {
                  # factor variables
                  countlabels <- append(countlabels, names(predictor)[i], n.factors)
                  n.factors <- n.factors + 1
                  max.levels <- max(max.levels, length(levels(predictor[, i])))
                  }
            }
            cat("Number of variables: ", length(sX) - 1, fill = TRUE)
            cat("Number of numerics: ", n.numerics, fill = TRUE)
            cat("Number of factors: ", n.factors, fill = TRUE)
            if(OPTIONS$LIM == "or") {
                  mask <- rep(FALSE, length(response))
                  predict.template <- rep(0, length(XXXpred[, 1]))
            }
            else {
                  mask <- rep(TRUE, length(response))
                  predict.template <- rep(1, length(XXXpred[, 1]))
            }
            if(OPTIONS$LIM == "mixte") {
                  mask2 <- rep(FALSE, length(response))
                  predict.template2 <- rep(0, length(XXXpred[, 1]))
            }
            limits <- matrix(0, nrow = n.numerics, ncol = 3)
            # dimnames(limits) <- list(limitlabels, c("lower", "upper", "mean"))
            counts <- matrix(0, nrow = n.factors, ncol = max.levels)
            # dimnames(counts) <- list(countlabels, NULL)
            n.numerics <- 0
            n.factors <- 0
            #
            #now loop through the variables
            #
            for(i in 1:(length(sX))) {
            #
            # first check to see if we have a numeric
            #
            if(is.numeric(predictor[, i])) {
                  n.numerics <- n.numerics + 1
                  #
                  #  Get the lower limit by sorting the predictor and the response
                  #  and then finding the lowest sequence with response > 0
                  #
                  sorted <- cbind(predictor[order(predictor[, i], response), i], response[order(predictor[, i], response)])
                  j <- seq(along = sorted[, 1])
                  min.pos <- min(j[sorted[, 2] > 0])
                  min.pos2 <- max(1, min.pos)
                  min.pos <- max(1, min.pos - npast)
                  lower.limit <- sorted[min.pos, 1]
                  #
                  lower.limit2 <- sorted[min.pos2, 1]
                  #
                  #  Get the upper limit by sorting the predictor and 0 minus the response
                  #  and then finding the highest sequence with response > 0
                  #
                  sorted <- cbind(predictor[order(predictor[, i], 0 - response), i], response[order(predictor[, i], 0 - response)])
                  j <- seq(along = sorted[, 1])
                  max.pos <- max(j[sorted[, 2] > 0])
                  max.pos2 <- min(length(sorted[, 1]), max.pos)
                  max.pos <- min(length(sorted[, 1]), max.pos +npast)
                  upper.limit <- sorted[max.pos, 1]
                  #
                  upper.limit2 <- sorted[max.pos2, 1]
                  #
                  #  now update the logical mask
                  #
                  if(OPTIONS$LIM == "and") {
                        mask[predictor[, i] < lower.limit] <-FALSE
                        mask[predictor[, i] > upper.limit] <-FALSE
                        k <- match(names(predictor)[i], names(XXXpred))
                        predict.template[XXXpred[, k] > upper.limit] <- 0
                        predict.template[XXXpred[, k] < lower.limit] <- 0
                  }
                  if(OPTIONS$LIM == "or") {
                        mask[(predictor[, i] > lower.limit) &(predictor[, i] < upper.limit)] <- TRUE
                        k <- match(names(predictor)[i], names(XXXpred))
                        predict.template[(XXXpred[, k] > lower.limit) & (XXXpred[,k] < upper.limit)] <- 1
                  }
                  if(OPTIONS$LIM == "mixte") {
                        mask[predictor[, i] < lower.limit2] <-FALSE
                        mask[predictor[, i] > upper.limit2] <-FALSE
                        mask2[(predictor[, i] > lower.limit) &(predictor[, i] < lower.limit2)] <- TRUE
                        mask2[(predictor[, i] > upper.limit2) &(predictor[, i] < upper.limit)] <- TRUE
                        k <- match(names(predictor)[i], names(XXXpred))
                        predict.template[XXXpred[, k] > upper.limit2] <- 0
                        predict.template[XXXpred[, k] < lower.limit2] <- 0
                        predict.template2[(XXXpred[, k] > lower.limit) & (XXXpred[,k] < lower.limit2)] <- 1
                        predict.template2[(XXXpred[, k] > upper.limit2) & (XXXpred[, k] < upper.limit)] <- 1
                  }
                  limits[n.numerics,  ] <- c(zapsmall(lower.limit, 4), zapsmall(upper.limit,4), zapsmall(mean(predictor[response >0, i]), 4))
                  cat("Lower and upper limits, and occupied mean for ",names(predictor[i]), " are ", limits[n.numerics,  ],fill = TRUE)
                  limits2 <- limits
                  limits2[n.numerics,  ] <- c(zapsmall(lower.limit2, 4), zapsmall(upper.limit2, 4), zapsmall(mean(predictor[response > 0, i]), 4))
            }
            else if(is.factor(predictor[, i])) {
                  n.factors <- n.factors + 1
                  n.levels <- length(levels(predictor[, i]))
                  counts[n.factors, 1:n.levels] <- summary(predictor[response > 0, i])
                  for(n in (1:n.levels)) {
                        if(counts[n.factors, n] == 0) {
                              cat("All cases excluded for ",names(predictor[i]),"level ", n, fill = TRUE, "\n")
                              mask[codes(predictor[, i]) ==n] <- FALSE
                              k <- match(names(predictor)[i], names(XXXpred))
                              predict.template[codes(XXXpred[, k]) == n] <- 0
                        }
                  }
            }
      }
      if(OPTIONS$LIM == "mixte") {
            mask <- mask | mask2
            predict.template <- predict.template | predict.template2
      }
      excluded <- length(mask[mask == FALSE])
      cat("Limits of models (gr.modmask) set and ", excluded, " cases excluded", fill = TRUE)
      gr.modmask.local[, gr.Yi] <- as.logical(mask)
      assign("gr.modmask", gr.modmask.local, env = .GlobalEnv)
      # used in grasp.step
      excluded2 <- length(predict.template[predict.template == FALSE])
      cat("Limits of prediction (gr.predmask) set and ", excluded2," cases excluded", fill = TRUE)
      gr.predmask.local[, gr.Yi] <- as.logical(predict.template)
      assign("gr.predmask", gr.predmask.local, env = .GlobalEnv)
      # used in grasp.pred.export
      cat("\n")
}
cat(" ********** GRASP LIMITS END ********** ", "\n")
if(OPTIONS$SINK) {
      sink()
      cat("<=>")
}
else cat("\n")
}

