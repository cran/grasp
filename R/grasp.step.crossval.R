"grasp.step.crossval" <-
function(tested.model)
{
Yname <- names(YYY)[gr.Yi]
if(OPTIONS$SINK) {
      # if TRUE save results to a text file
      path <- paste(OPTIONS$PATHLOG, "log_", Yname, ".txt", sep = "")
      sink(path, append = TRUE)
}
# cat("# FUNCTION: grasp.step.crossval: ")
# assign("gr.Yi", gr.Yi, where = 1, immediate=T)
SUBSETS <- NULL
if(!exists("PLOTCROSS.VAL")) {
      assign("PLOTCROSS.VAL", YYY[1,  ], env = .GlobalEnv)
      assign("PLOTCROSS.CVAL", YYY[1,  ], env = .GlobalEnv)
}
subsets <- rep(1:OPTIONS$CVGROUPS, length.out = length(XXX[, 1]))
temppredict <- rep(NA, length(XXX[, 1]))
# initializes a prediction vector
# generates random integer values bewtween 1 and the number of desired groups
for(Nloop in 1:OPTIONS$CVGROUPS) {
      # loops through the number of groups
      SUBSETS$sub1 <- subsets != Nloop
      # First subset with group number = Nloop
      if(OPTIONS$CVGROUPS == 1) SUBSETS$sub1 <- !SUBSETS$sub1
      # tests if there is only one group (special case)
      # reverses subset 1
      SUBSETS$sub1 <- (as.logical(gr.modmask[, gr.Yi]) & SUBSETS$sub1)
      # to take gr.modmask into account 
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      tempmodel <- update(tested.model, subset = SUBSETS$sub1)
      # calculates the model without the Nloop group
      if(OPTIONS$CVGROUPS == 1) 
            SUBSETS$sub2 <- SUBSETS$sub1
      else 
            SUBSETS$sub2 <- (as.logical(gr.modmask[, gr.Yi]) & !SUBSETS$sub1)
      assign("SUBSETS", SUBSETS, env = .GlobalEnv)
      temppredict[SUBSETS$sub2] <- predict.gam(tempmodel, XXX[SUBSETS$sub2,  ], type = "response")
      }
      temp1 <- YYY[!is.na(temppredict), gr.Yi]
      temp2 <- temppredict[!is.na(temppredict)]
      BIN <- family(tested.model)[[2]] == "Logit: log(mu/(1 - mu))"
      if(BIN)
      if(any((YYY[gr.modmask[, gr.Yi], gr.Yi] > 0) & (YYY[gr.modmask[, gr.Yi], gr.Yi] < 1)))
      BIN <- FALSE
      if(BIN) {
            cvROC <- grasp.step.rocplot(temp2, temp1)
            assign("gr.CrossSel", cvROC, env = .GlobalEnv)
            assign("gr.statname", "ROC", env = .GlobalEnv)
      }
      else {
            cvCOR <- cor(temp1, temp2)
            assign("gr.CrossSel", cvCOR, env = .GlobalEnv)
            assign("gr.statname", "COR", env = .GlobalEnv)
      }
      CVGROUPS <- 1
      subsets <- rep(1:CVGROUPS, length.out = length(XXX[, 1]))
      temppredict <- rep(NA, length(XXX[, 1]))
      # initializes a prediction vector
      # generates random integer values bewtween 1 and the number of desired groups
      for(Nloop in 1:CVGROUPS) {
            # loops through the number of groups
            SUBSETS$sub1 <- subsets != Nloop
            # First subset with group number = Nloop
            if(CVGROUPS == 1) SUBSETS$sub1 <- !SUBSETS$sub1
            # tests if there is only one group (special case)
            # reverses subset 1
            SUBSETS$sub1 <- (as.logical(gr.modmask[, gr.Yi]) & SUBSETS$sub1)
            # to take gr.modmask into account 
            assign("SUBSETS", SUBSETS, env = .GlobalEnv)
            tempmodel <- update(tested.model, subset = SUBSETS$sub1)
            # calculates the model without the Nloop group
            if(CVGROUPS == 1) SUBSETS$sub2 <- SUBSETS$sub1 else SUBSETS$sub2 <- (as.logical(gr.modmask[, gr.Yi]) &!SUBSETS$sub1)
            assign("SUBSETS", SUBSETS, env = .GlobalEnv)
            temppredict[SUBSETS$sub2] <- predict.gam(tempmodel, XXX[SUBSETS$sub2,  ], type = "response")
            }
            temp1 <- YYY[!is.na(temppredict), gr.Yi]
            temp2 <- temppredict[!is.na(temppredict)]
            if(BIN) {
                  nbgroups <- 10
                  ROC <- grasp.step.rocplot(temp2, temp1)
                  assign("gr.StatSel", ROC, env = .GlobalEnv)
            }
            else {
                  COR <- cor(temp1, temp2)
                  assign("gr.StatSel", COR, env = .GlobalEnv)
            }
            if(OPTIONS$SINK) {
                  sink()
                  cat("<=>")
            }
}

