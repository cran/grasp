"grasp.step.rocplot" <-
function(z1, z2)
{
# source("d:\\s_lib\\newfunc\\rocplot.fun")
# Fonction roc.plot written by Antoine Guisan (Swiss Center for Faunal Cartography, 1999)
# See table 2 in Fielding & Bell (1997), Environmental Conservation 24(1): 41 
# se = sensitivity, sp = specificity; make use of the roc.plot.auc function
eva <- data.frame(seuil=0,se=1,sp=0)
k <- 0.01
i <- 2
while(k < 0.9) {
      conttable <- table(z1 >= k, z2)
      #print(conttable)
      if(length(conttable) < 4) a <- 0 else a <- conttable[4]
            b <- conttable[2]
      if(length(conttable) < 4)
            c <- 0
      else c <- conttable[3]
      d <- conttable[1]
      eva[i, "seuil"] <- k
      eva[i, "se"] <- ifelse(is.na(a/(a + c)), 0, a/(a + c))
      eva[i, "sp"] <- d/(b + d)
      k <- k + 0.01
      i <- i + 1
}
eva[i, "seuil"] <- 1
eva[i, "se"] <- 0
eva[i, "sp"] <- 1
assign("gr.EVA.ROC", eva[eva$se>0,], env = .GlobalEnv)
x<- eva[eva$se>0,]
X <- data.frame(sens = x$se, spec = x$sp, eenminspec = 1 -x$sp, diffsens = rep(0, length = length(x$se)), diffspec = rep(0, length = length(x$sp)), area = rep(0, length = length(x$se)))
X$diffsens[ - (nrow(x))] <- X$sens[2:(nrow(X))] - X$sens[1:(nrow(X) - 1)]
X$diffspec[ - (nrow(x))] <- X$eenminspec[2:(nrow(X))] - X$eenminspec[1:(nrow(X) - 1)]
X$area <- X$sens * X$diffspec + 0.5 * X$diffsens * X$diffspec
return(auc=-sum(X$area))
}

