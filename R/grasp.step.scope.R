"grasp.step.scope" <-
function(sX)
{
cat("#", "\n")
cat("# FUNCTION: grasp.step.scope (by A. Lehmann)", "\n")
cat("# generates automatically a model scope", "\n")
cat("#", "\n")
matchinter <- match("INTER", names(XXX)[sX])
if(!is.na(matchinter))
      sX <- sX[ - matchinter]
vnames <- names(XXX)[sX]
# cat(sX, "\n")
step.list <- as.list(vnames)
names(step.list) <- vnames
for(Xi in sX) {
      vname <- names(XXX)[Xi]
      # loops through independant variable names
      junk <- c("1")
      # minimum scope
      if(!is.na(OPTIONS$DF1[Xi]) & !is.factor(XXX[[vname]])) junk <- c("1", paste(OPTIONS$SMOOTHER, "(", vname,",", OPTIONS$DF1[Xi], ")", sep = ""))
      if(is.vector(vname))
            if(!is.factor(XXX[[vname]]))
                  junk <- c(junk, paste(OPTIONS$SMOOTHER, "(",vname, ",", OPTIONS$DF2[Xi], ")",sep = ""))
            else junk <- c(junk, vname)
      junk <- eval(parse(text = paste("~", paste(junk, collapse = "+"))))
      # collapse list of variables to create a scope line
      step.list[[vname]] <- junk
      }
step.list
}

