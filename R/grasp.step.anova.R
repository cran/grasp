"grasp.step.anova" <-
function (object, scope, scale, direction = c("both", "backward", 
    "forward"), trace = TRUE, keep = NULL, steps = 1000, ...) 
{
cat("#", "\n")
cat("# FUNCTION: grasp.step.anova", "\n")
cat("# (by Splus, adapted by A. Lehmann from step.gam)", "\n")
cat("# grasp.step.anova is a modified version of step.gam of Splus using ANOVA based on Chi or F tests instead ",
"\n")
cat("# of AIC criteria", "\n")
cat("#", "\n")
LIMIT <- OPTIONS$P.limit

    scope.char <- function(formula) {
        tt <- terms(formula)
        tl <- attr(tt, "term.labels")
        if (attr(tt, "intercept")) 
            c("1", tl)
        else tl
    }
    re.arrange <- function(keep) {
        namr <- names(k1 <- keep[[1]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
            namc))
    }
    untangle.scope <- function(terms, regimens) {
        a <- attributes(terms)
        response <- deparse(a$variables[[2]])
        term.labels <- a$term.labels
        if (!is.null(a$offset)) {
            off1 <- deparse(a$variables[[a$offset]])
        }
        nt <- length(regimens)
        select <- integer(nt)
        for (i in seq(nt)) {
            j <- match(regimens[[i]], term.labels, 0)
            if (any(j)) {
                if (sum(j > 0) > 1) 
                  stop(paste("The elements of a regimen", i, 
                    "appear more than once in the initial model", 
                    sep = " "))
                select[i] <- seq(j)[j > 0]
                term.labels <- term.labels[-sum(j)]
            }
            else {
                if (!(j <- match("1", regimens[[i]], 0))) 
                  stop(paste("regimen", i, "does not appear in the initial model", 
                    sep = " "))
                select[i] <- j
            }
        }
        if (length(term.labels)) 
            term.labels <- paste(term.labels, "+")
        if (!is.null(a$offset)) 
            term.labels <- paste(off1, term.labels, sep = " + ")
        return(list(response = paste(response, term.labels, sep = " ~ "), 
            select = select))
    }
make.step <- function(models, fit, scale, object)
{
      chfrom <- sapply(models, "[[", "from")
      chfrom[chfrom == "1"] <- ""
      chto <- sapply(models, "[[", "to")
      chto[chto == "1"] <- ""
      dev <- sapply(models, "[[", "deviance")
      df <- sapply(models, "[[", "df.resid")
      ddev <- c(NA, diff(dev))
      ddf <- c(NA, diff(df))
      pvalue <- sapply(models, "[[", "pvalue")
      heading <- c("Stepwise Model Path \nAnalysis of Deviance Table","\nInitial Model:", deparse(as.vector(formula(object))), "\nFinal Model:", deparse(as.vector(formula(fit))), paste("\nScale: ", format(scale), "\n", sep = ""))
      aod <- data.frame(From = chfrom, To = chto, Df = ddf, Deviance= ddev, "Resid. Df" = df, "Resid. Dev" = dev, pvalue= pvalue, check.names = FALSE)
      fit$anova <- as.anova(aod, heading)
      fit
}

    direction <- match.arg(direction)
    if (missing(scope)) 
        stop("you must supply a scope argument to step.gam(); the gam.scope() function might be useful")
    if (!is.character(scope[[1]])) 
        scope <- lapply(scope, scope.char)
    response <- untangle.scope(object$terms, scope)
    form.y <- response$response
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    items <- response$select
    family <- family(object)
    Call <- object$call
    term.lengths <- sapply(scope, length)
    n.items <- length(items)
    visited <- array(FALSE, term.lengths)
    visited[array(items, c(1, n.items))] <- TRUE
    if (!is.null(keep)) {
        keep.list <- vector("list", length(visited))
        nv <- 1
    }
    models <- vector("list", length(visited))
    nm <- 2
    form.vector <- character(n.items)
    for (i in seq(n.items)) form.vector[i] <- scope[[i]][items[i]]
          form <- deparse(object$formula)
    if (trace) 
        cat("Start: ", form)
    fit <- object
    n <- length(fit$fitted)
    if (missing(scale)) {
        famname <- family$family["name"]
        scale <- switch(famname, Poisson = 1, Binomial = 1, deviance.lm(fit)/fit$df.resid)
    }
    else if (scale == 0) 
      ##bAIC <- fit$AIC
    scale  <- deviance.lm(fit)/fit$df.resid
    bAIC <- deviance(fit) + 2 * (n - fit$df.resid) * scale

      if(trace)
            cat("; p limit=", LIMIT, "\n")
      models[[1]] <- list(deviance = deviance(fit), df.resid = fit$df.resid,
      pvalue = LIMIT, from = "", to = "")
      if (!is.null(keep)) {
           keep.list[[nv]] <- keep(fit, bAIC)
           nv <- nv + 1
      }
      STOP <- 1
      bFRED <- 1

      while(STOP > LIMIT & steps > 0) {
            steps <- steps - 1
            STOP <- bFRED
            bFRED <- LIMIT
            kFRED <- LIMIT
            bitems <- items
            bfit <- fit
            Nstep <- 0

        for (i in seq(n.items)) {
            if (backward) {
                trial <- items
                trial[i] <- trial[i] - 1
                if (trial[i] > 0 && !visited[array(trial, c(1, n.items))]) {
                  visited[array(trial, c(1, n.items))] <- TRUE
                  tform.vector <- form.vector
                  tform.vector[i] <- scope[[i]][trial[i]]
                  form <- paste(form.y, paste(tform.vector, collapse = " + "))
                  if (trace) 
                    cat("Trial: ", form)
                  tfit <- update(object, eval(parse(text = form)), trace = FALSE, ...)
                  TEST <- anova(fit, tfit, test = OPTIONS$TEST)
                  if(OPTIONS$TEST == "Chisq")
                       tFRED <- TEST[2, 5]
                  else tFRED <- TEST[2, 6]
                  if(!is.null(keep)) {
                        keep.list[[nv]] <- keep(tfit,tFRED)
                        nv <- nv + 1
                  }
                  if(tFRED > bFRED) {
                        bFRED <- tFRED
                        kFRED <- tFRED
                        bitems <- trial
                        bfit <- tfit
                        bform.vector <- tform.vector
                        bfrom <- form.vector[i]
                        bto <- tform.vector[i]
                        Nstep <- i
                  }
                  if(trace)
                        cat("; p.value(-) =", format(round(tFRED, 6)),"\n")
            }
   } # backward
            if (forward) {
                trial <- items
                trial[i] <- trial[i] + 1
                if (trial[i] <= term.lengths[i] && !visited[array(trial, c(1, n.items))]) {
                  visited[array(trial, c(1, n.items))] <- TRUE
                  tform.vector <- form.vector
                  tform.vector[i] <- scope[[i]][trial[i]]
                  form <- paste(form.y, paste(tform.vector, collapse = " + "))
                  if (trace) 
                    cat("Trial: ", form)
                  tfit <- update(object, eval(parse(text = form)), trace = FALSE, ...)
                  TEST <- anova(fit, tfit, test = OPTIONS$TEST)
                  if(OPTIONS$TEST == "Chisq")
                        tFRED <- TEST[2, 5]
                  else tFRED <- TEST[2, 6]
                  if(!is.null(keep)) {
                        keep.list[[nv]] <- keep(tfit,tFRED)
                        nv <- nv + 1
                  }
                  if(tFRED < kFRED & tFRED < LIMIT) {
                        bFRED <- LIMIT + 0.001
                        kFRED <- tFRED
                        bitems <- trial
                        bfit <- tfit
                        bform.vector <- tform.vector
                        bfrom <- form.vector[i]
                        bto <- tform.vector[i]
                        Nstep <- i
                  }
                  if(trace)
                        cat("; p.value(+) =", format(round(tFRED, 6)),"\n")
                  }

            } # forward
        }# while
if(STOP <= LIMIT | steps == 0 | Nstep == 0) {
      if (!is.null(keep)) 
            fit$keep <- re.arrange(keep.list[seq(nv - 1)])
            return(make.step(models[seq(nm - 1)], fit, scale, object))
        }
        else {
            if (trace) 
                  cat("Step : ", Nstep, "/ ", deparse(bfit$formula), "; p.value =", kFRED, "\n\n")
                  items <- bitems
                  models[[nm]] <- list(deviance = deviance(bfit), df.resid = bfit$df.resid, pvalue = kFRED,from = bfrom, to =bto)
                  nm <- nm + 1
                 fit <- bfit
                  form.vector <- bform.vector
              }
         }
}

