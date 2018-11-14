"latex.summary.lme" <-
  function(object, title="",parameter=NULL, file="",
           shadep=0.05,caption=NULL,label=NULL,ctable=FALSE,form=NULL,
           interceptp = FALSE, moredec=0, where="!htbp", ...) {
    # This function can be mis-used for gls models when an explicit
    # form is given
    options(Hverbose=FALSE)
    require('Hmisc')
    require('nlme')
    dd <- object$dims
    method <- object$method
    fixF <- object$call$fixed
    xtTab <- as.data.frame(object$tTable)
    sigp <- xtTab[,"p-value"]< shadep # cells that will be shaded
    if (!interceptp){
      sigp[1] <- FALSE # intercept will never be shaded
      # Replace small significances, discarding p-value for (Intercept)
      xtTab[1,"p-value"] = 1 # we do not show it anyway, easier formatting
    }
    pval <- format(zapsmall(xtTab[, "p-value"],4))
    pval[as.double(pval) < 0.0001] <- "$< .0001$"
    xtTab[, "p-value"] <- pval
    xtTab[,"t-value"] <- round(xtTab[,"t-value"],1)
    if (ncol(xtTab) == 5) # not for gls
      xtTab[,"DF"] <- as.integer(xtTab[,"DF"])
    # extract formula
    if (is.null(form)) {
      if (!is.null(object$terms)) {
        form=object$terms
      } else {
        form = formula(object)
      }
    }
    if (is.null(parameter)) {
      parameter=as.character(form[[2]])
    }
    if (any(wchLv <- (as.double(levels(xtTab[, "p-value"])) == 0))) {
      levels(xtTab[, "p-value"])[wchLv] <- "<.0001"
    }
    if (is.null(label))
      label <- lmeLabel("contr",form)
    form <- deparse(removeFormFunc(as.formula(form)),width.cutoff=500)
    
    form <- paste(sub('~','$\\\\sim$ ',form),sep="")
    # All I( in factors are replaced with (This could be improved)
    row.names(xtTab) <- 
      gsub("I\\(","(",dimnames(object$tTable)[[1]])
    row.names(xtTab) <-  gsub("\\^2","\\texttwosuperior",row.names(xtTab))
    
    # Determine base level  
    levs <- lapply(object$contrasts,function(object) {dimnames(object)[[1]][1]})
    levnames <- paste(names(levs),levs,sep=" = ",collapse=", ")
    # Try to locate numeric covariables
    #  v1 <- all.vars(formula(object))[-1]
    ## Changed 8.10.2008, not regression-tested
    v1 <- all.vars(form)[-1]
    numnames <- v1[is.na(match(v1,names(levs)))]
    if (length(numnames > 0)) {
      numnames <- paste(numnames," = 0",collapse=", ")
      levnames <- paste(levnames,numnames,sep=", ")
    }
    if (is.null(caption)){ # TODO: Allow %s substitution
      if (inherits(object,"lme"))
        md = "Mixed model (lme)" else
          if (inherits(object,"gls"))
            md = "Extended linear model (gls)" else
              md = "Linear model"
            caption <- paste(md," contrast table for \\emph{",
                             parameter, "} (model ",form,
                             "). The value in row (Intercept) gives the reference value for ",
                             levnames,".",sep='')
    }
    caption.lot <- paste("Contrast table for ",parameter, " by ",
                         levnames)
    ndec <- pmax(round(1-log10(xtTab[,2]+0.000001)+moredec),0)
    xtTab[,1] <- formatC(round(xtTab[,1],ndec))
    xtTab[,2] <- formatC(round(xtTab[,2],ndec))
    if (ncol(xtTab) == 5) {
      names(xtTab) <- c("Value","StdErr","DF","t","p")
      pcol = 5
    } else {# gls misuse
      names(xtTab) <- c("Value","StdErr","t","p")
      pcol = 4
    }
    # Only show intercept p/t when explicitely required
    if (!interceptp){
      xtTab[1,pcol-1] <- NA
      xtTab[1,pcol] <- ''
    }
    cellTex <- matrix(rep("", NROW(xtTab) * NCOL(xtTab)), nrow=NROW(xtTab))
    cellTex[sigp,pcol] <- "cellcolor[gray]{0.9}"
    rowlabel <- ifelse(nchar(parameter) >9,"",parameter)
    latex(xtTab, title=title, file=file, caption=caption,caption.lot=caption.lot,
          caption.loc="bottom", label=label, cellTexCmds = cellTex,
          rowlabel=rowlabel, ctable=ctable, where=where,
          booktabs = !ctable, numeric.dollar=FALSE,col.just=rep("r",5),...)
  }

"latex.lme" <-
  function(object, title="",parameter=NULL,file="",shadep=0.05,
           caption=NULL,label=NULL,ctable=FALSE,form=NULL,
           interceptp=FALSE,  moredec= 0, where="!htbp",...) {
    options(Hverbose=FALSE)
    require('Hmisc')
    require('nlme')
    latex.summary.lme(summary(object),title=title,parameter=parameter, 
                      file=file, shadep=shadep, caption=caption,
                      label=label, ctable=ctable, form=form, moredec=moredec, where=where,...)
  }