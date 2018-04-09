## Add all the required packages in the @import line in the isniglm function. 
## To export a function, specify @export functionname rather than @export alone. 


## Dataset for a survey of sexual behavior
##    #'
##    #' A dataset from a survey of sexual behavior
##    #'
##    #'
##    #' This data frame contains the following variables:
##    #' \itemize{
##    #'  \item  y: Response to the survey (1=yes, 0=no)
##    #'  \item gender: 0=male; 1=female
##    #'  \item fac: the student's faculty (1=medical/dental/veterinary, 0=other)
##    #'  \item genderbyfac: the product of gender and fac
##    #'  \item  g: an indicator of missing status (0=observed, 1=missing) for y
##    #' }
##    #'
##    #' @docType data
##    #' @keywords datasets
##    #' @name sosgrp
##    #' @usage data(sosgrp)
##    #' @format A data frame with 6136 rows and 5 variables
##    NULL


#' Dataset for a survey of sexual behavior
    #'
    #' A dataset from a survey of sexual behavior
    #'
    #'
    #' This data frame contains the following three factor variables:
    #' \itemize{
    #'  \item  sexact: response to the survey question "Have you ever had sexual intercouse" with two levels of 
    #'                   \code{no} (ref level) and \code{yes}.
    #'  \item gender: two levels of \code{male} (ref level) and female.
    #'  \item faculty: the student's faculty with two levels of \code{mdv}=medical/dental/veterinary (ref level) and  \code{other}
    #'                =all the other faculty categories.
    ##  \item genderbyfac: the product of gender and fac
    ##  \item  g: an indicator of missing status (0=observed, 1=missing) for y
    #' }
    #'
    #' @docType data
    #' @keywords datasets
    #' @name sos
    #' @usage data(sos)
    #' @format A data frame with 6136 rows and 3 variables
    NULL


    #' Function for ISNI computation when the outcome follows GLMs.
    #'
    #' Calculate the ISNI when the regression outcome is subject to missingness and follows generalized linear models (GLMs)
    #' @param formula an object of class "Formula": a symbolic description of the models to be fitted for the outcome and missingness status variable.  
    #'                The details of model specification are given under "Details".  
    ## @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
    #' @param family  a description of the error distribution to be used in the GLM for the outcome.
    #'             
    #' @param data  the name of data frame containing the variables in the model and all the observations including those intended to be collected 
    #'              but became missing.
    #'             
    #' @param weights an optional vector of "prior weights" to be used in the fitting process for the outcome model and the missingness mechanism model.
    #'               Should be NULL or a numeric vector.
    #' @param subset an optional vector specifying a subset of observations to be used in the fitting process for the outcome model and the missingness mechanism model.
    #' @param start starting values for the parameters in the linear predictor of the outcome model. 
    #' @param offset an optional vector to specify an a priori known component to be included in the linear predictor during fitting the GLM for the outcome. 
    #'               This should be NULL or a numeric vector of length equal to the number of observations. 
    ## @param flag  an indicator of using log link (instead of inverse) for Gamma distribution
    #' @details The ISNI analysis is based on a joint selection model and requires specifying two model equations: the complete-data model and the missing data mechanism model.
    #'        To specify the variables in the models and required for computing the ISNI measures, we make use of the  \code{R} package "Formula"  designed for handling model
    #'        equations with multiple responses    and multiple sets of predictors. At a minimum, the user should supply a single-equation 
    #'         in the typical form:  \code{response} ~ \code{Xterms} where \code{response} is the (numeric or factor) vector for the outcome of interest and \code{Xterms} 
    #'        is a series of terms, separated by + operators, which specify a linear predictor for response. With the signle-equation specification, the \code{isnimgm} function 
    #'         will by default use (\code{is.na(response)}) as the 
    #'          missingness status variable and \code{Xterms} as the observed predictors for missingness. The \code{isniglm} then computes the MAR estimates and conducts ISNI computation
    #'         to evaluate the rate of change of model estimates in the neighborhood of the MAR model where the missingness probability is allowed to depend on the unobserved value 
    #'         of \code{response},   even after  conditioning on the other  missingness predictors. 
    #'
    #'         The above single-equation formula specification uses the same set of predictors for the outcome model and the missingness mechanism model for computing ISNI. 
    #'         To use different sets of predictors, one can explicitly specifiy a two-equation formula as: \code{response} | \code{is.na(response)} ~ \code{Xterms} |  \code{Sterms},
    #'        which specifies the formula  for the complete-data model as \code{response} ~ \code{Xterms} and that for the missing data mechanism model as \code{is.na(response)} ~ \code{Sterms}, and 
    #'        \code{Xterms} and the observed predictors for missingness \code{Sterms} can be different. 
    #' @name isniglm
    #' @aliases isniglm
    #' @export isniglm
    #' @import  matrixcalc mvtnorm nlme nnet  stats Formula
    #' @examples
    #' ## load data set
    #' data(sos)
    #'
    #' ## Perform the MAR analysis
    #' ymodel= sexact  ~ gender*faculty
    #' summary(glm(ymodel,family=binomial, data=sos))
    #'
    #' ## Perform ISNI analysis
    #' sos.isni<-isniglm(ymodel, family=binomial, data=sos)
    #' sos.isni
    #' summary(sos.isni)
    #' 
    #' ## specifying the missing data model explicitly
    #' ygmodel= sexact | is.na(sexact)  ~ gender*faculty | gender *faculty
    #' summary(isniglm(ygmodel, family=binomial, data=sos))
    #'  
    #' 
    ##  ##using the factor variables in R. 
    ##  RD99 <- data.frame(   intercourse = factor(r99$y, levels = c(0, 1), labels =
    ##  c("no", "yes")),   gender = factor(r99$gender, levels = c(0, 1), labels =
    ##  c("male", "female")),   faculty = factor(r99$fac, levels = c(0, 1), labels =
    ##  c("other", "mdv")))
    ##
    #'  ## ISNI for grouped binomial regression. 
    ##  sosgrp <- sos
    ##   levels(sosgrp$sexact) = c(levels(sosgrp$sexact), "Unknown")
    ##  sosgrp$sexact[which(is.na(sosgrp$sexact))] = "Unknown"
    ##  sosgrp$count <- rep(1, length(nrow(sosgrp)))
    ##  sosgrp <- aggregate(count ~ gender+faculty+ sexact, data=sosgrp, FUN=sum)
    ##  sosgrp$sexact <- factor(ifelse(sosgrp$sexact=="Unknown", NA,
    ##            sosgrp$sexact), levels=c(1,2), labels=c("no","yes"))
    ##  ymodel= sexact   ~ gender+faculty+gender*faculty 
    ## isniglm(ymodel, family=binomial, data=sosgrp, weight=count)
    #' gender <- c(0,0,1,1,0,0,1,1)
    #' faculty    <- c(0,0,0,0,1,1,1,1)
    #' gender = factor(gender, levels = c(0, 1), labels =c("male", "female"))
    #' faculty = factor(faculty, levels = c(0, 1), labels =c("other", "mdv"))
    #'  
    #' SAcount <- c(NA, 1277, NA, 1247, NA, 126, NA, 152)
    #' total       <- c(1189,1710,978,1657,68,215,73,246)
    #' sosgrp <- data.frame(gender=gender, faculty=faculty, SAcount=SAcount, total=total)
    #' ymodel <- SAcount/total ~gender*faculty
    #' sosgrp.isni<-isniglm(ymodel, family=binomial, data=sosgrp, weight=total)
    isniglm = function(formula,  family=gaussian, data, weights,  subset,  start=NULL,  offset) {
   
 
    ## (1) process the call and set up model frame.
    cl <- match.call()
    if (missing(data))  data <- environment(formula)
    if (is.character(family)) 
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }
    

    mf <- match.call(expand.dots = FALSE)    
    m <- match(c("formula", "data", "weights", "subset", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    f <- Formula(formula)
    if (any(length(f)>2)) stop("Cannot have more than two models") 
    mf$formula <- f
    options(na.action=na.pass)
    mf[[1L]] <- as.name("get_all_vars")  ## get original input variable
    mfvar<- eval(mf, parent.frame())
    if (!missing(subset)) {
      SubSet <-mfvar$subset
      mfvar <-mfvar[SubSet,]
                          }
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if (missing(weights)) WTs <- NULL else 
         WTs <- model.extract(mf, "weights")
    if (!is.null(WTs) && !is.numeric(WTs)) 
        stop("'weights' must be a numeric vector")
    if (!is.null(WTs) && any(WTs < 0)) 
        stop("negative weights not allowed")
    if (is.null(WTs)){
        IWT <- 0 
        WTs <- rep(1, nrow(mf))
    }
    isni_WTs <- NULL

    if (missing(offset)) OffSets <- rep(0, nrow(mf)) else
         OffSets <- model.extract(mf, "offset")
    if (!is.numeric(OffSets)) 
        stop("'offsets' must be a numeric vector")
    isni_offset <- NULL

    ##(2) Extract responses and predictors from the model frame
    ymodel=formula(f, lhs=1, rhs=1)
    x<- as.matrix(model.matrix(f, data=mf, rhs=1))
    y<- model.part(f,data=mf, lhs=1)[,1]  ## will also be a vector
    if (nrow(x)<1)  print("No predictor variables specified for the outcome")
    
    s<- as.matrix(model.matrix(f, data=mf, rhs=length(f)[2])) 
    mf= cbind(mf, mfvar[!(names(mfvar) %in% names(mf))],  isni_WTs=WTs, isni_offset=OffSets)  
    
    ## check if the missing status indicator g is specified, if not add g in the lhs of formula and to the model.frame
    if (length(f)[1]==1) {
       f <- update(f, .| g_ ~.)
       mf <- cbind(mf, g_=as.numeric(is.na(y)))
    }  
    gmodel=formula(f, lhs=2,rhs=length(f)[2])      
    g<- mf$g_ <-  model.part(f,data=mf, lhs=2)[,1]
    options(na.action=na.omit)
    
    ## Drop all observations with missing values in any outcome predictors  in X. 
    missX<- apply(x, 2, FUN=function (u) any(is.na(u)))
    if (any(missX)) cat(paste("\n Note: There are missing values in fixed-effect outcome predictors-- ", paste(dimnames(x)[[2]][missX], collapse=" "), ". Observations with missing values in these predictors will be dropped out from computing ISNI, 
       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", sep=""))
    missX  <- apply(x, 1, FUN=function (u) any(is.na(u)))
    WTs=WTs[!missX]
    OffSets = OffSets[!missX]
    s=s[!missX, ,drop=F]
    g=g[!missX]
    mf=mf[!missX,,drop=F]
    options(na.action=na.pass) 
    x<- as.matrix(model.matrix(f, data=mf, rhs=1))
    y<- model.part(f,data=mf, lhs=1)[,1]  ## will also be a vector 
    xomit <-  as.matrix(model.matrix(f, data=mf[!is.na(y), ], rhs=1))
    if (ncol(x) != ncol(xomit)) {
        cat(paste("\n All variable names in the design matrix of ISNI analysis including observations with missing outcomes: \n", paste (dimnames(x)[[2]], collapse=" ")))
        cat(paste("\n All variable names in the design matrix of the MAR model excluding observations with missing outcomes:  \n", paste (dimnames(xomit)[[2]], collapse=" ")))
        stop("\n The design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function
               in model formula.  ")
     }    
    options(na.action=na.omit)

    yo<-y[g==0]
    xo<-as.matrix(x[g==0,,drop=F])
    xm<-as.matrix(x[g==1,,drop=F]) 
    sdy<-sd(as.numeric(yo))

    ## fitting a glm outcome model on fully observed data 
    rego <- glm(ymodel, data=mf, family=family,  weights=isni_WTs, offset=isni_offset,start=start)     
    coef<-summary(rego)$coef[,1]
    yfit<-rego$fitted.values

    if(family$family=='gaussian') {
    xx<-t(sqrt(WTs[g==0])*xo)%*%(sqrt(WTs[g==0])*xo)
    tnorm<-sum(c(yo-yfit)^2)/length(yo)
    se<-tnorm^0.5*diag(solve(xx))^0.5
    } else se<-summary(rego)$coef[,2]

    if (! (family$family %in% c("gaussian","poisson","binomial","Gamma", "inverse.gaussian")))
      stop(paste(c("ISNI not implemented for the ", family$family, " distribution") ) )
    if ((family$family=='gaussian' & family$link != 'identity') | (family$family=='poisson' & family$link != 'log' ) |
       (family$family=='binomial' & family$link != 'logit') | (family$family=='Gamma' & family$link != 'inverse' ) |
          (family$family=='inverse.gaussian' & family$link != '1/mu^2')  ) 
        stop(paste(c("ISNI not implemented yet for the ", family$link, " link function ",  " under the ", family$family," distribution ")) )
    dist.int<-charmatch(family$family,c('gaussian','poisson','binomial','Gamma','inverse.gaussian'))
    cc<-switch(dist.int,
    rep(1,length(y)),
    exp(x%*%coef),
    exp(x%*%coef)/(1+exp(x%*%coef))^2,
    ##if(flag) exp(x%*%coef) else
    (x%*%coef)^(-2), 
    (x%*%coef)^(-3/2),
    stop('Invalid family type'))
    cc <- as.numeric(cc)    
 
    # calculating the first part of ISNI
    sscp1<-t(sqrt(WTs[g==0]*cc[g==0])*xo)%*%(sqrt(WTs[g==0]*cc[g==0])*xo)

    # calculating the second part of ISNI
    ## Fit missing data model, drop any predictors with missing values from the model. 
    missS  <- apply(s, 2, FUN=function (u) any(is.na(u)))
    s <- as.data.frame(cbind(s, isni_WTs=mf$isni_WTs, g_=mf$g_))
    regzg<-multinom(g_ ~.-1-isni_WTs, data=s[,!missS],   weights=isni_WTs)
    hm<-1-regzg$fitted.values[g==1]
    ccm<-WTs[g==1]*hm*cc[g==1]
    sscp2<-t(xm)%*%ccm
    
    # calculating sensitivity transformation
    isni<- solve(sscp1,sscp2)
    tau<-switch(dist.int,
    tnorm,1,1,
    rego$dev/rego$df.null*(6+rego$dev/rego$df.null)/(6+2*rego$dev/rego$df.null),
    sum((yo-yfit)^2/(yo*yfit^2))/length(yo),
    stop('Invalid distribution type'))


    isni<-switch(dist.int,tau*isni,tau*isni,
    tau*isni,tau*isni,-2*tau*isni,
    stop('Invalid distribution type'))

    if(family$family=='poisson'|family$family=='binomial') senstran<-abs(se/isni)
    else senstran<-abs((sdy*se)/isni)
    res=list(coefficients=rego$coef,se=se,tau=tau,isni=isni[,1],c=senstran[,1], call=cl, formula=formula)
    class(res) = c(res$class, "isniglm")
    res
    }

    ## Function to print a summary ISNIGLM, the index of sensitivity to
    ## nonignorability, in a generalized linear model for distributions
    ## of Gaussian, Poisson, Binomial, Gamma, and Inverse Gaussian.
    ## Weihua Gao 07/15/2015
    ##
    #' Function to print out a summary of isniglm  object in a matrix form.
    #'
    #' @param object the isniglm object obtained from the isniglm function
    #' @param digits the number of significant digits to use when printing
    ## @param dig.tst dig.tst
    #' @param ...  further arguments passed to or from other methods.
    #' @return The function summarizes the MAR coefficient estimates, standard errors, isni and c statistics from the isniglm object as a matrix form.
    #' @name summary.isniglm
    #' @aliases summary.isniglm
    #' @export summary.isniglm
    #' @export

    summary.isniglm<-function(object, digits = max(3, getOption("digits") - 2),
                  ...) {

    if (class(object) != "isniglm")  stop('Invalid object class')
    cat("\nCall:\n", paste(deparse(object$call), sep = "\n", 
        collapse = "\n"), "\n\n", sep = "")
    ## Name the columns
    isniname<-c('MAR Est.','Std. Err','ISNI','c')

    ## Set up matrix to hold result
    res<-matrix(0,length(object$coef),length(isniname))
    dimnames(res)<-list(names(object$coef),isniname)

    for (i in 1:length(object$coef))
       res[i,]<- c(object$coef[i],object$se[i],object$isni[i],object$c[i])
    printCoefmat(res, digits = digits,  cs.ind = 1:2)
    }



    #' Function to print  the isniglm object.
    #'
    #' \code{print} method for class isniglm
    #' @param x the isniglm object obtained from the isniglm function
    #' @param digits the number of significant digits to use when printing
    #' @param ... further arguments passed to or from other methods.
    #'
    #' @return The function prints the model call, isni and c statistics from the isniglm object.
    #' @name print.isniglm
    #' @aliases print.isniglm
    #' @export print.isniglm
    #' @export
    print.isniglm<-function(x, digits = max(3, getOption("digits") - 2), ...) {
	
     cat("\nCall:\n", paste(deparse(x$call), sep = "\n", 
        collapse = "\n"), "\n\n", sep = "")
     if (class(x) != "isniglm")  stop('Invalid object class')
    
     if (length(x$coef)>0) {
        cat("ISNIs:\n")
        print.default(format(x$isni, digits = digits), print.gap = 2L, 
            quote = FALSE)
        cat("\n")
        cat("c statistics:\n")
        print.default(format(x$c, digits = digits), print.gap = 2L, 
            quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
    
    }

  ## Extract the MAR coefficient estimates from the outcome GLM model. 
  ##
  ## \code{isni.isniglm} is a generic function which extracts the ISNIs for the model estimates from the \code{isniglm} object
  ## @param object the isniglm object obtained from the isniglm function
  ## @param ... further arguments passed to or from other methods.
  ##
  ## @return  The isni values extracted from the isniglm object.
  ## @name isni.isniglm
  ## @aliases isni.isniglm
  ## @export isni.isniglm