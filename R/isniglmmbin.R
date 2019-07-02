#' A randomzied trial data set for Smoking cessation 
#'
#' The variables are as follows:

#' \itemize{
#'   \item id. subjects id
#'  \item  time. Time in weeks after baseline
#'  \item quit. 0: smoking; 1: quit smoking
#'  \item sub. patients id
#'  \item helmert1. Herlmert contrast 1 among 4 treatment groups.
#'  \item helmert2. Herlmert contrast 2 among 4 treatment groups.
#'  \item helmert3. Herlmert contrast 3 among 4 treatment groups.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name skquit
#' @usage data(skquit)
#' @format A data frame with 1861 rows and 6 variables
NULL


#' Function for ISNI computation when the longitudinal/clustered binary outcome follows a GLMM.
#'
#' Calculate the ISNI when the regression outcome is subject to missingness and follows generalized linear mixed-effects models (GLMMs) for binary outcomes. 
#' @param formula an object of class "Formula": a symbolic description of the models to be fitted for the outcome and missingness status variable.  
#'                The details of model specification are given under "Details".  
#' @param data  the name of data frame containing all the variables in the model and all the observations including those intended to be collected 
#'              but became missing.
#' @param random an object of class "formula": an one-sided linear formula description of the random-effects part of the model to be fitted 
#'               for the outcome
#' @param id the name of the level-2 clustering variable. 
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process for the outcome model and the missingness mechanism model.
#' @param weights frequency weights to be assigned to each \code{id}. when supplied, indicates differential weights are used; otherwise each \code{id} is weighted equally.  
#' @param predprobobs Null if using buil-in multinomial transitional logistic model to obtain predicted probabilities of being observed;
#'                    otherwise user supply the name of the variable in \code{data} that gives these probabilities for all the observations in the data.
#' @param misni  FALSE if using the default approach to computing ISNI with a scalar nonignorability parameter; 
#'               TRUE when computing ISNI with multiple nonignorability parameters.
#' @param method Indicate the method to obtain the MAR estimates:
#'               1: OPTIM(); 2: MIXOR(); 3: GLMER()               
#' @details The ISNI analysis is based on a joint selection model and requires specifying two model equations: the outcome model and the missingness mechanism model.
#'        At a minimum, the user should supply a single-equation in the typical form:  \code{response} ~ \code{Xterms} where \code{response} is the (numeric or factor) vector for the binnary outcome of interest and \code{Xterms} 
#'        is a series of terms, separated by + operators, which specify a linear predictor for response. With the signle-equation specification, the \code{isniglmm} function 
#'        will by default use the utility function \code{definemissingstatus} provided in the package to generate the 
#'        missingness status variables at the current and prior visits and then use \code{Xterms} as the predictors for fitting a first-order transitional missing data model. 
#'        It is important to sort within-\code{id} observations  by time so that the missingness status variables can be defined correctly in this default setting.  The \code{isniglmmbin()} then computes the MAR estimates and conducts ISNI computation
#'        to evaluate the rate of change of model estimates in the neighborhood of the MAR model where the missingness probability is allowed to depend on the unobserved value 
#'        of \code{response},   even after  conditioning on the other  missingness predictors. 
#'
#'        The above single-equation formula specification uses the same set of predictors for the outcome model and the missingness mechanism model for computing ISNI.  
# '       To permit using different sets of predictors,  we make use of the  \code{R} package "Formula" designed for for handling model equations with multiple responses  and multiple sets of predictors. 
#'        To use different sets of predictors, one can explicitly specifiy a two-equation formula as: \code{response} | \code{miss + missprior} ~ \code{Xterms} |  \code{Sterms},
#'        which specifies the formula  for the outcome model as \code{response} ~ \code{Xterms} and that for the missingness mechanism model as \code{miss | missprior} ~ \code{Sterms}, 
#'        where \code{Xterms} and \code{Sterms} can be different, \code{miss} and \code{missprior} are the variable names in \code{data} denoting the missingness status at the 
#'        current and prior visits, respectively.  
#'
#'        For \code{isniglmm}, \code{response} ~ \code{Xterms} specfied the fixed-effect part of the linear mixed-effects model for the outcome. The random-effect part of the model is
#'        specified as a one-sided formula via the argument \code{random}. 
#' @name isniglmmbin
#' @import  matrixcalc mvtnorm nnet  stats Formula mixor
#' @importFrom lme4 glmer
#' @aliases isniglmmbin
#' @export
#' @examples
#'
#' data(skquit)
#'
#' formula=quit~time 
#' ## formula=quit~time + helmert1:as.factor(time) +helmert2:as.factor(time)+ helmert3:as.factor(time)
#' random=~1
#'
#' result=isniglmmbin(formula, skquit, random, id,misni=FALSE,method=2)
#' summary(result)
#' 
isniglmmbin = function(formula, data, random, id, weights, subset, predprobobs, misni=FALSE, method=1) {

 ## (1) process the call and set up model frame.
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  if (missing(id)) 
    stop("Users need to supply the name of the level-2 clusting variable in the data via the argument id.")
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "id", 
               "predprobobs"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  f <- Formula(formula)
  if (any(length(f) > 2)) 
    stop("Cannot have more than two models")
  mf$formula <- as.Formula(formula, random)
  options(na.action = na.pass)
  mf[[1L]] <- as.name("get_all_vars")
  mfvar <- eval(mf, parent.frame())
  if (!missing(subset)) {
    SubSet <- mfvar$subset
    mfvar <- mfvar[SubSet, ]
  }
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ID = model.extract(mf, "id")
  if (missing(weights)) 
    WTs <- NULL
  else WTs <- model.extract(mf, "weights")
  if (!is.null(WTs) && !is.numeric(WTs)) 
    stop("'weights' must be a numeric vector")
  if (!is.null(WTs) && any(WTs < 0)) 
    stop("negative weights not allowed")
  if (is.null(WTs)) 
    WTs <- rep(1, nrow(mf))
  isni_WTs <- isni_id <- isni_outcome_ <- NULL
  mf = cbind(mf, mfvar[!(names(mfvar) %in% names(mf))], isni_id = ID, 
             isni_WTs = WTs)
  if (!missing(predprobobs)) 
    mf$fitprobs_ = model.extract(mf, "predprobobs")
  ymodel = formula(f, lhs = 1, rhs = 1)
  x <- as.matrix(model.matrix(f, data = mf, rhs = 1))
  y <- model.part(f, data = mf, lhs = 1)[, 1]
  if (nrow(x) < 1) 
    print("No predictor variables specified for the outcome")
  z <- model.matrix(random, data = mf)
  options(na.action = na.omit)
  if (length(f)[1] == 1) {
    cat("\n NOTE: The model formula specification does not include  the missingness status variables at the current and the prior visits. \n       The program will assume that data has already been sorted by the time variable within each level-2 ID variable and proceed to create the missingness status variables and then fit a transitional missingness mechanism model. \n")
    f <- update(f, . | g_ + gp_ ~ .)
    mf <- cbind(mf, isni_outcome_ = y)
    mf <- definemissingstatus(data = mf, id = isni_id, y = isni_outcome_)
    if (ncol(model.part(f, data = mf, lhs = 2)) != 2) 
      stop("The missing data model should be in the form of g+gp ~ Sterm")
  }
  gmodel <- formula(f, lhs = 2, rhs = length(f)[2])
  missx <- apply(x, 2, FUN = function(u) any(is.na(u)))
  if (any(missx)) 
    cat(paste("\n Note: There are missing values in fixed-effect outcome predictors-- ", 
              paste(dimnames(x)[[2]][missx], collapse = " "), ". Observations with missing values in these predictors will be dropped out from computing ISNI, \n       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", 
              sep = ""))
  missz <- apply(z, 1, FUN = function(u) any(is.na(u)))
  if (any(missz)) 
    cat(paste("\n Note: There are missing values in random-effect outcome predictors-- ", 
              paste(names(x)[missx], collapse = " "), ". Such observations will be dropped out from computing ISNI, \n       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", 
              sep = ""))
  missX <- apply(x, 1, FUN = function(u) any(is.na(u)))
  missZ <- apply(z, 1, FUN = function(u) any(is.na(u)))
  WTs = WTs[(!missX) & (!missZ)]
  mf = mf[(!missX) & (!missZ), , drop = F]
  options(na.action = na.pass)
  x <- as.matrix(model.matrix(f, data = mf, rhs = 1))
  y <- model.part(f, data = mf, lhs = 1)[, 1]
  z <- model.matrix(random, data = mf)
  xomit <- as.matrix(model.matrix(f, data = mf[!is.na(y), ], 
                                  rhs = 1))
  zomit <- model.matrix(random, data = mf[!is.na(y), ])
  if (ncol(x) != ncol(xomit)) {
    cat(paste("\n All variable names in fixed-effects design matrix of ISNI analysis including observations with missing outcomes: \n", 
              paste(dimnames(x)[[2]], collapse = " ")))
    cat(paste("\n All variable names in fixed-effects design matrix of the MAR model excluding observations with missing outcomes:  \n", 
              paste(dimnames(xomit)[[2]], collapse = " ")))
    stop("\n The fixed-effects design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function\n               in model formula.  ")
  }
  if (ncol(z) != ncol(zomit)) {
    cat(paste("\n All variable names in random-effects design matrix of ISNI analysis including observations with missing outcomes: \n", 
              paste(dimnames(z)[[2]], collapse = " ")))
    cat(paste("\n All variable names in random-effects design matrix of the MAR model excluding observations with missing outcomes:  \n", 
              paste(dimnames(zomit)[[2]], collapse = " ")))
    stop("\n The random-effects design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function\n               in model formula.  ")
  }
  options(na.action = na.omit)
  maxT = max(table(mf$isni_id))
  
   ## Fit the MAR model using different methods
     
    ## use optim() function. 
    if (method==1) {
       if (ncol(z)==1) mixor.rand=NA
       else  mixor.rand=which(colnames(x)%in%colnames(z)==T)[-1]-1
       options(na.action=na.omit)    
       start=mixor(formula=ymodel,data=mf,id =isni_id,which.random.slope = mixor.rand,vcov=F,link = "logit",indep.re =T)$Model[,1]

       lb=c(rep(-Inf, ncol(x)), rep(0,length(start)-ncol(x)) )
       print("Fitting mixed logit models assuming MAR using optim() function...") 
       optimres= optim(start,fun.mnll,gr=fun.grad,data=mf,fix.form=ymodel,z=z,hessian=TRUE,method="L-BFGS-B", lower=lb)
        print("MAR estimates obtained, computing observed Information Matrix now....")
       igglmm=optimres$par
       nabla11=solve(optimres$hessian)
    
    }
  
     else if (method==2){
      #if only one random effect is included in model, MIXOR will accept the random intercept only, the fixed part must include the intercept
      if (ncol(z)==1) mixor.rand=NA
      else   mixor.rand=which(colnames(x)%in%colnames(z)==T)[-1]-1
       options(na.action=na.omit)
       print("Note: This method uses the function mixor() to obtain GLMM estimates under MAR and uses the Expected Information Matrix computed by mixor() to compute ISNI. Generally this method  is fast and the results approximate ISNI values using the observed Information matrix")           
       print("Fitting mixed logit models assuming MAR using mixor() function...") 
       mixorres=mixor(formula=ymodel,data=mf,id =isni_id,which.random.slope = mixor.rand,vcov=F,link = "logit",indep.re =T)
       print("MAR estimates obtained, computing Expected Information Matrix now which is fast to obtain but only approximates observed Information Matrix....")
       igglmm=mixorres$Model[,1]
       nabla11=mixorres$varcov
     } 
     
     else if (method==3){
       if (length(random)==3) {
      fglmer=as.formula(paste(paste(ymodel[2], ymodel[1], ymodel[3], sep=""),paste("+(", paste(paste(random[3], " | isni_id", sep=""),")"))))
       }
       if (length(random)==2) {
      fglmer=as.formula(paste(paste(ymodel[2], ymodel[1], ymodel[3], sep=""),paste("+(", paste(paste(random[2], " | isni_id", sep=""),")"))))
       }
       #only allow one random effect, #print error when number of random effect>1
       if (ncol(z)>1){
         stop("The  adaptive Gauss-Hermite quadrature (nAGQ>1) in glmer() 
              at present implemented only for models with a single scalar random effect. Use method=1 or 2 for vector random effects.")
       }
       
        print("Fitting mixed logit models assuming MAR using glmer() function...")    
       glmerres=glmer(formula=fglmer, data =mf, family=binomial,na.action=na.omit,nAGQ=10)
       print("MAR estimates obtained, computing observed Information Matrix now....")
       glmer.ran=attr(unclass(VarCorr(glmerres))$isni_id,"stddev")
       names(glmer.ran)=paste("Random", names(attr(unclass(VarCorr(glmerres))$isni_id,"stddev")), sep=".")
       igglmm=c(fixef(glmerres),glmer.ran)
       nabla11=solve(optim(igglmm,fun.mnll,gr=fun.grad,data=mf,fix.form=ymodel,z=z,hessian=TRUE,method="L-BFGS-B")$hessian)
     }
   print("Computing  ISNI now...")
   ##  fit a missing data model to obtain fitted probabilities for being observed.
  if (missing(predprobobs)) {
    ##if (!("g_" %in% names(mf)))  stop("A variable called 'g_' for missing data status should be supplied in the data")
    ##if (!("gp_" %in% names(mf)))  stop("A variable called 'gp_' for missing data status for the prior visit should be supplied in the data")
    mf.mdm= tmdm(gmodel, data=mf, weights=isni_WTs)
    mf$fitprobs_=mf.mdm$obsprob_ 
    mf$A10_= mf.mdm$A10_;  mf$A20_= mf.mdm$A20_;  mf$A11_= mf.mdm$A11_
  } else 
   {
    if (misni==T) stop("predprobobs needs to to a matrix to obtain ISNI vector for multiple nonignorability parameter")
  }

  nabla12=fun.margall(igglmm,data=mf,fix.form=ymodel,z=z,gpred=mf$fitprobs_,calc=4)
  se<-sqrt(diag(nabla11))
  isni=nabla11%*%nabla12
  sdy <- sd(y,na.rm=T)
  if (misni==FALSE) {
       isni=apply(isni,1, sum); senstran<-abs((se)/isni)
   } else
   {
       isni=apply(abs(isni),1, sum); senstran<-abs((se)/isni)
   }
  res=list(coef=igglmm,se=se,isni=c(isni),c=c(senstran), call=cl)
  class(res) = c(res$class, "isniglmm")
  res
}



#' Function to print out a summary of isniglmm  object in a matrix form.
#'
#' @param object the isniglmm object obtained from the isniglmm function
#' @param digits the number of significant digits to use when printing
#' @param ... additional arguments
#' @export summary.isniglmm
#' @export
#' @name summary.isniglmm
#' @aliases summary.isniglmm
summary.isniglmm<-function(object, digits = max(3, getOption("digits") - 2),...) {
  
  if (class(object) != "isniglmm")  stop('Invalid object class')
  
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

   #' Function to print  the isniglmm object.
    #'
    #' \code{print} method for class isniglmm
    #' @param x the isniglmm object obtained from the isniglmm function
    #' @param digits the number of significant digits to use when printing
    #' @param ... further arguments passed to or from other methods.
    #'
    #' @return The function print.isnilmm prints the model call,  isni and c statistics from the isniglmm object.
    #' @name print.isniglmm
    #' @aliases print.isniglmm
    #' @export print.isniglmm
    #' @export
    print.isniglmm<-function(x, digits = max(3, getOption("digits") - 2), ...) {
	
     if (class(x) != "isniglmm")  stop('Invalid object class')
     cat("\nCall:\n", paste(deparse(x$call), sep = "\n", 
        collapse = "\n"), "\n\n", sep = "")

    
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

## Internal Function to calculate the various items (listed in calc) for subject i at quadrature point qpt.
##
## Calculate the compound symmetry matrix and its first and second derivatives wrt the vari-cov matrix
## @param qpt   design matrix for random effects
## @param datai list of data for subject i, must have below compnent: x: data frame representing covariates for beta.
##	y: vector representing the panel outcome.
## @param par   vector for the parameter beta and Sigma to be maximized
## @param calc   1: Calculate the conditional likelihood for subject i: f(Y_i|b_i);   
##               2: Calculate the first derivative of item 1 above w.r.t beta and sigma;
##               4: return the three parts  of conditional mean's derivative w.r.t beta and sigma, 
## @aliases  fun.subiq

    
 fun.subiq<- function (qpt,datai,par,calc=1) {
 
      obs<-!is.na(datai$y)
      
      ## when at least one y is observed
      if (any(obs)) {
      xiobs<-as.matrix(datai$x[obs,,drop=F])
      ziobs<-as.matrix(datai$z[obs,,drop=F])  
      yiobs<-as.vector(datai$y[obs])
      niobs<-length(yiobs)
      etaiobs<-as.matrix(cbind(xiobs,t(t(ziobs)*qpt)))%*%as.matrix(par)
      ##print(t(t(ziobs)*qpt))
      muiobs<-as.vector(exp(etaiobs)/(1+exp(etaiobs)))
      } else {
      niobs=0
      }

      cond.lik<-numeric(niobs)                 ## conditional likelihood for each observation
      der.cond.lik<-numeric(length(par))    ## derivative of conditional likelihood. 
      
      if (niobs>0) {
      for (j in 1:niobs)                   ## loop over observations within each subject;
      {
        cond.lik[j] <- dbinom(yiobs[j],1,muiobs[j])     
      }

      pbound<-10^(-10)
      ifelse(cond.lik<pbound,pbound,cond.lik)
      cond.lik.obs<-prod(cond.lik)
      }

      if (calc ==2 | calc==4)
      {
        if (niobs >0) {
        for (j in 1:niobs)        ## loop over observations within each subject;
        { 
          der.cond.lik<- 
            der.cond.lik+ as.vector((yiobs[j]-muiobs[j])*c(xiobs[j,],ziobs[j,]*qpt))*cond.lik.obs       
        }
                    }
      }
      if (calc==4) {
        nimis<-length(datai$y)-sum(obs)
        ximis<-as.matrix(datai$x[!obs,,drop=F])
        zimis<-as.matrix(datai$z[!obs,,drop=F])  
        yimis<-as.vector(datai$y[!obs])
        xzimis = as.matrix(cbind(ximis,t(t(zimis)*qpt)))
        etaimis<-as.matrix(cbind(ximis,zimis*qpt))%*%as.matrix(par)
        muimis<-as.vector(exp(etaimis)/(1+exp(etaimis)))
        if (niobs==0) {
          der.cond.mean1<-t(xzimis * (muimis*(1-muimis)))
          der.cond.mean2<-NULL
          der.cond.mean3<-NULL 
        } else {
          der.cond.mean1<-cond.lik.obs* t(xzimis * (muimis*(1-muimis)))
          der.cond.mean2<- as.matrix(der.cond.lik)%*% t(as.matrix(muimis))
          der.cond.mean3<-muimis*cond.lik.obs  
           }
      }
      
      if (calc ==1)  res<-cond.lik.obs ## conditional likelihood for subject i
      ## vector derivative of conditional likelhiood for subject i
      else if (calc ==2) res <- as.vector(der.cond.lik) 
      else if (calc ==4) res <- list(part1=der.cond.mean1,part2=der.cond.mean2,part3=der.cond.mean3)
      res
    }

## Internal Function to integrate the function fun w.r.t. qpt using Gaussian-Hermite Quadrature
##
## Calculate subject-level quantities when the regression outcome is subject to missingness and 
## follows linear mixed-effects  Models. 
## @param func function that contains the integration formula, the first argument must be qpt
## @param datai list of data for subject i, must have below compnent: x: data frame representing covariates for beta.
## @param ... additional arguments to the function "func"
## @aliases fun.ghqd
## @export

fun.ghqd <- function (func,datai,...) {
  ##Gaussian-Hermite Quadrature to integrate the function fun w.r.t. qpt, 
  ##   .... is the additional argument to the function "fun"
  
  qpt<-c(-4.85946283, -3.58182348, -2.48432584, -1.46598909, -0.48493571,
         0.48493571,  1.46598909,  2.48432584,  3.58182348,  4.85946283)
  qwt<-c(.00000432, .00075807, .01911158, .13548371, .34464235,
         .34464235, .13548371, .01911158, .00075807, .00000431)
  
  res<-func(qpt=qpt[1],datai,...)*qwt[1]
  
  if (dim(datai$z)[2]==1) {
    for (i in 2:length(qpt)) res<- res+ func(qpt=qpt[i],datai,...)*qwt[i]
  } 
  else {
    repqpt=rep(list(qpt),dim(datai$z)[2])
    repqwt=rep(list(qwt),dim(datai$z)[2])
    qptM=as.matrix(do.call(expand.grid, repqpt))
    qwtM=as.matrix(do.call(expand.grid, repqwt))
    colnames(qptM)=NULL
    colnames(qwtM)=NULL
    for (k in 1:nrow(qptM)){
      res<-res + func(qpt=qptM[k,],datai,...)*prod(qwtM[k,])
    }
  }
  res
}


## Internal Function to integrate the function fun w.r.t. qpt using Gaussian-Hermite Quadrature
##
## Calculate subject-level quantities when the regression outcome is subject to missingness and 
## follows generalized linear mixed-effects  Models. 
## @param  par  : vector for the parameter $\beta$ and $Sigma$ to be maximized
## @param  data : data.frame, including observed data and observations at dropout time.
##                including columns for: y--outcome, g--missing indicator
##                 columns for fixed effect covariates in y, randome effects for y
##                 predictors for g. 
## @param  x: design matrix for fixed parameters
## @param  y: vector of the outcome
## @param  z: design matrix for random parameters
## @param  gpred:predicted probabilities of being observed
## @param  calc : list of items to be calcuated
##                 1: A scalar negative marginal loglikelihood is to be calculated  
##                 2: A vector of the first derivative of negative marginal loglikelihood w.r.t 
##                    beta and sigma$.  
##                 3: A matrix for (information matrix)^(-1) of the negative marginal loglikelihood w.r.t. 
##                    $\beta$ and $\sigma$.
##                 4: A vector of the first derivative of the conditional mean of missing data 
##                    given the observed data w.r.t beta and sigma. 
## @aliases fun.margall
## @export
fun.margall <- function (par, data,fix.form,z,gpred, r1grp=NULL,calc=1) {
  
  fix.temp<-model.frame(fix.form,data=data,na.action=NULL)
  x<- as.matrix(model.matrix(fix.form, data=fix.temp))
  y<- as.vector(model.extract(fix.temp, "response"))
  
  

  if (length(par) != (dim(x)[2]+dim(z)[2]) ) stop("Length of Parameter is incorrect")
  id<- data$id
  if (length(id) != length(y) | length(id) != dim(x)[1] | length(id) != dim(z)[1])
    stop("Length of id variable is incorrect, possiblely due to missingness") 
  
  uid<-unique(data$id)
  n<-length(uid)
  
  mar.lik.sub <-numeric(n)           ## vector to hold subject-specific marginal likelihood. 
  der.mar.lgl <-numeric(length(par)) ## vector to hold derivative of marginal loglokilihood
  invinfo     <-matrix(0,length(par),length(par)) ## vector to hold inverse information matrix of 
  ## marginal logliklihood.
  if (is.null(r1grp)) der.meanym <-numeric(length(par)) ## vector to hold derivative of conditional mean of missing y.
  else {
    ur1<-unique(r1grp)
    der.meanym<-matrix(0,nrow=length(par),ncol=length(ur1))
  }     


  ## dermat<-matrix(0, nrow=length(par),ncol=sum(data$g==0))
  dermat1<-NULL
  dermat2<-NULL
  k=1
  
  fun.dmean1<-function(qpt,datai,par) fun.subiq(qpt,datai,par,calc=4)$part1
  fun.dmean2<-function(qpt,datai,par) fun.subiq(qpt,datai,par,calc=4)$part2    
  fun.dmean3<-function(qpt,datai,par) fun.subiq(qpt,datai,par,calc=4)$part3     
  for (i in 1:n)                     ## loop over subjects 
  {
    datai<-list(x=as.matrix(x[id==uid[i],,drop=F]),z=z[id==uid[i],,drop=F],
                y=y[id==uid[i]])  ## get data for subject i
    ## obtain marginal likelihood for subject i. 
    
    ## if all yi are missing. 
    if (calc ==1 & all(is.na(datai$y)) )  mar.lik.sub[i] <- 1
    if (calc ==1 & ! all(is.na(datai$y)) ) mar.lik.sub[i]<-fun.ghqd(fun.subiq,datai=datai,par=par,calc=1)  
    
    ## increase values only if not all yi are missing
    if ( ((calc ==2) | (calc==3)) & !all(is.na(datai$y)) ) {
      
      ## obtain derivative of marginal loglikelihood for subject i
 
         mar.lik.sub[i]<-fun.ghqd(fun.subiq,datai=datai,par=par,calc=1)
         der.mlogl.sub <-fun.ghqd(fun.subiq,datai=datai,par=par,calc=2)/mar.lik.sub[i]
         der.mar.lgl <- der.mar.lgl + der.mlogl.sub
      
         ## calculate inverse information matrix of the marginal loglikelihood. 
         if (calc==3) {invinfo <- invinfo + 
          as.matrix(der.mlogl.sub)%*%t(as.matrix(der.mlogl.sub))
                   }
    }
    
    if ( (calc==4) & is.na(sum(datai$y)) ){    
      gpredi<-gpred[id==uid[i]]
      hpredi <- gpredi[is.na(datai$y)]

      if (all(is.na(datai$y))) {
                   mar.lik.sub[i]=1
                  der.meanym <-der.meanym + (fun.ghqd(fun.dmean1,datai=datai,par=par)/mar.lik.sub[i]) %*% as.matrix(hpredi) 
                               } else {
           mar.lik.sub[i]<-fun.ghqd(fun.subiq,datai=datai,par=par,calc=1)  
          der.mlik.sub<-fun.ghqd(fun.subiq,datai=datai,par=par,calc=2)
        der.meanym <-der.meanym + (fun.ghqd(fun.dmean1,datai=datai,par=par)/mar.lik.sub[i]
                       +fun.ghqd(fun.dmean2,datai=datai,par=par)/mar.lik.sub[i]
                        -as.matrix(der.mlik.sub)%*%fun.ghqd(fun.dmean3,datai=datai,par=par)/((mar.lik.sub[i])^2)) %*% as.matrix(hpredi) 

      } ## end of if statement
    } ##end of calc if statement         
    k=k+ length(datai$y) 
    
  } ## end of for loop
  
 
  if (calc==1)  res<- -sum(log(mar.lik.sub))  ## A scalar negative marginal loglikelihood
  if (calc==2) {res<- -der.mar.lgl}   ## derivative of the negative marginal logliklihood
  if (calc==3) {res <- invinfo}     
  if (calc==4) res<- der.meanym 
  res
  
  
}

fun.mnll<-function(par,data,fix.form,z) fun.margall(par,data=data,fix.form=fix.form,z=z,calc=1)
fun.grad<-function(par, data,fix.form,z) fun.margall(par,data=data,fix.form=fix.form,z=z,calc=2)
fun.hess<-function(par, data,fix.form,z) fun.margall(par,data=data,fix.form=fix.form,z=z,calc=3)

 