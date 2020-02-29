## Updates
## Nov 16, 2018
## Add codes to permit ISNI calculation when there are subjects with all outcomes are missing. 
##  The added codes are in isnimgm(), fun.mgmsubi(), 


#' A data set for Psychiatric Drug Treatment
#'
#' The variables are as follows:

#' \itemize{
#'   \item y. number of days
#'  \item time. in weeks
#'  \item sub. patients id
#'  \item group. treatment group with placebo (PBO) and desipramine (DMI)
#'  \item basey. baseline days
#'  \item g1. dummy variables for treatment group
#'  \item g2. dummy variables for treatment group
#'  \item time1. dummy variables for time
#'  \item time2. dummy variables for time
#'  \item grptime. group by time
#' }
#'
#' @docType data
#' @keywords datasets
#' @name coc
#' @usage data(coc)
#' @format A data frame with 869 rows and 10 variables
NULL

#' A data set for Quality of Life Emontional Functioning outcome.
#'
#' The variables are as follows:

#' \itemize{
#'  \item id: patients id
#'  \item y: EF score
#'  \item time: time in months since randomization. 
#'  \item group: placebo (0) or flutamide (1)
#'  \item perf: baseline performance score
#'  \item sever: baseline disease severity
##  \item t1: indicator variable for month 1
##  \item t3: indicator variable for month 3
##  \item t6: indicator variable for month 6
#  \item yp: most recently observed  prior outcome
#'  \item basey: EF at baseline
#'  \item g: missingness status ("O"=observed, "D"=dropout, "I"=intermittent missingness)
#'  \item gp: missingness status in the prior visit ("O"=observed, "D"=dropout, "I"=intermittent missingness, "U"=undefined)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name qolef
#' @usage data(qolef)
#' @format A data frame with 2860 rows and 10 variables
NULL

#' Function for ISNI computation when the outcome follows marginal multivariate Gaussian Models.
#'
#' Calculate the ISNI when the regression outcome is subject to missingness and follows marginal multivaraite Gaussian models.
#' @param formula an object of class "Formula": a symbolic description of the models to be fitted for the outcome and missingness status variable.  
#'                The details of model specification are given under "Details".  
#' @param data  the name of data frame containing all the variables in the model and all the observations including those intended to be collected 
#'              but became missing.
## @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
#' @param cortype  the type of within-subject correlation structure.
#' @param id the name of variable for the level-2 clustering variable. 
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process for the outcome model and the missingness mechanism model.
#' @param weights frequency weights to be assigned to each \code{id}. when supplied, indicates differential weights are used; otherwise each \code{id} is weighted equally.  
#' @param predprobobs Null if using buil-in multinomial transitional logistic model to obtain predicted probabilities of being observed;
#'                    otherwise user supply the name of the variable in \code{data} that gives these probabilities for all the observations in the data.
#' @param misni  FALSE if using the default approach to computing ISNI with a scalar nonignorability parameter; 
#'               TRUE when computing ISNI with multiple nonignorability parameters.
#' @details The ISNI analysis is based on a joint selection model and requires specifying two model equations: the complete-data model and the missing data mechanism model.
#'        To specify the variables in the models that are required for computing the isni measures, we make use of the  \code{R} package "Formula"  designed for handling model
#'        equations with multiple responses    and multiple sets of predictors . At a minimum, the user should supply a single-equation 
#'         in the typical form:  \code{response} ~ \code{Xterms} where \code{response} is the (numeric or factor) vector for the outcome of interest and \code{Xterms} 
#'        is a series of terms, separated by + operators, which specify a linear predictor for response. With the signle-equation specification, the \code{isniglm} function 
#'        will by default use the utility function \code{definemissingstatus} provided in the package to generate the 
#'        missingness status variables at the current and prior visits and then use \code{Xterms} as the observed missingness predictors for fitting a first-order transitional missing data model. 
#'        It is important to sort within-\code{id} observations  by time so that the missingness status variables can be defined correctly in this default setting.  The \code{isnimgm} then computes the MAR estimates and conducts ISNI computation
#'        to evaluate the rate of change of model estimates in the neighborhood of the MAR model where the missingness probability is allowed to depend on the unobserved value 
#'        of \code{response},   even after  conditioning on the other  missingness predictors. 
#'
#'        The above single-equation formula specification uses the same set of predictors for the outcome model and the missing data mechanism model for computing ISNI. 
#'        To use different sets of predictors, one can explicitly specifiy a two-equation formula as: \code{response} | \code{miss + missprior} ~ \code{Xterms} |  \code{Sterms},
#'        which specifies the formula  for the outcome model as \code{response} ~ \code{Xterms} and that for the missing data mechanism model as \code{miss + missprior} ~ \code{Sterms}, 
#'        where \code{Xterms} and the observed missingness predictors \code{Sterms} can be different, \code{miss} and \code{missprior} are the variable names in \code{data} denoting the missingness status at the 
#'        current and prior visits, respectively. 
#' @references Ma G, Troxel AB, Heitjan DF. An Index of Local Sensitivity to Nonignorable Dropout in Longitudinal Modeling. Stat Med. 2005;24:2129-2150. \cr
#' Xie H. Analyzing Longitudinal Clinical Trial Data with Nonignorable Missingness and Unknown Missingness Reasons. Comput Stat Data Anal. 2012;56:1287-1300. \cr
#' Xie H, Qian Y. Measuring the Impact of Nonignorability in Panel Data with Non-Monotone Nonresponse. Journal of Applied Econometrics. 2012;27:129-159. 
#' @name isnimgm
#' @aliases isnimgm
#' @import  matrixcalc mvtnorm nlme nnet  stats Formula
#' @importFrom nlme corCompSymm
#' @export isnimgm
#' @examples
#' models= y | g+gp ~   perf + sever+ as.factor(time) + group +as.factor(time):group   |
#'        as.factor(time) * group + yp+ perf + sever
## gmodel= as.factor(g)~ t3+t6+group+yp+perf+sever
## qolef$t12<-qolef$t1*qolef$group
## qolef$t32<-qolef$t3*qolef$group
## qolef$t62<-qolef$t6*qolef$group
##
#' qolef.isni=isnimgm(models, data=qolef, id=id)
#' summary(qolef.isni)
#'
## ymodel= y | g+gp ~   t1+t3+t6 + group  + t12 + t32+t62 + perf + sever | t3+t6+group+yp+perf+sever
## summary(isnimgm(ymodel, data=qolef, id=id))
#'
isnimgm = function(formula, data, cortype="CS", id, subset, weights, predprobobs, misni=FALSE) {
  


  ## (1) process the call and set up model frame.
  cl <- match.call()
  if (missing(data))  data <- environment(formula)
  if (missing(id)) stop("Users need to supply the name of the level-2 clusting variable in the data via the argument id.")
  
  mf <- match.call(expand.dots = FALSE)    
  m <- match(c("formula", "data", "weights", "subset", "id","predprobobs"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  f <- Formula(formula)
  if (any(length(f)>2)) stop("Cannot have more than two models") 
  mf$formula <- f
  options(na.action=na.pass)
  mf[[1L]] <- as.name("get_all_vars")
  mfvar<- eval(mf, parent.frame())
  if (!missing(subset)) {
    SubSet <-mfvar$subset
    mfvar <-mfvar[SubSet,]
                          }
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ID= model.extract(mf, "id")

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
  isni_WTs <- isni_id <- isni_outcome_ <- NULL 
  mf= cbind(mf, mfvar[!(names(mfvar) %in% names(mf))],  isni_id=ID, isni_WTs=WTs)  
  if (! missing(predprobobs))   mf$fitprobs_=  model.extract(mf, "predprobobs")  


  ##(2) Extract responses and predictors from the model frame
  ymodel=formula(f, lhs=1, rhs=1)
  x<- as.matrix(model.matrix(f, data=mf, rhs=1))
  y<- model.part(f,data=mf, lhs=1)[,1]  ## will be a vector
  if (nrow(x)<1)  print("No predictor variables specified for the outcome")
  options(na.action=na.omit)

  ## check if the missing status indicator g and gp are specified, if not add g and gp in the lhs of formula
  if (length(f)[1]==1) {
     cat("\n NOTE: The model formula specification does not include  the missingness status variables at the current and the prior visits. 
       The program will assume that data has already been sorted by the time variable within each level-2 ID variable and proceed to create the missingness status variables and then fit a transitional missingness mechanism model. \n")
     f <- update(f, .| g_ +gp_ ~.)
     mf <- cbind(mf, isni_outcome_=y)
     mf <- definemissingstatus(data=mf, id=isni_id, y=isni_outcome_) 
     if (ncol(model.part(f,data=mf, lhs=2)) != 2) stop("The missing data model should be in the form of g+gp ~ Sterm") 
       }
  gmodel <- formula(f, lhs=2,rhs=length(f)[2])

  ## ## Drop all observations with missing values in predictor matrix X to avoid NAs in ISNI computation.  
  missX<- apply(x, 2, FUN=function (u) any(is.na(u)))
  if (any(missX)) cat(paste("\n Note: There are missing values in fixed-effect outcome predictors-- ", paste(dimnames(x)[[2]][missX], collapse=" "), ". Observations with missing values in these predictors will be dropped out from computing ISNI, 
       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", sep=""))
  missX  <- apply(x, 1, FUN=function (u) any(is.na(u)))
  WTs=WTs[!missX]
  mf=mf[!missX,,drop=F]
  options(na.action=na.pass) 
  x<- as.matrix(model.matrix(f, data=mf, rhs=1))
  y<- model.part(f,data=mf, lhs=1)[,1]  
  xomit <-  as.matrix(model.matrix(f, data=mf[!is.na(y), ], rhs=1))
  if (ncol(x) != ncol(xomit)) {
        cat(paste("\n All variable names in fixed-effects design matrix of ISNI analysis including observations with missing outcomes: \n", paste (dimnames(x)[[2]], collapse=" ")))
        cat(paste("\n All variable names in fixed-effects design matrix of the MAR model excluding observations with missing outcomes:  \n", paste (dimnames(xomit)[[2]], collapse=" ")))
        stop("\n The design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function
               in model formula.  ")
     }    
  options(na.action=na.omit)
  maxT = max(table(mf$isni_id))
  sdy <- sd(y,na.rm=T)  


  ## fit the mgm with the observed data.
  if (cortype=="CS")  iggls= gls(ymodel, data=mf,correlation = corCompSymm(form =  ~ 1 | isni_id), weights=~1/isni_WTs, method="ML", na.action=na.exclude) else
  if (cortype=="AR1") iggls= gls(ymodel, data=mf,correlation = corAR1(form =  ~ 1 | isni_id), weights=~1/isni_WTs, method="ML",na.action=na.exclude) else
  if (cortype=="UN")  iggls= gls(ymodel, data=mf,correlation = corSymm(form =  ~ 1 | isni_id), weights=~1/isni_WTs, method="ML",na.action=na.exclude) else
  stop ("This Type of correlation is not implmented yet for ISNI computation")


  ##  fit a missing data model to obtain fitted probabilities for being observed.
  if (missing(predprobobs)) {
    mf.mdm= tmdm(gmodel, data=mf)
    mf$fitprobs=mf.mdm$obsprob 
    mf$A10= mf.mdm$A10;  mf$A20= mf.mdm$A20;  mf$A11= mf.mdm$A11
  } else  if ( misni==T) 
     stop("predprobobs needs to to a matrix to obtain ISNI vector for multiple nonignorability parameter")
  
 
  D=list(sigma=iggls$sigma, rho=intervals(iggls)$corStruct[,"est."])
  b<- iggls$coef
  nb = length(b)
  nD=length(D$sigma)+length(D$rho)
  npar= nb + nD

  uid = unique(mf$isni_id)
  nabla12=matrix(0, nrow=npar, ncol=3)
  ## compute nabla11 and nabla12
  nabla11=matrix(0, nrow=npar, ncol=npar)
  for (i in 1:length(uid)) {
    xi= as.matrix(x[mf$isni_id==uid[i],,drop=F])
    yi= y[mf$isni_id==uid[i]]
    ## Increase nabla11 only if there is at least one observed outcome
    if (!all(is.na(yi))) nabla11=nabla11+ fun.mgmsubi(yi=yi,xi=xi,maxT=maxT, b=b,D=D, cortype=cortype, transform=FALSE, case=1)
    gfiti= NULL; Afiti=NULL
    if (! missing(predprobobs))  gfiti= mf$fitprobs_[mf$isni_id==uid[i]]
    else Afiti= cbind(mf$A10[mf$isni_id==uid[i]], mf$A20[mf$isni_id==uid[i]], mf$A11[mf$isni_id==uid[i]])
    if (any(is.na(yi))) nabla12 = nabla12 + fun.mgmsubi(yi=yi,xi=xi,maxT=maxT, b=b,D=D, cortype=cortype, transform=FALSE, gfiti=gfiti,Afiti=Afiti, case=2)
  }
  invnabla11= solve(nabla11)
  invnabla11[1:nb,1:nb]= -iggls$varBeta
  isni=-invnabla11%*%nabla12

  se=sqrt(diag((-invnabla11)))
  bD= c(b,unlist(D))
  if (cortype=="CS" | cortype=="AR1" ) {
  names(bD)=c(names(b), names(D))
                                   } else
  if (cortype=="UN" )                {
  names(bD)=c(names(b), 'sigma', names(D[[2]]))
                             }
  isnivec=isni
  dimnames(isnivec)<-list(names(bD),c("ISNI_IO", "ISNI_DO", "ISNI_II"))
  if (misni==FALSE) {
       isni=apply(isni,1, sum); senstran<-abs((sdy*se)/isni)
   } else
   {
       isni=apply(abs(isni),1, sum); senstran<-abs((sdy*se)/isni)
   }
  
  res=list(coefficients=bD,se=se,isni=c(isni),c=c(senstran), call=cl, isnivec=isnivec, misni=misni, 
                       logLik=iggls$logLik, aic=summary(iggls)$AIC, bic=summary(iggls)$BIC)
  class(res) = c(res$class, "isnimgm")
  return(res)
}



#' Function to print out a summary of isnimgm  object in a matrix form.
#'
#' @param object the isnimgm object obtained from the isnimgm function
#' @param digits the number of significant digits to use when printing
#' @param ... additional arguements
#' @export summary.isnimgm
#' @export
#' @name summary.isnimgm
#' @aliases summary.isnimgm

summary.isnimgm<-function(object, digits = max(3, getOption("digits") - 2), 
            ...) {

  if (class(object) != "isnimgm")  stop('Invalid object class')
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", 
        collapse = "\n"), "\n\n", sep = "")
  ## Name the columns
    if (object$misni==T) isniname<-c('MAR Est.','Std. Err','MISNI','c') else 
       isniname<-c('MAR Est.','Std. Err','ISNI','c')

  ## Set up matrix to hold result
  res<-matrix(0,length(object$coef),length(isniname))
  dimnames(res)<-list(names(object$coef),isniname)

  for (i in 1:length(object$coef))
       res[i,]<- c(object$coef[i],object$se[i],object$isni[i],object$c[i])
  printCoefmat(res, digits = digits,  cs.ind = 1:2)
}

   
    #' Function to print  the isnimgm object.
    #'
    #' \code{print} method for class isnimgm
    #' @param x the isnimgm object obtained from the isnimgm function
    #' @param digits the number of significant digits to use when printing
    #' @param ... further arguments passed to or from other methods.
    #'
    #' @return The function print.isnimgm prints the model call, isni and c statistics from the isnimgm object.
    #' @name print.isnimgm
    #' @aliases print.isnimgm
    #' @export print.isnimgm
    #' @export
    print.isnimgm<-function(x, digits = max(3, getOption("digits") - 2), ...) {

     if (class(x) != "isnimgm")  stop('Invalid object class')
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
     cat("\nlogLik of the MAR model: ", paste(format(x$logLik,digits=digits), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
     cat("\nAIC of the MAR model: ", paste(format(x$aic, digits=digits), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
     cat("\nBIC of the MAR model: ", paste(format(x$bic, digits=digits), sep = "\n", 
        collapse = "\n"), "\n", sep = "")
     cat("\n")
     invisible(x)
    
    }


## Internal Function for various subject-level computation required for ISNIGLS.
##
## Calculate subject-level quantities when the regression outcome is subject to missingness and follows marginal multivaraite Gaussian Models. 
## @param yi vector of the response for the ith subject
## @param xi matrix of the covariates for the ith subject
## @param maxT maximum number of visits
## @param b  the mean parameter vector beta
## @param D  the vector of unique parameters in the variance-covariance matrix for the error term in the GLS model for Y
## @param correlation  the form of within-subject correlation structure in the GLS model for Y
## @param transform logical indicating wether or not the parameter in D is transformed.
## @param gfiti  vector of predicted probabilities of being observed for all the observations from the ith subject
## @param Afiti matrix of 3 columns of predicted transitional probabilities for the missing observations from the ith subject. 
## @param case   1: calculated nabla11_i; 2: calculate nabla12_i
## @aliases fun.glssubi
## @export
fun.mgmsubi <- function ( yi, xi, maxT=maxT, b, D, cortype,transform=FALSE, gfiti=NULL, Afiti=NULL,  case=1) {


  ## output
  ## res -- if case=1, res=hessian matrix
  ##        if case=2, res=\nabla_12 for subject i.
  ##        if case=3, res=negative loglikelihood, retired and  unused in the package.
  ##        if case=4, res=negative score value, retired and unused in the pacakge
  ##        if case=5, res=der.meanymi, retired and unused in the package.


  obsi  = !is.na(yi)
  xiobs = as.matrix(xi[obsi,,drop=F])
  yiobs = as.vector(yi[obsi])
  niobs = length(yiobs)
  muiobs = as.vector(xiobs %*% as.matrix(b))
  riobs= yiobs - muiobs
  nb = length(b)
  nD=length(D$sigma)+length(D$rho)
  npar= nb + nD
  sigma= D$sigma; rho=D$rho
 

  ## obtain the variance-covariance matrix
  if (cortype=="CS") {
    covyi= fun.csmat(sigma=sigma, rho=rho, obsi=length(yi),transform=transform, case=1)
  }

  if (cortype=="AR1") {
    covyi= fun.ar1mat(sigma=sigma, rho=rho, obsi=length(yi),transform=transform, case=1)
  }

  if (cortype=="UN") {
    col=matrix(0, nrow=maxT, ncol=maxT)
    for (i in 1:(maxT-1)){
                          col[(i+1):maxT,i]=rho[((i-1)*maxT-i*(i-1)/2+1) : (i*maxT-i*(i+1)/2)]
                         }
    rho2=col+t(col)+diag(maxT)

    covyi= fun.generalmat(sigma=sigma, rho=rho2, obsi=length(yi),transform=transform, case=1)
  }

    covyiobs= covyi[obsi,obsi]
    ## only invert if there is yiobs is not empty
    if (niobs>0) icovyiobs= solve(covyiobs)
    ##print(icovyiobs)
 
  ## case=1, compute the nabla11 for subject i
  if (case==1) {
    nabla11= matrix(0, npar, npar)
    ## return 0 if there is no observed outcome.
    if (niobs==0) return(nabla11)
    nabla11[1:nb, 1:nb]=-t(xiobs)%*%icovyiobs%*%xiobs
  if (cortype=="CS") {
    covid1 = fun.csmat(sigma=sigma,rho=rho,obsi=niobs,transform=transform,case=2)
    covid2 = fun.csmat(sigma=sigma,rho=rho,obsi=niobs,transform=transform,case=3)

    Ai= covid1
    derAi= matrix(NA, nrow=niobs, ncol=niobs)
    for (j in 1:length(Ai)) Ai[[j]]= icovyiobs%*%covid1[[j]]%*%icovyiobs

    for (i in 1:nD) {
      for (j in 1:nD){
        derAi= -icovyiobs%*%(covid1[[i]]%*%icovyiobs%*%covid1[[j]]-covid2[i,j,,]+covid1[[j]]%*%icovyiobs%*%covid1[[i]])%*%icovyiobs
        nabla11[(nb+i),(nb+j)]= -0.5*sum(diag(-Ai[[i]]%*%covid1[[j]]+icovyiobs%*%covid2[i,j,,]))+0.5*t(as.matrix(riobs))%*%derAi%*%riobs
      }
    }

    for (j in 1:nD) {nabla11[1:nb,(nb+j)]= -0.5*t(xiobs)%*%(Ai[[j]]+t(Ai[[j]]))%*%riobs}

    nabla11[(nb+1):npar, 1:nb]=t(nabla11[1:nb, (nb+1):(npar)])
    }

  if (cortype=="AR1") {
    covid1 = fun.ar1mat(sigma=sigma,rho=rho,obsi=niobs,transform=transform,case=2)
    covid2 = fun.ar1mat(sigma=sigma,rho=rho,obsi=niobs,transform=transform,case=3)
    Ai= covid1
    derAi= matrix(NA, nrow=niobs, ncol=niobs)
    for (j in 1:length(Ai)) Ai[[j]]= icovyiobs%*%covid1[[j]]%*%icovyiobs
    for (i in 1:nD) {
      for (j in 1:nD){
        derAi= -icovyiobs%*%(covid1[[i]]%*%icovyiobs%*%covid1[[j]]-covid2[i,j,,]+covid1[[j]]%*%icovyiobs%*%covid1[[i]])%*%icovyiobs
        nabla11[(nb+i),(nb+j)]= -0.5*sum(diag(-Ai[[i]]%*%covid1[[j]]+icovyiobs%*%covid2[i,j,,]))+0.5*t(as.matrix(riobs))%*%derAi%*%riobs
      }
    }

    for (j in 1:nD) nabla11[1:nb,(nb+j)]= -0.5*t(xiobs)%*%(Ai[[j]]+t(Ai[[j]]))%*%riobs

    nabla11[(nb+1):npar, 1:nb] = t(nabla11[1:nb, (nb+1):(npar)])
 }

 if (cortype=="UN") {
    covid1 = fun.generalmat(sigma=sigma,rho=rho2,obsi=obsi,transform=transform,case=2, case2=1)
    covid2 = fun.generalmat(sigma=sigma,rho=rho2,obsi=obsi,transform=transform,case=3)
    Ai= covid1
    derAi= matrix(NA, nrow=niobs, ncol=niobs)
    for (j in 1:length(Ai)) Ai[[j]]= icovyiobs%*%covid1[[j]]%*%icovyiobs
    for (i in 1:nD) {
      for (j in 1:nD){
        derAi= -icovyiobs%*%(covid1[[i]]%*%icovyiobs%*%covid1[[j]]-covid2[i,j,,]+covid1[[j]]%*%icovyiobs%*%covid1[[i]])%*%icovyiobs
        nabla11[(nb+i),(nb+j)]= -0.5*sum(diag(-Ai[[i]]%*%covid1[[j]]+icovyiobs%*%covid2[i,j,,]))+0.5*t(as.matrix(riobs))%*%derAi%*%riobs
      }
    }

    for (j in 1:nD) nabla11[1:nb,(nb+j)]= -0.5*t(xiobs)%*%(Ai[[j]]+t(Ai[[j]]))%*%riobs

    nabla11[(nb+1):npar, 1:nb]=t(nabla11[1:nb, (nb+1):(npar)])
  }
    return(nabla11)
  }## end if case=1

  ## case=2, calculate the \nabla_12 for subject i.
  if (case==2) {

      if (any(is.na(yi))){

        misi  = is.na(yi)
        ximis = as.matrix(xi[misi,,drop=F])
        yimis = as.vector(yi[misi])
        ##fitp0i = gfiti[misi]
        Afit0i=matrix(0, nrow=sum(misi==T), ncol=3)
        if (is.null(gfiti))  Afit0i =Afiti[misi, ,drop=F]
        else Afit0i[,1]= gfiti[misi]
        

        cit = covyi[misi, obsi,drop=F]
        dmean.D= matrix(0, nrow=nD, ncol=ncol(Afit0i))
        if (niobs==0) {dmean.beta=t(ximis)%*% Afit0i; return(rbind(dmean.beta, dmean.D)) }
        
        dmean.beta = t(ximis - cit %*% icovyiobs %*% xiobs) %*% Afit0i ## as.matrix(fitp0i)
        ##numeric(nD)

        if (cortype=="CS") {
        derD= fun.csmat(sigma=sigma,rho=rho,obsi=length(yi),transform=transform,case=2)
                          } else
        if (cortype=="AR1") {
        derD= fun.ar1mat(sigma=sigma,rho=rho,obsi=length(yi),transform=transform,case=2)
                           }              
        if  (cortype=="UN")  {
        derD= fun.generalmat(sigma=sigma,rho=rho2,obsi=length(yi),transform=transform,case=2, case2=2)
                          }

        for (k in 1:nD) {
          Aij<-icovyiobs%*%derD[[k]][obsi,obsi]%*%icovyiobs
          dercitDk<- derD[[k]][misi,obsi,drop=F]
          dmean.D[k,]<-t(dercitDk%*%icovyiobs%*%as.matrix(riobs) -cit%*%Aij%*%as.matrix(riobs)) %*% Afit0i ##as.matrix(fitp0i)
                        }

       ## dercov.sigma<-fun.csmat(sigma=sigma,rho=rho,nobs=length(yi),transform=transform,case=2)
       ## Aij<-icovyiobs%*%dercov.sigma[obsi,obsi]%*%icovyiobs
       ## dercit.sigma<- dercov.sigma[misi,obsi,drop=F]
       ##  dmean.sigma<-t(dercit.sigma%*%icovyiobs%*%as.matrix(riobs) -cit%*%Aij%*%as.matrix(riobs)) %*% Afit0i ##as.matrix(fitp0i)

      ##  dercov.rho<-fun.csmat(sigma=sigma,rho=rho,nobs=length(yi),transform=transform,case=3)
      ##  Aij<-icovyiobs%*%dercov.rho[obsi,obsi]%*%icovyiobs
      ##  dercit.rho<- dercov.rho[misi,obsi,drop=F]
      ##  dmean.rho<-t(dercit.rho%*%icovyiobs%*%as.matrix(riobs) -cit%*%Aij%*%as.matrix(riobs))%*% Afit0i ##as.matrix(fitp0i)

        res = rbind(dmean.beta, dmean.D) ##c(dmean.beta, dmean.D)
        return(res)
      } else
        res = numeric(dim(xiobs)[2]+2)
    
  } ## end if case=2

 
  res

}

## Internal Function for generate compound symmetry matrix and its first and second derivatives wrt D
##
## Calculate the compound symmetry matrix and its first and second derivatives wrt D.
## @param sigma standard deviation
## @param rho correlation coefficient
## @param obsi dimension of the square matrix
## @param transform logical indicating wether or not the parameter in D is transformed.
## @param case   1: return nobs x nobs CS matrix; 2: return a list for first derivatives of the compound symmetry matrix wrt sigma and rho;
##               3: return an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho.
## @aliases fun.csmat
## @export
fun.csmat<-function(sigma,rho,obsi,transform,case=1) {
  ## calculate the the related quantity for compound symmetry vcov matrix.
  ## input
  ## sigma  -- the standard deviation
  ## rho  ---  the correlation coefficient
  ## nobs --- the number of observations, nobs>=1
  ## transform --- logical number indicator if the parameters are transformed

  ## output
  ## case=1 --- return nobs x nobs CS matrix
  ## case=2 --- return a list for first derivatives of the compound symmetry matrix wrt sigma and rho
  ## case=3 --- return an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho

  if (transform==FALSE) {
    if (case==1) {
         nobs = obsi
         res<-sigma^2*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
                 }
    else if (case==2) {
         nobs = obsi
         derSigma<- 2*sigma*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
         derRho<- sigma^2*(matrix(1,nrow=nobs,ncol=nobs)-diag(1,nrow=nobs))
         res <- list(derSigma=derSigma, derRho=derRho)
         res
                       }
    ## derivative wrt to rho
    else if (case==3) {
      nobs = obsi
      res = array(NA, c(2,2, nobs,nobs))
      res[1,1,,]=2*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
      res[1,2,,]=res[2,1,,]=2*sigma*(matrix(1,nrow=nobs,ncol=nobs)- diag(1, nrow=nobs))
      res[2,2,,]=matrix(0, nrow=nobs, ncol=nobs)
      res
                      }
  } else
  {
    sigma<-fun.trans2pos(sigma)
    rho <- fun.trans2rho(rho)
    if (case==1) {
         nobs = obsi
         res<-sigma^2*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
                 }
    else if (case==2) {
         nobs = obsi
         derSigma<- 2*sigma*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
         derRho<- sigma^2*(matrix(1,nrow=nobs,ncol=nobs)-diag(1,nrow=nobs))
         res <- list(derSigma=derSigma, derRho=derRho)
         res
                       }
    ## derivative wrt to rho
    else if (case==3) {
      nobs = obsi
      res = array(NA, c(2,2, nobs,nobs))
      res[1,1,,]=2*(rho*matrix(1,nrow=nobs,ncol=nobs)+diag(1-rho,nrow=nobs))
      res[1,2,,]=res[2,1,,]=2*sigma*(matrix(1,nrow=nobs,ncol=nobs)- diag(1, nrow=nobs))
      res[2,2,,]=matrix(0, nrow=nobs, ncol=nobs)
      res
                      }
  }
  res

}


## Internal Function for generate AR1 matrix and its first and second derivatives wrt D
##
## Calculate the autoregressive matrix and its first and second derivatives wrt D.
## @param sigma standard deviation
## @param rho autoregressive  parameter
## @param obsi dimision of the square matrix
## @param transform logical indicating wether or not the parameter in D is transformed.
## @param case   1: return nobs x nobs AR1 matrix; 2: return a list for first derivatives of the compound symmetry matrix wrt sigma and rho;
##               3: return an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho.
## @aliases fun.ar1mat
## @export
fun.ar1mat<-function(sigma,rho,obsi,transform,case=1) {
  ## calculate the the related quantity for AR1 (autoregressive process of order 1) vcov matrix.
  ## input
  ## sigma  -- the standard deviation
  ## rho  ---  the autoregressive  parameter
  ## nobs --- the number of observations, nobs>=1
  ## transform --- logical number indicator if the parameters are transformed

  ## output
  ## case=1 --- return nobs x nobs AR1 matrix
  ## case=2 --- return a list for first derivatives of the AR1 matrix wrt sigma and rho
  ## case=3 --- return an array containing 2nd derivatives of the AR1 matrix wrt sigma and rho

  if (transform==FALSE) {
  
  if (case==1) {nobs = obsi
                res<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                res[i,j]<-sigma^2*rho^abs(i-j)
                                  }
                                  }
               } else 
  if (case==2) {nobs = obsi
                derSigma<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                derSigma[i,j] <- 2*sigma*rho^abs(i-j)
                }}
                derRho<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                derRho[i,j] <- sigma^2*abs(i-j)*rho^(abs(i-j)-1)
                }}
                res <- list(derSigma=derSigma, derRho=derRho)
               } else 

  ## derivative wrt to rho

  if (case==3) {nobs = obsi
                res = array(NA, c(2,2, nobs,nobs))
                res11 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                res11[i,j]<-2*rho^abs(i-j)
                }}
                res[1,1,,]=res11

                res12 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs){
                for (j in 1:nobs){
                res12[i,j]<-2*sigma*abs(i-j)*rho^(abs(i-j)-1)
                }}
                res[1,2,,]=res[2,1,,]=res12

                res22 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs){
                for (j in 1:nobs){
                if(abs(i-j)<=1) res22[i,j] <- 0 else
                                res22[i,j]<-sigma^2*abs(i-j)*(abs(i-j)-1)*rho^(abs(i-j)-2)
                }}
                res[2,2,,]=res22
               }
  } else
  {
    sigma<-fun.trans2pos(sigma)
    rho <- fun.trans2rho(rho)
  if (case==1) {nobs = obsi
                res<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                res[i,j]<-sigma^2*rho^abs(i-j)
                }}
               } else 
  if (case==2) {nobs = obsi
                derSigma<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                derSigma[i,j] <- 2*sigma*rho^abs(i-j)
                }}
                derRho<-matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                derRho[i,j] <- sigma^2*abs(i-j)*rho^(abs(i-j)-1)
                }}
                res <- list(derSigma=derSigma, derRho=derRho)
               } else 

  ## derivative wrt to rho

  if (case==3) {nobs = obsi
                res = array(NA, c(2,2, nobs,nobs))
                res11 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs) {
                for (j in 1:nobs) {
                res11[i,j]<-2*rho^abs(i-j)
                }}
                res[1,1,,]=res11

                res12 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs){
                for (j in 1:nobs){
                res12[i,j]<-2*sigma*abs(i-j)*rho^(abs(i-j)-1)
                }}
                res[1,2,,]=res[2,1,,]=res12

                res22 <- matrix(0, nrow=nobs, ncol=nobs)
                for (i in 1:nobs){
                for (j in 1:nobs){
                if(abs(i-j)<=1) res22[i,j] <- 0 else
                                res22[i,j]<-abs(i-j)*(abs(i-j)-1)*rho^(abs(i-j)-2)
                }}
                res[2,2,,]=res22
               }
 }
 res
}

## Internal Function for generate a general symmetric matrix and its first and second derivatives wrt D
## Calculate the general symmetric matrix and its first and second derivatives wrt D.
## @param sigma standard deviation
## @param rho correlation coefficient
## @param obsi dimension of the square matrix
## @param transform logical indicating wether or not the parameter in D is transformed.
## @param case   1: return nobs x nobs general symmetric  matrix; 2: return a list for first derivatives of the general symmetric matrix wrt D;
##               3: return an array containing 2nd derivatives of the general symmetric matrix wrt D.
## @param case2 indicator for variance and covariance parameters. 
## @aliases fun.generalmat
## @export
fun.generalmat<-function(sigma,rho,obsi,transform,case=1, case2=NULL) {
  ## calculate the the related quantity for compound symmetry vcov matrix.
  ## input
  ## sigma  -- the standard deviation
  ## rho  ---  the correlation coefficient
  ## nobs --- the number of observations, nobs>=1
  ## transform --- logical number indicator if the parameters are transformed

  ## output
  ## case=1 --- return nobs x nobs CS matrix
  ## case=2 --- return a list for first derivatives of the compound symmetry matrix wrt sigma and rho
  ## case=3 --- return an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho

if (transform==FALSE) {
                       if (case==1)     {rhoi=rho[1:obsi, 1:obsi]
                                         res<-sigma^2*rhoi
                                        }                   
                      else if (case==2) {if (case2==1) {
                                         rhoi=rho[obsi, obsi]
                                         nobs=sum(obsi)
                                         nful=length(obsi)
                                         nrho=sum(lower.tri(rho))
                                         res=vector("list", 1+length(nrho))

                                         row<-c()
                                         for (i in 1:(nful-1)){
                                         row<- append(row,rep(i, (nful-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(nful-1)){
                                         col<- append(col,seq(i+1, nful))
                                                              }
                                         w<- which(obsi==FALSE)

                                         for (i in 1:(1+nrho)){
                                              if (i==1){
                                              res[[i]]=2*sigma*rhoi
                                                       } else
                                                       {                                                                                                 
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=sigma^2
                                                       a[col[i-1],row[i-1]]=sigma^2
                                                       res[[i]]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[[i]]=a[-w,-w]
                                                               }
                                                        }
                                                              }
                                                              }
                                         else if (case2==2)    {
                                         rhoi=rho[1:obsi, 1:obsi]
                                         nrho=sum(lower.tri(rho))
                                         res=vector("list", 1+length(nrho))

                                         row<-c()
                                         for (i in 1:(obsi-1)){
                                         row<- append(row,rep(i, (obsi-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(obsi-1)){
                                         col<- append(col,seq(i+1, obsi))
                                                              }

                                         for (i in 1:(1+nrho)){
                                              if (i==1){
                                              res[[i]]=2*sigma*rhoi
                                                       } else
                                                       { 
                                                       a=matrix(0, nrow=obsi, ncol=obsi)
                                                       a[row[i-1],col[i-1]]=sigma^2
                                                       a[col[i-1],row[i-1]]=sigma^2
                                                       res[[i]]=a
                                                       }
                                                              }
                                                              }              
                                        }
                      else if (case==3) {rhoi=rho[obsi, obsi]
                                         nobs=sum(obsi)
                                         nful=length(obsi)
                                         nrho=sum(lower.tri(rho))
                                         res = array(NA, c(1+nrho,1+nrho, nobs,nobs))

                                         row<-c()
                                         for (i in 1:(nful-1)){
                                         row<- append(row,rep(i, (nful-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(nful-1)){
                                         col<- append(col,seq(i+1, nful))
                                                              }

                                         w<- which(obsi==FALSE)

                                         for (i in 1:(1+nrho)){
                                         for (j in 1:(1+nrho)){
                                              if (i==1 & j==1){
                                              res[i,j,,]=2*rhoi
                                                              } else
                                              if (i==1 & j>1){                                                                                                 
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[j-1],col[j-1]]=2*sigma
                                                       a[col[j-1],row[j-1]]=2*sigma
                                                       res[i,j,,]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[j-1],col[j-1]]=2*sigma
                                                       a[col[j-1],row[j-1]]=2*sigma
                                                       res[i,j,,]=a[-w,-w]
                                                               }
                                                               } else
                                              if (j==1 & i>1){                                                                                            
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[i,j,,]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[i,j,,]=a[-w,-w]
                                                               }
                                                             } else 
                                              if(i>1 & j>1) {
                                              res[i,j,,]=matrix(0, nobs, nobs)
                                                            }
                                                              }
                                                              }
                                        }
                      } else

                     {sigma<-fun.trans2pos(sigma)
                      rho  <- fun.trans2rho(rho) 

                       if (case==1)     {rhoi=rho[1:obsi, 1:obsi]
                                         res<-sigma^2*rhoi
                                        }                   
                      else if (case==2) {if (case2==1) {
                                         rhoi=rho[obsi, obsi]
                                         nobs=sum(obsi)
                                         nful=length(obsi)
                                         nrho=sum(lower.tri(rho))
                                         res=vector("list", 1+length(nrho))

                                         row<-c()
                                         for (i in 1:(nful-1)){
                                         row<- append(row,rep(i, (nful-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(nful-1)){
                                         col<- append(col,seq(i+1, nful))
                                                              }
                                         w<- which(obsi==FALSE)

                                         for (i in 1:(1+nrho)){
                                              if (i==1){
                                              res[[i]]=2*sigma*rhoi
                                                       } else
                                                       {                                                                                                 
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=sigma^2
                                                       a[col[i-1],row[i-1]]=sigma^2
                                                       res[[i]]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[[i]]=a[-w,-w]
                                                               }
                                                        }
                                                              }
                                                              }
                                         else if (case2==2)    {
                                         rhoi=rho[1:obsi, 1:obsi]
                                         nrho=sum(lower.tri(rho))
                                         res=vector("list", 1+length(nrho))

                                         row<-c()
                                         for (i in 1:(nful-1)){
                                         row<- append(row,rep(i, (nful-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(nful-1)){
                                         col<- append(col,seq(i+1, nful))
                                                              }

                                         for (i in 1:(1+nrho)){
                                              if (i==1){
                                              res[[i]]=2*sigma*rhoi
                                                       } else
                                                       { 
                                                       a=matrix(0, nrow=obsi, ncol=obsi)
                                                       a[row[i-1],col[i-1]]=sigma^2
                                                       a[col[i-1],row[i-1]]=sigma^2
                                                       res[[i]]=a
                                                       }
                                                              }
                                                              }   
                                        }
                      else if (case==3) {rhoi=rho[obsi, obsi]
                                         nobs=sum(obsi)
                                         nful=length(obsi)
                                         nrho=sum(lower.tri(rho))
                                         res = array(NA, c(1+nrho,1+nrho, nobs,nobs))

                                         row<-c()
                                         for (i in 1:(nful-1)){
                                         row<- append(row,rep(i, (nful-i)))
                                                              }
                                         col<-c()
                                         for (i in 1:(nful-1)){
                                         col<- append(col,seq(i+1, nful))
                                                              }
                                         w<- which(obsi==FALSE)

#to.upper<-function(X) t(X)[lower.tri(X,diag=FALSE)]
#v<-to.upper(temp)
#w<-which(is.na(v))
#if (row[j-1] %in% w | col[j-1] %in% w) 

                                         for (i in 1:(1+nrho)){
                                         for (j in 1:(1+nrho)){
                                              if (i==1 & j==1){
                                              res[i,j,,]=2*rhoi
                                                              } else
                                              if (i==1 & j>1){                                                                                                 
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[j-1],col[j-1]]=2*sigma
                                                       a[col[j-1],row[j-1]]=2*sigma
                                                       res[i,j,,]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[j-1],col[j-1]]=2*sigma
                                                       a[col[j-1],row[j-1]]=2*sigma
                                                       res[i,j,,]=a[-w,-w]
                                                               }
                                                               } else
                                              if (j==1 & i>1){                                                                                            
                                               if (nobs==nful){
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[i,j,,]=a
                                                               }else
                                                               {
                                                       a=matrix(0, nrow=nful, ncol=nful)
                                                       a[row[i-1],col[i-1]]=2*sigma
                                                       a[col[i-1],row[i-1]]=2*sigma
                                                       res[i,j,,]=a[-w,-w]
                                                               }
                                                             } else 
                                              if(i>1 & j>1) {
                                              res[i,j,,]=matrix(0, nobs, nobs)
                                                            }
                                                              }
                                                              }
                                        }
                     }
res
}


#' Utility function to generate missing status variables in longitudinal data with dropout and/or intermittent missingness.
#'
#' @param data the name of the panel dataset in the long format with each row denoting a subject-visit observation
#'       for ALL the planned visits, regardless of being missed or not. When a subject is lost to follow up, the data set
#'        must include the observation at the first time of being lost to follow up. 
#' @param id   the name of the level-2 clustering variable. 
#' @param time  the name of the variable denoting the time of the visit. Can set time=NULL if data is already sorted by id and time within 
#'                id. 
#' @param y the name of the outcome variable of the interest that is subject to missingness. 
#' @return a dataset with the following three new variables added: 
#' \itemize{
#'    \item    g_ : missingness indicator, "O"-observed, "I"-intermittent missing, "D"-dropout
#'    \item    gp_: missingness indicator in the previous visit, "O"-observed, "I"-intermittent missing, "D"-dropout, "U"-undefined. 
#'    \item    yp_: the immediately observed prior outcome.
#'  }
#' @export definemissingstatus
#' @name definemissingstatus
#' @examples
#' qolefnew <- definemissingstatus(qolef, id=id, time=time, y=y)
definemissingstatus<-function(data, id, time, y) {
  

  if (missing(data)) stop("Need to specify the name of the dataset for defining missingness status")
  if (missing(id)) stop("Need to specify the name of the level-2 clustering variable for  defining missingness status")
  if (missing(y)) stop("Need to specify the name of the outcome variable for  defining missingness status")

  id=as.character(substitute(id))
  if (!missing(time)) time=as.character(substitute(time))
  y=as.character(substitute(y))

  ## first sort the data by id and time
  if (!missing(time)) data = data[order(data[,id], data[,time]),]
  uid = unique(data[,id])
  lastobsT=numeric(length(uid))  
  g=gp=res=NULL

  for (i in 1:length(uid)) {
   datai <- data[data[,id]==uid[i],,drop=F]  
   ypi <-gi<-gpi<-numeric(dim(datai)[1])
   maxT=nrow(datai)
   if (any(!is.na(datai[,y]))) lastobsT[i] <- max((1:nrow(datai))[!is.na(datai[,y])]) else
    lastobsT[i] <- 0 

  for (j in 1:nrow(datai)) {

    ## assign values to yp, g, gp 
     if (j==1) {
        ypi[1]=NA;
        if (!is.na(datai[j,y])) gi[j]= "O" else
        if (lastobsT[i]<1) gi[j]="D" else 
        gi[j]="I"  
        gpi[j]="U"
       if (j < maxT) ypi[j+1] <- datai[j,y]
                               } else
                               {
        if (j < maxT) ypi[j+1] = ifelse(is.na(datai[j,y]), ypi[j],  datai[j, y])
         gpi[j] = gi[j-1]
         if (j < lastobsT[i]) gi[j] = ifelse( is.na(datai[j,y]), "I", "O") else
         if (j == lastobsT[i])  gi[j] = ifelse( is.na(datai[j,y]), "D","O") else
         if (j > lastobsT[i])   gi[j] ="D"  
                                } 
                             } ## end of  for loop for index j
  
  res = rbind(res, data.frame(datai, yp_=ypi,g_=gi,gp_=gpi))  
  }  ## end of the outer for loop for index i. 
  res
}


#' Function to fit the transitional missing data model and obtain the predicted probabilities of being observed for all observations.
#'
#' Fit missing data model and obtain predicted probabilities of being observed for all observations.
#' @param formula a formula to specify a multinomial transitional missing data model  in the form of g+gp ~Sterms. 
#' @param data the name of the dataset for fitting missing data mechansim model
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process for the outcome model and the missingness mechanism model.
#' @param weights an optional vector of "prior weights" to be used in the fitting process for the missingness mechanism model.
#'               Should be NULL or a numeric vector.
#' @aliases tmdm
#' @export tmdm
#' @examples
#' qolefitg <- tmdm(g+gp~as.factor(time)+group+perf+sever,data=qolef)
tmdm<- function(formula, data, weights, subset) {
   
   ## (1) process the call and set up model frame.
   cl <- match.call()
   if (missing(data))  data <- environment(formula)
  
   mf <- match.call(expand.dots = FALSE)    
   m <- match(c("formula", "data",  "subset", "weights"), names(mf), 0L)
   mf <- mf[c(1L, m)]
   f <- Formula(formula)
   if (any(length(f) !=1 )) stop ("Model specification for the missing data model is wrong. Please check if  missing data model is specified as g +gp ~Sterms.")
   mf$formula <- f
   mf[[1L]] <- as.name("model.frame") 
   options(na.action=na.pass)
   mf <- eval(mf, parent.frame())
   
   ## This is the function to fit the multinomial logit transitional model for the missingness
   if (missing(weights)) WTs <- NULL else 
         WTs <- model.extract(mf, "weights")
   if (!is.null(WTs) && !is.numeric(WTs)) 
        stop("'weights' must be a numeric vector")
   if (!is.null(WTs) && any(WTs < 0)) 
        stop("negative weights not allowed")
   if (is.null(WTs))
        WTs <- rep(1, nrow(mf))
  
   WTs_ <- NULL
   mf$WTs_= WTs
   mf$obsprob_= NA ## initialize the hhat column.
   mf$A10_= NA
   mf$A20_= NA
   mf$A11_= NA


   ##(2) Extract responses and predictors from the model frame
   if (ncol(model.part(f,data=mf, lhs=1)) != 2) stop("Need to specify both g_ and gp_ as the responses for  the transitional missing data model") 
   mf$g_<- model.part(f,data=mf, lhs=1)[,1]
   mf$gp_<- model.part(f,data=mf, lhs=1)[,2]
   gmodel=formula(f, lhs=1, rhs=1)
   gmodel <- as.formula(paste("g_ ~ ", gmodel[3], sep=" " ))

   s<- as.data.frame(model.matrix(gmodel, data=mf))
   if (nrow(s)<1) print("No predictor variables specified for the outcome")
   options(na.action=na.omit)


   if (any(!(mf$g_ %in% c("O","I","D")))) stop("Data error: missing status variable must take D, I or O")
   if (any(!(mf$gp_ %in% c("O","I","D","U")))) stop("Data error: missing status variable
                for the prior visit must take D, I, O or U")

   if (any(mf$gp_=="I" & mf$g_=="D")) {
    if (!("D" %in% levels(mf$gp_))) levels(mf$gp_) <- c(levels(mf$gp_), "D")
     mf$gp_[mf$gp_=="I" & mf$g_=="D"]= "D"
   }


   ## set reference level of the response for logistic regression.
   mf$g_=as.factor(mf$g_)
   mf$g_=relevel(mf$g_, "O")

   ## (1) gp="D: set hhat=0 for all observations after dropout time
   mf$obsprob_[mf$gp_=="D"]=0; mf$A10_[mf$gp_=="D"]=0; mf$A20_[mf$gp_=="D"]=0; mf$A11_[data$gp=="D"]=0

   ## (2) gp="O": fit multinomial logistic regression to obtain hhat
   mf.v0 = mf[mf$gp_=="O",,drop=F]
   s.v0= s[mf$gp_=="O",,drop=F]
   if (! any(mf.v0$g_=="O")) {            ## If the set of g values is all on "I", "D" or "I, D"  in this subsample.This is essentially dropouts.
      mf$obsprob_[mf$gp_=="O"]=0; mf$A10_[mf$gp_=="O"]=0; mf$A20_[mf$gp_=="O"]=0; mf$A11_[mf$gp_=="O"]=0;      
    }   else if (all(mf.v0$g_=="O")) { ## If all data are observed in this subsample
     mf$obsprob_[mf$gp_=="O"]=1; mf$A10_[mf$gp_=="O"]=1; mf$A20_[mf$gp_=="O"]=0; mf$A11_[mf$gp_=="O"]=0;
      } else if (any(mf.v0$g_!="O")) {
        missS  <- apply(s.v0, 2, FUN=function (u) any(is.na(u)))
        s.v0$WTs_=mf.v0$WTs_
        s.v0$g_=mf.v0$g_
        if (length(unique(mf.v0$g_))>2) {     ## g includes "O, I, D" in this subsample.  
          mdm.v0 =multinom(g_ ~ .-1- WTs_, data=s.v0[,!missS], weights=WTs_, Hess=TRUE,maxit=500)
          mdm.v0.fit=as.data.frame(mdm.v0$fit)
          mf$obsprob_[mf$gp_=="O"]=mdm.v0.fit$O; mf$A10_[mf$gp_=="O"]=as.numeric(mf.v0$g_=="I")-mdm.v0.fit$I; 
          mf$A20_[mf$gp_=="O"]=as.numeric(mf.v0$g_=="D")-mdm.v0.fit$D; mf$A11_[mf$gp_=="O"]=0; 
              }  else if (length(unique(mf.v0$g_))==2)  {  ## g includes "O, I", or "O,D" in this subsample.
                  mdm.v0 =multinom(g_ ~ .-1- WTs_, data=droplevels(s.v0[,!missS]), weights=WTs_, Hess=TRUE,maxit=500)
                  ## mdm.v0 =glm(g_ ~ .-1- WTs_, data=s.v0[,!missS], weights=WTs_, family=binomial(link=logit))
                  mf$obsprob_[mf$gp_=="O"]=1-mdm.v0$fit; 
                  mf$A10_[mf$gp_=="O"]=as.numeric(mf.v0$g_=="I")*(1-mdm.v0$fit);
                  mf$A20_[mf$gp_=="O"]=as.numeric(mf.v0$g_=="D")*(1-mdm.v0$fit);  
                  mf$A11_[mf$gp_=="O"]=0;
                                                   }
     } else  print("Warning: check the values of g_ for the subsample gp=O")
    

   ## (3) gp="I": 
   if (any(mf$gp_=="I")) {
   ## The missingness status at prior visit gp_ is essentially a dropout.  
   ##mf$gp_[mf$gp_=="I" & mf$g_=="D"]= "D"
   mf.v1 = mf[mf$gp_=="I",,drop=F]
   s.v1= s[mf$gp_=="I",,drop=F]
   missS  <- apply(s.v1, 2, FUN=function (u) any(is.na(u)))
   s.v1$WTs_=mf.v1$WTs_
   s.v1$g_=mf.v1$g_
      
   if (any(mf.v1$g_=="D")) { ## if the set of g values is "D", "O, D", "I, D", "O,I,D"
        stop("Data error: There should no dropout immediately after intermittent missingness. Change intermittent missingness to dropout ")
   } else if (! any(mf.v1$g_=="O")) {## if the set of g values is "I" only. This means all are dropouts.
      mf$obsprob_[mf$gp_=="I"]=0; mf$A10_[mf$gp_=="I"]=0; mf$A20_[mf$gp_=="I"]=0; mf$A11_[mf$gp_=="I"]=0;  
   } else if (all(mf.v1$g_=="O")) { ## If all data are observed in this subsample
         mf$obsprob_[mf$gp_=="I"]=1; mf$A10_[mf$gp_=="I"]=1; mf$A20_[mf$gp_=="I"]=0; mf$A11_[mf$gp_=="I"]=0;
   } else  if (any(mf.v1$g_!="O")) { ## the set of g values is " O, I"
       mdm.v1 =multinom(g_ ~ .-1- WTs_, data=droplevels(s.v1[,!missS]),  weights=WTs_, Hess=TRUE,maxit=500)
       ##  mdm.v1 =glm(g_ ~ .-1- WTs_, data=s.v1[,!missS], family=binomial(link=logit), weights=WTs_)
      mf$obsprob_[mf$gp_=="I"]=mf$A11_[mf$gp_=="I"]=1-mdm.v1$fit; 
      mf$A10_[mf$gp_=="I"]=mf$A20_[mf$gp_=="I"]=0
    } 
  }



   ## (4) gp="U": baseline observations
    ##mf$A10_[mf$gp_=="U"]=0; mf$A20_[mf$gp_=="U"]=0; mf$A11_[mf$gp_=="U"]=0
    ##mf$obsprob_[mf$gp_=="U"]=mf$A10_[mf$gp_=="U"]+ mf$A20_[mf$gp_=="U"]+ mf$A11_[mf$gp_=="U"];
    mf.vU = mf[mf$gp_=="U",,drop=F]
   s.vU= s[mf$gp_=="U",,drop=F]
   if (! any(mf.vU$g_=="O")) {            ## If the set of g values is all on "I", "D" or "I, D"  in this subsample.Then the probability of being observed is zero.
      mf$obsprob_[mf$gp_=="U"]=0; mf$A10_[mf$gp_=="U"]=0; mf$A20_[mf$gp_=="U"]=0; mf$A11_[mf$gp_=="U"]=0;      
    }   else if (all(mf.vU$g_=="O")) { ## If all data are observed in this subsample
     mf$obsprob_[mf$gp_=="U"]=1; mf$A10_[mf$gp_=="U"]=1; mf$A20_[mf$gp_=="U"]=0; mf$A11_[mf$gp_=="U"]=0;
      } else if (any(mf.vU$g_!="O")) {
        missS  <- apply(s.vU, 2, FUN=function (u) any(is.na(u)))
        s.vU$WTs_=mf.vU$WTs_
        s.vU$g_=mf.vU$g_
        if (length(unique(mf.vU$g_))>2) {     ## g includes "O, I, D" in this subsample.  
          mdm.vU =multinom(g_ ~ .-1- WTs_, data=s.vU[,!missS], weights=WTs_, Hess=TRUE,maxit=500)
          mdm.vU.fit=as.data.frame(mdm.vU$fit)
          mf$obsprob_[mf$gp_=="U"]=mdm.vU.fit$O; mf$A10_[mf$gp_=="U"]=as.numeric(mf.vU$g_=="I")-mdm.vU.fit$I; 
          mf$A20_[mf$gp_=="U"]=as.numeric(mf.vU$g_=="D")-mdm.vU.fit$D; mf$A11_[mf$gp_=="U"]=0; 
              }  else if (length(unique(mf.vU$g_))==2)  {  ## g includes "O, I", or "O,D" in this subsample.
                  mdm.vU =multinom(g_ ~ .-1- WTs_, data=droplevels(s.vU[,!missS]), weights=WTs_, Hess=TRUE,maxit=500)
                  ## mdm.v0 =glm(g_ ~ .-1- WTs_, data=s.v0[,!missS], weights=WTs_, family=binomial(link=logit))
                  mf$obsprob_[mf$gp_=="U"]=1-mdm.vU$fit; 
                  mf$A10_[mf$gp_=="U"]=as.numeric(mf.vU$g_=="I")*(1-mdm.vU$fit);
                  mf$A20_[mf$gp_=="U"]=as.numeric(mf.vU$g_=="D")*(1-mdm.vU$fit);  
                  mf$A11_[mf$gp_=="U"]=0;
                                                   }
     } ##else  print("Warning: check the values of g_ for the subsample gp=U")

    return(mf)

}

fun.trans2pos <- function(u)  {

  res<-exp(u)
  res
}


fun.trans2rho <- function(u)  {

  tol=10^308
  if (u==Inf)   u= tol
  if (u== -Inf) u= -tol
  rho<- 2*exp(u)/(1+exp(u))-1
  rho
}


fun.postrans <- function(u)  {

  if (u) stop("Argument has to be positive")
  res <- log(u)
  res
}


fun.rhotrans <- function(rho)  {

  if (abs(rho) >1) stop("correlation coefficient must be between 1 and -1")
  tol= 1e-10
  u = (1+rho)/2
  if (u==0) u= u + tol
  if (u==1) u= 1- tol
  res<- log(u/(1-u))
  res
}



