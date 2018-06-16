

#' Function for ISNI computation when the outcome follows LMM.
#'
#' Calculate the ISNI when the regression outcome is subject to missingness and follows linear mixed-effects models (LMMs)
#' @param formula an object of class "Formula": a symbolic description of the models to be fitted for the outcome and missingness status variable.  
#'                The details of model specification are given under "Details".  
#' @param data  the name of data frame containing all the variables in the model and all the observations including those intended to be collected 
#'              but became missing.
#' @param random an object of class "formula": an one-sided linear formula description of the random-effects part of the model to be fitted 
#'               for the outcome
#' @param id the name of the level-2 clustering variable. 
## @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process for the outcome model and the missingness mechanism model.
#' @param weights frequency weights to be assigned to each \code{id}. when supplied, indicates differential weights are used; otherwise each \code{id} is weighted equally.  
#' @param predprobobs Null if using buil-in multinomial transitional logistic model to obtain predicted probabilities of being observed;
#'                    otherwise user supply the name of the variable in \code{data} that gives these probabilities for all the observations in the data.
#' @param misni  FALSE if using the default approach to computing ISNI with a scalar nonignorability parameter; 
#'               TRUE when computing ISNI with multiple nonignorability parameters.
#' @details The ISNI analysis is based on a joint selection model and requires specifying two model equations: the complete-data model and the missing data mechanism model.
#'        To specify the variables in the models that are required for computing the ISNI measures, we make use of the  \code{R} package "Formula"  designed for handling model
#'        equations with multiple responses    and multiple sets of predictors . At a minimum, the user should supply a single-equation 
#'         in the typical form:  \code{response} ~ \code{Xterms} where \code{response} is the (numeric or factor) vector for the outcome of interest and \code{Xterms} 
#'        is a series of terms, separated by + operators, which specify a linear predictor for response. With the signle-equation specification, the \code{isniglm} function 
#'        will by default use the utility function \code{definemissingstatus} provided in the package to generate the 
#'        missingness status variables at the current and prior visits and then use \code{Xterms} as the observed missingness predictors for fitting a first-order transitional missing data model. 
#'        It is important to sort within-\code{id} observations  by time so that the missingness status variables can be defined correctly in this default setting.  The \code{isnimgm} then computes the MAR estimates and conducts ISNI computation
#'        to evaluate the rate of change of model estimates in the neighborhood of the MAR model where the missingness probability is allowed to depend on the unobserved value 
#'        of \code{response},   even after  conditioning on the other  missingness predictors. 
#'
#'        The above single-equation formula specification uses the same set of predictors for the outcome model and the missingness mechanism model for computing ISNI. 
#'        To use different sets of predictors, one can explicitly specifiy a two-equation formula as: \code{response} | \code{miss + missprior} ~ \code{Xterms} |  \code{Sterms},
#'        which specifies the formula  for the complete-data model as \code{response} ~ \code{Xterms} and that for the missing data mechanism model as \code{miss + missprior} ~ \code{Sterms}, 
#'        where \code{Xterms} and the observed predictors for missingness \code{Sterms} can be different, \code{miss} and \code{missprior} are the variable names in \code{data} denoting the missingness status at the 
#'        current and prior visits, respectively.  
#'
#'        For \code{isnilmm}, \code{response} ~ \code{Xterms} specfied the fixed-effect part of the linear mixed-effects model for the outcome. The random-effect part of the model is
#'        specified as a one-sided formula via the argument \code{random}. 
#' @name isnilmm
#' @aliases isnilmm
#' @export
#' @examples
#'
#' data(qolef)
## qolef$t12<-qolef$t1*qolef$group
## qolef$t32<-qolef$t3*qolef$group
## qolef$t62<-qolef$t6*qolef$group
#' ymodel= y | g+ gp~   as.factor(time)*group+perf+sever
## gmodel=factor(g)~ group+factor(time)+yp+perf+sever
## ymran=~1
#'
#' ##Random intercept model
#' result=isnilmm(ymodel, random=~1, id=id,  data=qolef)
#' summary(result)
#' 
isnilmm = function(formula, data, random, id, weights, subset, predprobobs, misni=FALSE) {
  
    ## (1) process the call and set up model frame.
    cl <- match.call()
    if (missing(data))  data <- environment(formula)
    if (missing(id)) stop("Users need to supply the name of the level-2 clusting variable in the data via the argument id.")

    mf <- match.call(expand.dots = FALSE)    
    m <- match(c("formula", "data",  "subset", "weights", "id", "predprobobs"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    f <- Formula(formula)
    if (any(length(f)>2)) stop("Cannot have more than two models") 
    mf$formula <- as.Formula(formula, random)
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
    if (is.null(WTs))
        WTs <- rep(1, nrow(mf))
   
    isni_WTs <- isni_id <- isni_outcome_ <- NULL 
    mf= cbind(mf, mfvar[ !(names(mfvar) %in% names(mf))],  isni_id=ID, isni_WTs=WTs)  
    if (! missing(predprobobs))   mf$fitprobs_=  model.extract(mf, "predprobobs")  
   

   ##(2)  Extract responses and predictors in the outcome model from the model frame
   ymodel=formula(f, lhs=1, rhs=1)
   x<- as.matrix(model.matrix(f, data=mf, rhs=1))
   y<- model.part(f,data=mf, lhs=1)[,1]  ## will also be a vector
   if (nrow(x)<1)  print("No predictor variables specified for the outcome")
   z<- model.matrix(random,data=mf)
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

  ## Drop all observations with missing values in predictor matrix X or Z to avoid NAs in ISNI computation. 
    missx<- apply(x, 2, FUN=function (u) any(is.na(u)))
    if (any(missx)) cat(paste("\n Note: There are missing values in fixed-effect outcome predictors-- ", paste(dimnames(x)[[2]][missx], collapse=" "), ". Observations with missing values in these predictors will be dropped out from computing ISNI, 
       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", sep=""))
    missz <- apply(z, 1, FUN=function (u) any(is.na(u)))
    if (any(missz)) cat(paste("\n Note: There are missing values in random-effect outcome predictors-- ", paste(names(x)[missx], collapse=" "), ". Such observations will be dropped out from computing ISNI, 
       which may reduce ISNI values if the outcome is missing concurrently. Alternatively one can impute these missing predictors values and re-compute ISNI. ", sep=""))
    missX  <- apply(x, 1, FUN=function (u) any(is.na(u)))
    missZ <- apply(z, 1, FUN=function (u) any(is.na(u)))
    WTs=WTs[(!missX) & (!missZ)]
    mf=mf[(!missX) & (!missZ),,drop=F] 
    options(na.action=na.pass) 
    x<- as.matrix(model.matrix(f, data=mf, rhs=1))
    y<- model.part(f,data=mf, lhs=1)[,1]  ## will also be a vector 
    z<- model.matrix(random,data=mf)
    xomit <-  as.matrix(model.matrix(f, data=mf[!is.na(y), ], rhs=1))
    zomit <- model.matrix(random,data=mf[!is.na(y), ])
    if (ncol(x) != ncol(xomit)) {
        cat(paste("\n All variable names in fixed-effects design matrix of ISNI analysis including observations with missing outcomes: \n", paste (dimnames(x)[[2]], collapse=" ")))
        cat(paste("\n All variable names in fixed-effects design matrix of the MAR model excluding observations with missing outcomes:  \n", paste (dimnames(xomit)[[2]], collapse=" ")))
        stop("\n The fixed-effects design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function
               in model formula.  ")
     }    
    if (ncol(z) != ncol(zomit)) {
        cat(paste("\n All variable names in random-effects design matrix of ISNI analysis including observations with missing outcomes: \n", paste (dimnames(z)[[2]], collapse=" ")))
        cat(paste("\n All variable names in random-effects design matrix of the MAR model excluding observations with missing outcomes:  \n", paste (dimnames(zomit)[[2]], collapse=" ")))
        stop("\n The random-effects design matrix for the MAR model and ISNI analysis are different as shown above. Please modify your model formula specification, e.g. avoiding using as.factor function
               in model formula.  ")
     }   
    options(na.action=na.omit)
    maxT = max(table(mf$isni_id))

   ## Fit the lmm with the observed data
   if (length(random)==3) random = as.formula(paste(random[2], random[1],  random[3], " | isni_id ", sep=""))
   if (length(random)==2) random = as.formula(paste(random[1],  random[2], " | isni_id", sep=""))
   iglmm=lme(ymodel, data=mf, random =random, weights= ~1/isni_WTs, method="ML",na.action=na.omit)

   ##  fit a missing data model to obtain fitted probabilities for being observed.
   if (missing(predprobobs)) {
    mf.mdm= tmdm(gmodel, data=mf, weights=isni_WTs)
    mf$fitprobs_=mf.mdm$obsprob_ 
    mf$A10_= mf.mdm$A10_;  mf$A20_= mf.mdm$A20_;  mf$A11_= mf.mdm$A11_
   } else {
      if (misni==T) stop("predprobobs needs to to a matrix to obtain ISNI vector for multiple nonignorability parameter")
   }

  b<- fixef(iglmm)
  nb = length(b)
  nv=ncol(z)
  
  vcovm=VarCorr(iglmm)
  sigma=as.numeric(vcovm[,2])
  sigmav=sigma[-length(sigma)]
  sigmae=sigma[length(sigma)]  
  rhom=VarCorr(iglmm)[-nrow(vcovm),c(-1,-2)]
  corr=as.numeric(rhom[lower.tri(rhom)])
  
  d=c(sigmav,corr,sigmae)
  npar= nb+length(d)
 

  uid = unique(mf$isni_id)
  
  nabla12=matrix(0, nrow=npar, ncol=3)
  nabla11=matrix(0, nrow=npar, ncol=npar)
  for (i in 1:length(uid)) {    
    xi= as.matrix(x[mf$isni_id==uid[i],,drop=F])
    zi= as.matrix(z[mf$isni_id==uid[i],,drop=F])
    yi= y[mf$isni_id==uid[i]]
    nabla11=nabla11+ fun.mixsubi(yi=yi,desi=xi,zi=zi,b=b,d=d,gfiti=gfiti, case=1)
    gfiti= NULL; Afiti=NULL
    if (!missing(predprobobs))  gfiti= mf$fitprobs_[mf$id==uid[i]]
    else Afiti= cbind(mf$A10_[mf$isni_id==uid[i]], mf$A20_[mf$isni_id==uid[i]], mf$A11_[mf$isni_id==uid[i]])
    if (any(is.na(yi))) nabla12 = nabla12+ fun.mixsubi(yi=yi,desi=xi,zi=zi,b=b,d=d, gfiti=gfiti, Afiti=Afiti, case=2)
  }
  isni=-solve(nabla11)%*%nabla12
 
  se=sqrt(diag(solve(-nabla11)))
  bD= c(b, d)
  if (length(d)==2) names(bD)=c(names(b),"sigmav","sigmae")
  else {
    namesv=paste("sigmav", 1:nv, sep="")
    rhon=c()
    for (i in 1:(nv-1)){
      for (j in (i+1):nv){
       rhonij=i*10+j
       rhon=c(rhon,rhonij)
      }
    }
    namesr=paste("rho",rhon, sep="")
    names(bD)=c(names(b),namesv,namesr,"sigmae")
  }
  sdy <- sd(y,na.rm=T)
  isnivec=isni
  if (misni==FALSE) {
       isni=apply(isni,1, sum); senstran<-abs((sdy*se)/isni)
   } else
   {
       isni=apply(abs(isni),1, sum); senstran<-abs((sdy*se)/isni)
   }
  res=list(coef=bD,se=se,isni=c(isni),c=c(senstran), call=cl, isnivec=isnivec, misni=misni, 
        logLik=summary(iglmm)$logLik, aic=summary(iglmm)$AIC, bic=summary(iglmm)$BIC)
  class(res) = c(res$class, "isnilmm")
  res
}



#' Function to print out a summary of isnilmm  object in a matrix form.
#'
#' @param object the isnilmm object obtained from the isnilmm function
#' @param digits the number of significant digits to use when printing
#' @param ... additional arguements
#' @export summary.isnilmm
#' @export
#' @name summary.isnilmm
#' @aliases summary.isnilmm
summary.isnilmm<-function(object, digits = max(3, getOption("digits") - 2),...) {
  
  if (class(object) != "isnilmm")  stop('Invalid object class')  
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

   #' Function to print  the isnilmm object.
    #'
    #' \code{print} method for class isnilmm
    #' @param x the isnilmm object obtained from the isnilmm function
    #' @param digits the number of significant digits to use when printing
    #' @param ... further arguments passed to or from other methods.
    #'
    #' @return The function print.isnilmm prints the model call,  isni and c statistics from the isnilmm object.
    #' @name print.isnilmm
    #' @aliases print.isnilmm
    #' @export print.isnilmm
    #' @export
    print.isnilmm<-function(x, digits = max(3, getOption("digits") - 2), ...) {
	
     if (class(x) != "isnilmm")  stop('Invalid object class')
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

## Internal Function to generate and compute derivatives of  the var-cov matrix of the marginal error distribution in the form of  zDz^T+ \sigma_e*I. 
##
## Calculate the compound symmetry matrix and its first and second derivatives wrt the vari-cov matrix
## @param zi design matrix for random effects
## @param ni dimension of the square matrix
## @param d the vector of unique parameters in the variance-covariance matrix for the error term in the LMM model for Y
## @param m the location of the corresponding parameter in the d vector that the matrix derivative with respect to
## @param n used in second derivatives, the location of the second parameter in the d vector that the matrix derivative with respect to
## @param case   0: return the vari-cov matrix of the random effects; 
##               1: return first derivatives of the vari-cov matrix wrt sigmav; 
##               2: return second derivatives of the vari-cov matrix wrt sigmav if only one random effect, 
##                  return first derivatives of the vari-cov matrix wrt rho if multiple random effects;
##               3: return second derivatives of the vari-cov matrix wrt sigmav;
##               4: return second derivatives of the vari-cov matrix wrt sigmav and rho.
## @aliases fun.dev

fun.dev=function(zi,ni,d,m,n, case=1){
  
  nv=ncol(zi)
  
  if (nv==1){
    sigmav=d[1] 
    sigmae=d[2]
    if (case==0) res=(sigmav^2)*(zi%*%t(zi))+diag(sigmae^2,nrow=ni)
    else if (case==1) res=(2*sigmav)*(zi%*%t(zi))
    else if (case==2) res=2*(zi%*%t(zi))

  }
  else{
    
    sigmae= d[length(d)]
    sigmav=as.vector(d[1:nv])
    rho=as.vector(d[(nv+1):(length(d)-1)])
    if (length(rho)==1) {
      corrm=matrix(c(1,rho,rho,1),2,2)
    }
    else {
      c=matrix(0,length(rho),length(rho))
      c[lower.tri(c, diag=FALSE)]=rho
      corrm=c+t(c)+diag(1,length(rho))
    }
    
    D=(sigmav%*%t(sigmav))*corrm  
    
    if (case==0) res=diag(sigmae^2,nrow=ni)+zi%*%D%*%t(zi)
 
    else  res=zi%*%fun.devD(d,sigmav,rho, D, corrm, m,n, case=case) %*%t(zi)
  }
  
  res
}

## Internal Function for generate first and second derivatives wrt D
##
## Calculate the first and second derivatives wrt D.
## @param d the vector of unique parameters in the variance-covariance matrix for the error term in the LMM model for Y
## @param sigmav random effects SD vector
## @param rho random effects correlation vector
## @param D variance-covariance matrix for random effects
## @param corrm correlation matrix for random effects
## @param m the location of the corresponding parameter in the d vector that the D matrix derivative with respect to
## @param n used in second derivatives, the location of the second parameter in the d vector that the matrix derivative with respect to
## @param case   1: return first derivatives of the D matrix wrt sigmav; 2: return first derivatives of the D matrix wrt rho;
##               3: return second derivatives of the D matrix wrt sigmav; 
##               4: return second derivatives of the D matrix wrt sigmav and rho.
## @aliases fun.devD
## @export
fun.devD=function(d,sigmav,rho, D, corrm, m,n, case=1) {

  ## output
  ## case=1 --- return results for first derivatives of the vari-cov matrix wrt sigmav
  ## case=2 --- return results for first derivatives of the vari-cov matrix wrt rho 
  ## case=3 --- return results for second derivatives of the vari-cov matrix wrt sigmav
  ## case=4 --- return results for second derivatives of the vari-cov matrix wrt sigmav and rho
  
  if (case==1) {
    res=matrix(0,length(sigmav),length(sigmav))
    res[m,]=D[m,]/d[m]
    res[,m]=D[,m]/d[m]
    res[m,m]=2*res[m,m]
    res
  }
  
  else if (case==2) {
    res=sigmav%*%t(sigmav)
    res[!corrm==d[m]]=0
    res
  }
  
  ## derivative wrt to sigmav
  else if (case==3) {
    res=matrix(0,length(sigmav),length(sigmav))
    if (m==n) res[m,n]=2
    else {
      res[m,n]=corrm[m,n]
      res[n,m]=corrm[n,m]
    }
    res
  }
  
  ## derivative wrt to sigmav and rho
  else if (case==4) {
    res=sigmav%*%t(sigmav)
    res[!corrm==d[m]]=0
    res=res/d[n]
    res
  }
  
  res
  
}

## Internal Function for various subject-level computation required for ISNILMM.
##
## Calculate subject-level quantities when the regression outcome is subject to missingness and 
## follows linear mixed-effects  Models. 
## @param yi vector of the response for the ith subject
## @param desi design matrix of the fixed effect for the ith subject
## @param zi design matrix of the random effect for the ith subject
## @param b  the mean parameter vector beta
## @param d  the vector of unique parameters in the variance-covariance matrix for the error term in the LMM model for Y
## @param gfiti  vector of predicted probabilities of being observed for all the observations from the ith subject
## @param Afiti matrix of 3 columns of predicted transitional probabilities for the missing observations from the ith subject. 
## @param case   1: calculated nabla11_i; 2: calculate nabla12_i
## @aliases fun.mixsubi
## @export
fun.mixsubi <- function ( yi, desi, zi, b, d, gfiti=NULL, Afiti=NULL, case=1) {
  
  
  ## output
  ## res -- if case=1, res=hessian matrix
  ##        if case=2, res=\nabla_12 for subject i.

  
  
  obsi  = !is.na(yi)
  desiobs = as.matrix(desi[obsi,,drop=F])
  ziobs = as.matrix(zi[obsi,,drop=F])
  yiobs = as.vector(yi[obsi])
  niobs = length(yiobs)
  ni=length(yi)
  muiobs = as.vector(desiobs %*% as.matrix(b))
  riobs= yiobs - muiobs
  nb=length(b)
  nd=length(d)
  npar= nb + nd
  sigmae=d[nd]
  nv=ncol(zi)
  
  covi=fun.dev(zi,ni,d,m,n, case=0)
  coviobs= covi[obsi,obsi]
  icoviobs= solve(coviobs)

  if (case==1) {
    nabla11= matrix(0, npar, npar)
    nabla11[1:nb, 1:nb]=-t(desiobs)%*%icoviobs%*%desiobs
   ### 
   if (nv==1){
     dev.sigmav=fun.dev(ziobs,niobs,d, m,n, case=1)
     dev.sigmav2=fun.dev(ziobs,niobs,d,n, n,case=2)
     nabla11[nb+1, 1:nb]=-t(as.matrix(riobs))%*%icoviobs%*%dev.sigmav%*%icoviobs%*%desiobs
      nabla11[1:nb, nb+1]=t(nabla11[nb+1, 1:nb])  
      nabla11[nb+1, nb+1]=0.5*t(as.matrix(riobs))%*%(-2*icoviobs%*%dev.sigmav%*%icoviobs%*%dev.sigmav%*%icoviobs+
                            icoviobs%*%dev.sigmav2%*%icoviobs)%*%as.matrix(riobs)-
                            0.5*matrix.trace(-icoviobs%*%dev.sigmav%*%icoviobs%*%dev.sigmav+icoviobs%*%dev.sigmav2)
      nabla11[npar, nb+1]=nabla11[nb+1, npar]=sigmae*t(as.matrix(riobs))%*%(-icoviobs%*%dev.sigmav%*%
                                               icoviobs%*%icoviobs-icoviobs%*%icoviobs%*%dev.sigmav%*%icoviobs)%*%
                                               as.matrix(riobs)-sigmae*matrix.trace(-icoviobs%*%dev.sigmav%*%icoviobs) 
   }


    
  ###  
    else {
    #sigmav part
    for (m in 1:nv){
      dev.sigmav=fun.dev(ziobs,niobs,d,m,n, case=1)
      nabla11[nb+m, 1:nb]=-t(as.matrix(riobs))%*%icoviobs%*%dev.sigmav%*%icoviobs%*%desiobs
      nabla11[1:nb, nb+m]=t(nabla11[nb+m, 1:nb])
      for (n in 1:nv){
        dev.sigmav2=fun.dev(ziobs,niobs,d,n, n,case=1)
        dev2.sigmav=fun.dev(ziobs,niobs,d,m, n,case=3) 
        nabla11[nb+m, nb+n]=0.5*t(as.matrix(riobs))%*%(-icoviobs%*%dev.sigmav%*%icoviobs%*%dev.sigmav2%*%icoviobs-icoviobs%*%
                            dev.sigmav2%*%icoviobs%*%dev.sigmav%*%icoviobs+icoviobs%*%dev2.sigmav%*%icoviobs)%*%as.matrix(riobs)-
                            0.5*matrix.trace(-icoviobs%*%dev.sigmav%*%icoviobs%*%dev.sigmav2+icoviobs%*%dev2.sigmav)
        #nabla11[nb+n, nb+m]=t(nabla11[nb+m, nb+n])
      }
      nabla11[nb+m, npar]=t(as.matrix(riobs))%*%(-sigmae*icoviobs%*%dev.sigmav%*%icoviobs%*%icoviobs-
                          sigmae*icoviobs%*%icoviobs%*%dev.sigmav%*%icoviobs)%*%as.matrix(riobs)-
                          matrix.trace(-sigmae*icoviobs%*%dev.sigmav%*%icoviobs)
      nabla11[npar, nb+m]=t(nabla11[nb+m, npar])
    }  
    
    for (m in (nv+1):(length(d)-1))
    {
      dev.rho=fun.dev(ziobs,niobs,d,m, n, case=2)
      nabla11[nb+m, 1:nb]=-t(as.matrix(riobs))%*%icoviobs%*%dev.rho%*%icoviobs%*%desiobs
      nabla11[1:nb, nb+m]=t(nabla11[nb+m, 1:nb])
      
      for (n in 1:nv){
        dev.sigmav=fun.dev(ziobs,niobs,d,n, n,case=1)
        dev2.svrho=fun.dev(ziobs,niobs,d, m, n,case=4) 
        nabla11[nb+m, nb+n]=0.5*t(as.matrix(riobs))%*%(-icoviobs%*%dev.sigmav%*%icoviobs%*%dev.rho%*%icoviobs-icoviobs%*%dev.rho%*%
                            icoviobs%*%dev.sigmav%*%icoviobs+icoviobs%*%dev2.svrho%*%icoviobs)%*%as.matrix(riobs)-
                            0.5*matrix.trace(-icoviobs%*%dev.rho%*%icoviobs%*%dev.sigmav+icoviobs%*%dev2.svrho)
        nabla11[nb+n, nb+m]=t(nabla11[nb+m, nb+n])
      } 
      for (n in (nv+1):(length(d)-1)){
        dev.rho2=fun.dev(ziobs,niobs,d, n, n, case=2)
        dev2.svrho=fun.dev(ziobs,niobs,d,m, n,case=4) 
        nabla11[nb+m, nb+n]=0.5*t(as.matrix(riobs))%*%(-icoviobs%*%dev.rho%*%icoviobs%*%dev.rho2%*%icoviobs-icoviobs%*%dev.rho2%*%icoviobs%*%
                            dev.rho%*%icoviobs)%*%as.matrix(riobs)-0.5*matrix.trace(-icoviobs%*%dev.rho%*%icoviobs%*%dev.rho2)
        #nabla11[nb+n, nb+m]=t(nabla11[nb+m, nb+n])
      }
      nabla11[nb+m, npar]=t(as.matrix(riobs))%*%(-sigmae*icoviobs%*%dev.rho%*%icoviobs%*%icoviobs-
                          sigmae*icoviobs%*%icoviobs%*%dev.rho%*%icoviobs)%*%as.matrix(riobs)-
                         matrix.trace(-sigmae*icoviobs%*%dev.rho%*%icoviobs)
      nabla11[npar, nb+m]=t(nabla11[nb+m, npar])
      
     } 
    } 
    
    ##sigmae part
    nabla11[npar, 1:nb]=-2*sigmae*t(as.matrix(riobs))%*%icoviobs%*%icoviobs%*%desiobs
    nabla11[1:nb, npar]=t(nabla11[npar, 1:nb])
    
    nabla11[npar, npar]=t(as.matrix(riobs))%*%(-4*sigmae*sigmae*icoviobs%*%icoviobs%*%icoviobs+icoviobs%*%icoviobs)%*%
        as.matrix(riobs)-matrix.trace(-2*sigmae*sigmae*icoviobs%*%icoviobs+icoviobs) 
    
    return(nabla11)
  }
  
  ## case=2, calculate the \nabla_12 for subject i.
  if (case==2) {
    if (any(is.na(yi))){
      
      nabla12= matrix(0, npar,3)
      misi  = is.na(yi)
      
     #  adjmuximis=as.vector(covxi[misi,obsi,drop=F] %*% icovxiobs %*% as.matrix(rxiobs))
     # muximis=as.vector(xdesi %*% as.matrix(bx))[misi]+adjmuximis
      
      desimis = as.matrix(desi[misi,,drop=F])
     # desimis[,xmispos]=muximis
      yimis = as.vector(yi[misi])
      ##fitp0i = gfiti[misi]
       Afit0i=matrix(0, nrow=sum(misi==T), ncol=3)
       if (is.null(gfiti))  Afit0i =Afiti[misi, ,drop=F]
        else Afit0i[,1]= gfiti[misi]

      nabla12[1:nb,]= (t(desimis)-t(desiobs)%*% icoviobs %*%covi[obsi,misi, drop=F])%*% Afit0i ##as.matrix(fitp0i)
      
      
      
      if (nv==1){
        d.sigmav=fun.dev(zi,ni,d,m,n, case=1)
        nabla12[nb+1,]=t(d.sigmav[misi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs)- covi[misi, obsi,drop=F]%*%icoviobs%*%
                            d.sigmav[obsi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs))%*% Afit0i ## as.matrix(fitp0i)
        }
        
       else {
          for (m in 1:nv){
          d.sigmav=fun.dev(zi,ni,d,m,n, case=1)
          nabla12[nb+m, ]=t(d.sigmav[misi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs)- covi[misi, obsi,drop=F]%*%icoviobs%*%
                        d.sigmav[obsi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs))%*%Afit0i ## as.matrix(fitp0i)
        }
        for (m in (nv+1):(length(d)-1)){
          d.rho=fun.dev(zi,ni,d,m,n, case=2)
          nabla12[nb+m,]=t(d.rho[misi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs)- covi[misi, obsi,drop=F]%*%icoviobs%*%
                        d.rho[obsi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs))%*% Afit0i ##as.matrix(fitp0i)
        }  
       }
        
       
        
    
        
        nabla12[npar, ]=t(diag(2*sigmae,ni)[misi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs)-covi[misi, obsi,drop=F]%*%icoviobs%*%
                      diag(2*sigmae,ni)[obsi,obsi,drop=F]%*%icoviobs%*%as.matrix(riobs))%*% Afit0i ##as.matrix(fitp0i)  
        
      }
      return(nabla12)
    }## end if case=2
    
  } 
  


