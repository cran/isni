##install.packages("nlme")
##install.packages("mvtnorm")
##install.packages("nnet")
##install.packages("numDeriv")

library(nlme)
library(mvtnorm)
library(nnet)
library(numDeriv)

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
#'  \item time: time in months since randomization
#'  \item group: placebo (0) or flutamide (1)
#'  \item perf: baseline performance score
#'  \item sever: baseline disease severity
#'  \item t1: indicator variable for month 1
#'  \item t3: indicator variable for month 3
#'  \item t6: indicator variable for month 6
#'  \item yp: most recently observed  prior outcome
#'  \item basey: EF at baseline
#'  \item g: missingness status ("O"=observed, "D"=dropout, "I"=intermittent missingness)
#'  \item gp: missingness status in the prior visit ("O"=observed, "D"=dropout, "I"=intermittent missingness, "U"=undefined)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name qolef
#' @usage data(qolef)
#' @format A data frame with 2860 rows and 13 variables
NULL

#' Function for ISNI computation when the outcome follows GLS.
#'
#' Calculate the ISNI when the regression outcome is subject to missingness and follows generalized linear models (GLMs)
#' @param ymodel an object of class "formula": a symbolic description of the model to be fitted for the outcome
#' @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
#' @param ycorr  the form of within-subject correlation structure.
#' @param predprobobs Null if using buil-in multinomial transitional logistic model to obtain predicted probabilities of being observed;
#'                    otherwise user supply a vector of these probabilities for all the observations in alldata.
#' @param misni  FALSE if using the default approach to computing ISNI with a scalar nonignorability parameter; 
#'               TRUE when computing ISNI with multiple nonignorability parameters.
#' @param alldata  the name of data frame containing all the variables in the model and all the observations including those intended to be collected 
#'              but became missing.
#' @name isnigls
#' @aliases isnigls
#' @import  matrixcalc mvtnorm nlme nnet numDeriv stats
#' @export
#' @examples
#' ymodel= y~   t1+t3+t6 + group  + t12 + t32+t62 + perf + sever
#' gmodel= as.factor(g)~ t3+t6+group+yp+perf+sever
#' qolef$t12<-qolef$t1*qolef$group
#' qolef$t32<-qolef$t3*qolef$group
#' qolef$t62<-qolef$t6*qolef$group
#'
#' qolef.isni=isnigls(ymode=ymodel, gmodel=gmodel, alldata=qolef)
#' summary(qolef.isni)
#'
isnigls = function(ymodel, gmodel, ycorr='CS', predprobobs=NULL, misni=FALSE, alldata) {
  ## Calculate ISNI in a generalized linear model with outcome y, a vector of
  ## predictors x, complete-data indicator g and the distribution family.
  ## This formulation only allows missing data in the outcome vector y.

  ymodel.data<-model.frame(ymodel,data=alldata,na.action=NULL)
  x<- as.matrix(model.matrix(ymodel, data=ymodel.data))
  y<- as.vector(model.extract(ymodel.data, "response"))
  sdy= sd(y,na.rm=T)
  maxT = max(table(alldata$id))

  ##gmodel.data<-model.frame(gmodel,data=alldata,na.action=NULL)
  ##s<- as.matrix(model.matrix(gmodel, data=gmodel.data))
  ##g<- as.vector(model.extract(gmodel.data, "response"))
  ##names(gmodel.data)[1]="g"

  ## fit the GLS with the observed data.
  if (ycorr=="CS")  iggls= gls(ymodel, data=alldata,correlation = corCompSymm(form =  ~ 1 | id), na.action=na.exclude)
  if (ycorr=="AR1") iggls= gls(ymodel, data=alldata,correlation = corAR1(form =  ~ 1 | id), na.action=na.exclude)
  if (ycorr=="UN")  iggls= gls(ymodel, data=alldata,correlation = corSymm(form =  ~ 1 | id), na.action=na.exclude)



  ##  fit a missing data model to obtain fitted probabilities for being observed.
  if (is.null(predprobobs)) {
    if (!("g" %in% names(alldata)))  stop("A variable called 'g' for missing data status should be supplied in the data")
    if (!("gp" %in% names(alldata)))  stop("A variable called 'gp' for missing data status for the prior visit should be supplied in the data")
    alldata.mdm= fun.gfit(alldata, gmodel)
    alldata$fitprobs=alldata.mdm$obsprob 
    alldata$A10= alldata.mdm$A10;  alldata$A20= alldata.mdm$A20;  alldata$A11= alldata.mdm$A11
    ##Afit=cbind(alldata$A10, alldata$A20, alldata$A11)
  } else {
    if (!is.null(predprobobs) & misni==T) stop("predprobobs needs to to a matrix to obtain ISNI vector for multiple nonignorability parameter")
    if (!is.vector(predprobobs)) stop("predprobobs should be a vector")
    if (nrow(alldata) != length(predprobobs)) stop ("The length of predprobobs should equal the number of observations in alldata")
    alldata$fitprobs=predprobobs
  }

  if (ycorr=="CS")  D=list(sigma=iggls$sigma, rho=intervals(iggls)$corStruct[,"est."])
  if (ycorr=="AR1") D=list(sigma=iggls$sigma, rho=intervals(iggls)$corStruct[,"est."])
  if (ycorr=="UN")  D=list(sigma=iggls$sigma, rho=intervals(iggls)$corStruct[,"est."])

  b<- iggls$coef
  nb = length(b)
  nD=length(D$sigma)+length(D$rho)
  npar= nb + nD

  uid = unique(alldata$id)

  nabla12=matrix(0, nrow=npar, ncol=3)

  ## compute nabla11
  nabla11=matrix(0, nrow=npar, ncol=npar)
  for (i in 1:length(uid)) {
    xi= as.matrix(x[alldata$id==uid[i],,drop=F])
    yi= y[alldata$id==uid[i]]
    nabla11=nabla11+ fun.glssubi(yi=yi,xi=xi,maxT=maxT, b=b,D=D, ycorr=ycorr, transform=FALSE, case=1)
    gfiti= NULL; Afiti=NULL
    if (!is.null(predprobobs))  gfiti= alldata$fitprobs[alldata$id==uid[i]]
    else Afiti= cbind(alldata$A10[alldata$id==uid[i]], alldata$A20[alldata$id==uid[i]], alldata$A11[alldata$id==uid[i]])
    if (any(is.na(yi))) nabla12 = nabla12 + fun.glssubi(yi=yi,xi=xi,maxT=maxT, b=b,D=D, ycorr=ycorr, transform=FALSE, gfiti=gfiti,Afiti=Afiti, case=2)
  }
  ##nabla12=apply(nabla12,2,sum)
  isni=-solve(nabla11)%*%nabla12


  se=sqrt(diag(solve(-nabla11)))
  bD= c(b,unlist(D))
  if (ycorr=="CS" | ycorr=="AR1" ) {
  names(bD)=c(names(b), names(D))
                                   } else
  if (ycorr=="UN" )                {
  names(bD)=c(names(b), 'sigma', names(D[[2]]))
                                   }
  if (misni==FALSE) {
       isni=apply(isni,1, sum); senstran<-abs((sdy*se)/isni)
   } else
   {
       isni=apply(abs(isni),1, sum); senstran<-abs((sdy*se)/isni)
   }
  res=list(coef=bD,se=se,isni=c(isni),c=c(senstran))
  class(res) = c(res$class, "isnigls")
  res
}

## Function to print out ISNIGLS, the index of sensitivity to
## nonignorability, in a generalized least sqaure model.
##
#' Function to print out a summary of isnigls  object in a matrix form.
#'
#' @param object the isniglm object obtained from the isnigls function
#' @param ... additional arguements
#' @export
#' @name summary.isnigls
#' @aliases summary.isnigls

summary.isnigls<-function(object, ...) {

  if (class(object) != "isnigls")  stop('Invalid object class')
  ## Name the columns
  isniname<-c('Est','SE','ISNI','c')

  ## Set up matrix to hold result
  res1<-matrix(0,length(object$coef),length(isniname))
  dimnames(res1)<-list(names(object$coef),isniname)

  for (i in 1:length(object$coef))
    res1[i,]<- c(object$coef[i],object$se[i],object$isni[i],object$c[i])
  print(res1)
}

#' Internal Function for various subject-level computation required for ISNIGLS.
#'
#' Calculate subject-level quantities when the regression outcome is subject to missingness and follows generalized least sqaures models (GLS)
#' @param yi vector of the response for the ith subject
#' @param xi matrix of the covariates for the ith subject
#' @param maxT maximum number of visits
#' @param b  the mean parameter vector beta
#' @param D  the vector of unique parameters in the variance-covariance matrix for the error term in the GLS model for Y
#' @param ycorr  the form of within-subject correlation structure in the GLS model for Y
#' @param transform logical indicating wether or not the parameter in D is transformed.
#' @param gfiti  vector of predicted probabilities of being observed for all the observations from the ith subject
#' @param Afiti matrix of 3 columns of predicted transitional probabilities for the missing observations from the ith subject. 
#' @param case   1: calculated nabla11_i; 2: calculate nabla12_i
#' @aliases fun.glssubi
#' @export
fun.glssubi <- function ( yi, xi, maxT=maxT, b, D, ycorr,transform=FALSE, gfiti=NULL, Afiti=NULL,  case=1) {


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
  if (ycorr == "CS") {
    covyi= fun.csmat(sigma=sigma, rho=rho, obsi=length(yi),transform=transform, case=1)
  }

  if (ycorr == "AR1") {
    covyi= fun.ar1mat(sigma=sigma, rho=rho, obsi=length(yi),transform=transform, case=1)
  }

  if (ycorr == "UN") {
    col=matrix(0, nrow=maxT, ncol=maxT)
    for (i in 1:(maxT-1)){
                          col[(i+1):maxT,i]=rho[((i-1)*maxT-i*(i-1)/2+1) : (i*maxT-i*(i+1)/2)]
                         }
    rho2=col+t(col)+diag(maxT)

    covyi= fun.generalmat(sigma=sigma, rho=rho2, obsi=length(yi),transform=transform, case=1)
  }

    covyiobs= covyi[obsi,obsi]
    icovyiobs= solve(covyiobs)

 
  ## case=1, compute the nabla11 for subject i
  if (case==1) {
    nabla11= matrix(0, npar, npar)
    nabla11[1:nb, 1:nb]=-t(xiobs)%*%icovyiobs%*%xiobs
  if (ycorr == "CS") {
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

    for (j in 1:nD) nabla11[1:nb,(nb+j)]= -0.5*t(xiobs)%*%(Ai[[j]]+t(Ai[[j]]))%*%riobs

    nabla11[(nb+1):npar, 1:nb]=nabla11[1:nb, (nb+1):(npar)]
    }

  if (ycorr == "AR1") {
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

    nabla11[(nb+1):npar, 1:nb]=nabla11[1:nb, (nb+1):(npar)]
 }

 if (ycorr == "UN") {
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

    nabla11[(nb+1):npar, 1:nb]=nabla11[1:nb, (nb+1):(npar)]
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

        dmean.beta = t(ximis - cit %*% icovyiobs %*% xiobs) %*% Afit0i ## as.matrix(fitp0i)

        dmean.D= matrix(0, nrow=nD, ncol=ncol(Afit0i)) ##numeric(nD)

        if (ycorr== "CS") {
        derD= fun.csmat(sigma=sigma,rho=rho,obsi=length(yi),transform=transform,case=2)
                          } else
        if (ycorr== "AR1") {
        derD= fun.ar1mat(sigma=sigma,rho=rho,obsi=length(yi),transform=transform,case=2)
                           }              
        if (ycorr== "UN") {
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

#' Internal Function for generate compound symmetry matrix and its first and second derivatives wrt D
#'
#' Calculate the compound symmetry matrix and its first and second derivatives wrt D.
#' @param sigma standard deviation
#' @param rho correlation coefficient
#' @param obsi dimension of the square matrix
#' @param transform logical indicating wether or not the parameter in D is transformed.
#' @param case   1: return nobs x nobs CS matrix; 2: return a list for first derivatives of the compound symmetry matrix wrt sigma and rho;
#'               3: eturn an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho.
#' @aliases fun.csmat
#' @export
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


#' Internal Function for generate AR1 matrix and its first and second derivatives wrt D
#'
#' Calculate the autoregressive matrix and its first and second derivatives wrt D.
#' @param sigma standard deviation
#' @param rho autoregressive  parameter
#' @param obsi dimision of the square matrix
#' @param transform logical indicating wether or not the parameter in D is transformed.
#' @param case   1: return nobs x nobs AR1 matrix; 2: return a list for first derivatives of the compound symmetry matrix wrt sigma and rho;
#'               3: eturn an array containing 2nd derivatives of the compound symmetry matrix wrt sigma and rho.
#' @aliases fun.ar1mat
#' @export

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
                res12[i,j]<-abs(i-j)*rho^(abs(i-j)-1)
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


#' Internal Function for generate a general symmetric matrix and its first and second derivatives wrt D
#'
#' Calculate the general symmetric matrix and its first and second derivatives wrt D.
#' @param sigma standard deviation
#' @param rho correlation coefficient
#' @param obsi dimension of the square matrix
#' @param transform logical indicating wether or not the parameter in D is transformed.
#' @param case   1: return nobs x nobs general symmetric  matrix; 2: return a list for first derivatives of the general symmetric matrix wrt D;
#'               3: return an array containing 2nd derivatives of the general symmetric matrix wrt D.
#' @param case2 indicator for variance and covariance parameters. 
#' @aliases fun.generalmat
#' @export

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


#' Internal Function to fit the missing data model and obtain the predicted probabilities of being observed for all observations.
#'
#' Fit missing data model and obtain predicted probabilities of being observed for all observations.
#' @param data the name of the dataset for fitting missing data mechansim model
#' @param gmodel a formula to specify a multinomial transitional missing data model.
#' @aliases fun.gfit
#' @export
fun.gfit<- function(data, gmodel) {
  ## This is the function to fit the multinomial logit model for the missingness


  ##data=data[,!names(data)=="fitprobs"]
  data$obsprob= NA ## initialize the hhat column.
  data$A10= NA
  data$A20= NA
  data$A11= NA
  if (any(!(data$g %in% c("O","I","D")))) stop("Data error: missing status variable must take D, I or O")
  if (any(!(data$gp %in% c("O","I","D","U")))) stop("Data error: missing status variable
                for the prior visit must take D, I, O or U")


  ## set reference level of the response for logistic regression.
  data$g=as.factor(data$g)
  data$g=relevel(data$g, "O")

  ## (1) gp="D: set hhat=0 for all observations after dropout time
  data$obsprob[data$gp=="D"]=0; data$A10[data$gp=="D"]=0; data$A20[data$gp=="D"]=0; data$A11[data$gp=="D"]=0

  ## (2) gp="O": fit multinomial logistic regression to obtain hhat
    data.v0 = data[data$gp=="O",]
    if (! any(data.v0$g=="O")) {            ## If the set of g values is "I", "D" or "I, D"  in this subsample.This is essentially dropouts.
      data$obsprob[data$gp=="O"]=0;data$A10[data$gp=="O"]=0; data$A20[data$gp=="O"]=0; data$A11[data$gp=="O"]=0;      
    }   else if (all(data.v0$g=="O")) { ## If all data are observed in this subsample
     data$obsprob[data$gp=="O"]=1;data$A10[data$gp=="O"]=1; data$A20[data$gp=="O"]=0; data$A11[data$gp=="O"]=0;
      } else if (any(data.v0$g!="O")) {
        if (length(unique(data.v0$g))>2) {     ## g includes "O, I, D" in this subsample.
          mdm.v0 =multinom(gmodel, data=data.v0, Hess=TRUE,maxit=500)
          mdm.v0.fit=as.data.frame(mdm.v0$fit)
          data$obsprob[data$gp=="O"]=mdm.v0.fit$O; data$A10[data$gp=="O"]=as.numeric(data.v0$g=="I")-mdm.v0.fit$I; 
          data$A20[data$gp=="O"]=as.numeric(data.v0$g=="D")-mdm.v0.fit$D; data$A11[data$gp=="O"]=0; 
              }  else if (length(unique(data.v0$g))==2)  {  ## g includes "O, I", or "O,D" in this subsample.
                  mdm.v0 =glm(gmodel, family=binomial(link=logit), data=data.v0)
                  data$obsprob[data$gp=="O"]=1-mdm.v0$fit; 
                  data$A10[data$gp=="O"]=as.numeric(data.v0$g=="I")*(1-mdm.v0$fit);
                  data$A20[data$gp=="O"]=as.numeric(data.v0$g=="D")*(1-mdm.v0$fit);  
                  data$A11[data$gp=="O"]=0;
                                                   }
      } else  print("Warning: check the values of g for the subsample gp=0")

  ## (3) gp="I": 
    data.v1 = data[data$gp=="I",]
    if (any(data.v1$g=="D")) { ## if the set of g values is "D", "O, D", "I, D", "O,I,D"
        stop("Data error: There should no dropout immediately after intermittent missingness. Please check your data for such cases. ")
    } else if (! any(data.v1$g=="O")) {## if the set of g values is "I" only. This is essentially dropouts.
      data$obsprob[data$gp=="I"]=0; data$A10[data$gp=="I"]=0; data$A20[data$gp=="I"]=0; data$A11[data$gp=="I"]=0;  
    } else if (all(data.v0$g=="O")) { ## If all data are observed in this subsample
         data$obsprob[data$gp=="I"]=1; data$A10[data$gp=="I"]=1; data$A20[data$gp=="I"]=0; data$A11[data$gp=="I"]=0;
    } else  if (any(data.v1$g!="O")) { ## the set of g values is " O, I"
      mdm.v1 =glm(gmodel, family=binomial(link=logit), data=data.v1)
      data$obsprob[data$gp=="I"]=data$A11[data$gp=="I"]=1-mdm.v1$fit; 
      data$A10[data$gp=="I"]=data$A20[data$gp=="I"]=0
   ##   if (length(unique(data.v1$g))>2) data$obsprob[data$gp=="I"]=mdm.v1$fit[,1] else
   ##     if (length(unique(data.v1$g))==2)  data$obsprob[data$gp=="I"]=1-mdm.v1$fit[,1]
    } 

   ## (4) gp="U": baseline observations
    data$obsprob[data$gp=="U"]=1; data$A10[data$gp=="U"]=0; data$A20[data$gp=="U"]=0; data$A11[data$gp=="U"]=0

    return(data)

}



## code for some utility function

#'  function for isnigls section expit: expit transformation
#' @param u need more information
#' @export
#' @name expit
#' @aliases expit

expit<-function(u) {
  ## Inverse logit function.
  uu<-pmin(u,709)
  exp(uu)/(1+exp(uu))
}
##

#' function for isnigls section  expit2: expit transformation extended
#' @param u need more information
#' @param p need more information
#' @export
#' @name expit2
#' @aliases expit2

expit2<-function(u,p){
  ## Extended Inverse logit function.
  uu<-pmin(u,709)
  lb<--1/(p-1)
  (exp(uu)+lb)/(1+exp(uu))
}
##


fun.postrans <- function(u)  {

  if (u) stop("Argument has to be positive")
  res <- log(u)
  res
}

fun.trans2pos <- function(u)  {

  res<-exp(u)
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

fun.trans2rho <- function(u)  {

  tol=10^308
  if (u==Inf)   u= tol
  if (u== -Inf) u= -tol
  rho<- 2*exp(u)/(1+exp(u))-1
  rho
}



