
##install.packages("matrixcalc")

library(mvtnorm)
library(numDeriv)
library(matrixcalc)
library(nnet)
library(nlme)



#' Function for ISNI computation when the outcome follows LMM.
#'
#' Calculate the ISNI when the regression outcome is subject to missingness and follows linear mixed-effects models (LMMs)
#' @param ymfix an object of class "formula": a two-sided linear formula description of the fixed-effects part of the model to be fitted for the outcome
#' @param ymran an object of class "formula": an one-sided linear formula description of the random-effects part of the model to be fitted for the outcome
#' @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
#' @param predprobobs Null if using buil-in multinomial transitional logistic model to obtain predicted probabilities of being observed;
#'                    otherwise user supply a vector of these probabilities for all the observations in alldata.
#' @param misni  FALSE if using the default approach to computing ISNI with a scalar nonignorability parameter; 
#'               TRUE when computing ISNI with multiple nonignorability parameters.
#' @param alldata  the name of data frame containing all the variables in the model and all the observations including those intended to be collected but became missing.
#' @name isnilmm
#' @aliases isnilmm
#' @export
#' @examples
#'
#' data(qolef)
#' qolef$t12<-qolef$t1*qolef$group
#' qolef$t32<-qolef$t3*qolef$group
#' qolef$t62<-qolef$t6*qolef$group
#' ymfix=y~   t1+t3+t6 + group  + t12 + t32+t62 + perf + sever
#' gmodel=factor(g)~ group+factor(time)+yp+perf+sever
#' ymran=~1|id
#'
#' ##Random intercept model
#' result=isnilmm(ymfix,ymran, gmodel, alldata=qolef)
#' summary(result)
#' 
isnilmm = function(ymfix,ymran, gmodel, predprobobs=NULL, misni=FALSE, alldata) {
  
  model=lme(ymfix,data=alldata, random =ymran,method="ML",na.action=na.omit)
  
  ymodel.data<-model.frame(ymfix,data=alldata,na.action=NULL)
  des<- as.matrix(model.matrix(ymfix, data=ymodel.data))
  y<- as.vector(model.extract(ymodel.data, "response"))
  z<-model.matrix(formula(model$modelStruct$reStr)[[1]],data=alldata)

  sdy= sd(y,na.rm=T)

   ##  fit a missing data model to obtain fitted probabilities for being observed.
  if (is.null(predprobobs)) {
    if (!("g" %in% names(alldata)))  stop("A variable called 'g' for missing data status should be supplied in the data")
    if (!("gp" %in% names(alldata)))  stop("A variable called 'gp' for missing data status for the prior visit should be supplied in the data")
    alldata.mdm= fun.gfit(alldata, gmodel)
    alldata$fitprobs=alldata.mdm$obsprob 
    alldata$A10= alldata.mdm$A10;  alldata$A20= alldata.mdm$A20;  alldata$A11= alldata.mdm$A11
  } else {
    if (!is.null(predprobobs) & misni==T) stop("predprobobs needs to to a matrix to obtain ISNI vector for multiple nonignorability parameter")
    if (!is.vector(predprobobs)) stop("predprobobs should be a vector")
    if (nrow(alldata) != length(predprobobs)) stop ("The length of predprobobs should equal the number of observations in alldata")
    alldata$fitprobs=predprobobs
  }
  ##alldata$prob=fun.mprob(alldata, gmodel)$obsprob
  
  b<- fixef(model)
  nb = length(b)
  nv=ncol(z)
  
  vcovm=VarCorr(model)
  sigma=as.numeric(vcovm[,2])
  sigmav=sigma[-length(sigma)]
  sigmae=sigma[length(sigma)]  
  rhom=VarCorr(model)[-nrow(vcovm),c(-1,-2)]
  corr=as.numeric(rhom[lower.tri(rhom)])
  
  d=c(sigmav,corr,sigmae)
  npar= nb+length(d)
 

  uid = unique(alldata$id)
  
  nabla12=matrix(0, nrow=npar, ncol=3)
  
  ## compute nabla11i=1
  nabla11=matrix(0, nrow=npar, ncol=npar)
  for (i in 1:length(uid)) {
    
    desi= as.matrix(des[alldata$id==uid[i],,drop=F])
    zi= as.matrix(z[alldata$id==uid[i],,drop=F])
    yi= y[alldata$id==uid[i]]
    nabla11=nabla11+ fun.mixsubi(yi=yi,desi=desi,zi=zi,b=b,d=d,gfiti=gfiti, case=1)
    gfiti= NULL; Afiti=NULL
    if (!is.null(predprobobs))  gfiti= alldata$fitprobs[alldata$id==uid[i]]
    else Afiti= cbind(alldata$A10[alldata$id==uid[i]], alldata$A20[alldata$id==uid[i]], alldata$A11[alldata$id==uid[i]])
    ##gfiti= alldata$prob[alldata$id==uid[i]]
    if (any(is.na(yi))) nabla12 = nabla12+ fun.mixsubi(yi=yi,desi=desi,zi=zi,b=b,d=d, gfiti=gfiti, Afiti=Afiti, case=2)
  }
  ##nabla12=apply(nabla12,2,sum)
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
  if (misni==FALSE) {
       isni=apply(isni,1, sum); senstran<-abs((sdy*se)/isni)
   } else
   {
       isni=apply(abs(isni),1, sum); senstran<-abs((sdy*se)/isni)
   }
  res=list(coef=bD,se=se,isni=c(isni),c=c(senstran))
  class(res) = c(res$class, "isnilmm")
  res
}



#' Function to print out a summary of isnilmm  object in a matrix form.
#'
#' @param object the isnilmm object obtained from the isnigls function
#' @param ... additional arguements
#' @export
#' @name summary.isnilmm
#' @aliases summary.isnilmm
summary.isnilmm<-function(object, ...) {
  
  if (class(object) != "isnilmm")  stop('Invalid object class')
  ## Name the columns
  isniname<-c('Est','SE','ISNI','c')
  
  ## Set up matrix to hold result
  res1<-matrix(0,length(object$coef),length(isniname))
  dimnames(res1)<-list(names(object$coef),isniname)
  
  for (i in 1:length(object$coef))
    res1[i,]<- c(object$coef[i],object$se[i],object$isni[i],object$c[i])
  print(res1)
}



fun.dev=function(zi,ni,d,m,n, case=1){
  ## output
  ## case=0 --- return results for the vari-cov matrix of the random effects
  
  #if nv==1 assume only random effect
  ## case=1 --- return results for first derivatives of the vari-cov matrix wrt sigmav
  ## case=2 --- return results for second derivatives of the vari-cov matrix wrt sigmav

  
  #else
  ## case=1 --- return results for first derivatives of the vari-cov matrix wrt sigmav
  ## case=2 --- return results for first derivatives of the vari-cov matrix wrt rho 
  ## case=3 --- return results for second derivatives of the vari-cov matrix wrt sigmav
  ## case=4 --- return results for second derivatives of the vari-cov matrix wrt sigmav and rho
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
  


