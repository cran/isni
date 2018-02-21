
#' Dataset for a survey of sexual behavior
    #'
    #' A dataset from a survey of sexual behavior
    #'
    #'
    #' This data frame contains the following variables:
    #' \itemize{
    #'  \item  y: Response to the survey (1=yes, 0=no)
    #'  \item gender: 0=male; 1=female
    #'  \item fac: the student's faculty (1=medical/dental/veterinary, 0=other)
    #'  \item genderbyfac: the product of gender and fac
    #'  \item  g: an indicator of missing status (0=observed, 1=missing) for y
    #' }
    #'
    #' @docType data
    #' @keywords datasets
    #' @name r99
    #' @usage data(r99)
    #' @format A data frame with 6136 rows and 5 variables
    NULL

    #' Function for ISNI computation when the outcome follows GLMs.
    #'
    #' Calculate the ISNI when the regression outcome is subject to missingness and follows generalized linear models (GLMs)
    #' @param ymodel an object of class "formula": a symbolic description of the model to be fitted for the outcome
    #' @param gmodel an object of class "formula": a symbolic description of the selection model to be fitted for the missingness indictor g
    #' @param ydist  a description of the error distribution to be used in the GLM for the outcome y
    #' @param alldata  the name of data frame containing all the variables in the model and all the observations including those intended to be collected but became missing.
    #' @param flag  an indicator of using log link (instead of inverse) for Gamma distribution
    #' @name isniglm
    #' @aliases isniglm
    #' @export
    #' @examples
    #'    ## load data set
    #'    data(r99)
    #'
    #'    ymodel= y ~ gender+fac+genderbyfac
    #'
    #'    print(summary(glm(ymodel,family=binomial(link=logit), data=r99, subset=g==0)))
    #'
    #'    gmodel= g~gender+fac+genderbyfac
    #'    isnicSBsurvey<-isniglm(ymodel,gmodel=gmodel,ydist='binomial', alldata=r99)
    #'    summary(isnicSBsurvey)
    isniglm = function(ymodel, gmodel, ydist='gaussian', alldata, flag=0) {
    ## Calculate ISNI in a generalized linear model with outcome y, a vector of
    ## predictors x, complete-data indicator g and the distribution family.
    ## This formulation only allows missing data in the outcome vector y.
    ## flag: an indicator of using log link (instead of inverse) for Gamma distribution.

    ymodel.data<-model.frame(ymodel,data=alldata,na.action=NULL)
    x<- as.matrix(model.matrix(ymodel, data=ymodel.data))
    y<- as.vector(model.extract(ymodel.data, "response"))

    gmodel.data<-model.frame(gmodel,data=alldata,na.action=NULL)
    s<- as.matrix(model.matrix(gmodel, data=gmodel.data))
    g<- as.vector(model.extract(gmodel.data, "response"))

    ##x<-as.matrix(x)
    ##y<-as.matrix(y)
    dist.int<-charmatch(ydist,c('gaussian','poisson','binomial','gamma','inverse gaussian'))

    yo<-y[g==0]
    xo<-as.matrix(x[g==0,,drop=F])
    m=rep(1,length(y))
    mo<-m[g==0]
    xm<-as.matrix(x[g==1,,drop=F]) ##,nrow=length(g))
    sdy<-sd(yo)
    dataobs=alldata[g==0,]

    # fitting a glm with observed data
    rego<-switch(dist.int,
    glm(ymodel, data=dataobs),
    glm(ymodel,family=poisson(link=log), data=dataobs),
    glm(ymodel,family=binomial(link=logit), data=dataobs),
    if(flag==1) glm(ymodel,family=Gamma(link=log), data=dataobs) else
    glm(ymodel,family=Gamma(link=inverse), data=dataobs),
    glm(ymodel,family=inverse.gaussian, data=dataobs),
    stop('Invalid family type'))

    coef<-summary(rego)$coef[,1]
    yfit<-rego$fitted.values

    if(ydist=='gaussian') {
    xx<-t(xo)%*%xo
    tnorm<-sum(c(yo-yfit)^2)/length(yo)
    se<-tnorm^0.5*diag(solve(xx))^0.5
    } else se<-summary(rego)$coef[,2]

    cc<-switch(dist.int,
    rep(1,length(y)),
    exp(x%*%coef),
    m*exp(x%*%coef)/(1+exp(x%*%coef))^2,
    if(flag) exp(x%*%coef) else (x%*%coef)^(-2),
    (x%*%coef)^(-3/2),
    stop('Invalid family type'))

    # calculating the first part of ISNI

    if(flag) cc[g==0]<-yo/cc[g==0]
    sscp1<-t(sqrt(cc[g==0])*xo)%*%(sqrt(cc[g==0])*xo)

    # calculating the second part of ISNI
    regzg<-glm(gmodel,family=binomial,data=alldata, maxit=50)
    hm<-1-regzg$fitted.values[g==1]
    ccm<-hm*cc[g==1]
    sscp2<-t(xm)%*%ccm

    # calculating sensitivity transformation

    isni<- solve(sscp1,sscp2)
    tau<-switch(dist.int,
    tnorm,1,1,
    rego$dev/rego$df*(6+rego$dev/rego$df)/(6+2*rego$dev/rego$df),
    sum((yo-yfit)^2/(yo*yfit^2))/length(yo),
    stop('Invalid distribution type'))
    isni<-switch(dist.int,tau*isni,tau*isni,
    tau*isni,tau*isni,-2*tau*isni,
    stop('Invalid distribution type'))

    if(ydist=='poisson'|ydist=='binomial') senstran<-abs(se/isni)
    else senstran<-abs((sdy*se)/isni)
    res=list(coef=coef,se=se,tau=tau,isni=isni,c=senstran)
    class(res) = c(res$class, "isniglm")
    res
    }

    ## Function to print out ISNIGLM, the index of sensitivity to
    ## nonignorability, in a generalized linear model for distributions
    ## of Gaussian, Poisson, Binomial, Gamma, and Inverse Gaussian.
    ## Weihua Gao 07/15/2015
    ##
    #' Function to print out a summary of isniglm  object in a matrix form.
    #'
    #' @param object the isniglm object obtained from the isniglm function
    #' @param ... additional arguements
    #' @export
    #' @name summary.isniglm
    #' @aliases summary.isniglm

    summary.isniglm<-function(object, ...) {

    if (class(object) != "isniglm")  stop('Invalid object class')
    ## Name the columns
    isniname<-c('Est','SE','ISNI','c')

    ## Set up matrix to hold result
    res1<-matrix(0,length(object$coef),length(isniname))
    dimnames(res1)<-list(names(object$coef),isniname)

    for (i in 1:length(object$coef))
       res1[i,]<- c(object$coef[i],object$se[i],object$isni[i],object$c[i])
    print(res1)
    }
