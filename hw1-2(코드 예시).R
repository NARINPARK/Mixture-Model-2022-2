Kim8=function(x,theta){
  new_lik=-10^10;maxitr=1000;hist_lik=NULL
  for (i in 1:maxitr){
    old_lik=new_lik
    mu1=theta[1];mu2=theta[2];p=theta[3]
    mix=p*dnorm(x,mu1,1)+(1-p)*dnorm(x,mu2,1) # Mixture density

    d1_mu1=p*(x-mu1)*dnorm(x,mu1,1) # first derivative of mixture density with respect to mu1
    d1_mu2=(1-p)*(x-mu2)*dnorm(x,mu2,1) # first derivative of mixture density with respect to mu2
    d1_p=dnorm(x,mu1,1)-dnorm(x,mu2,1) # first derivative of mixture density with respect to p
    d2_mu1=p*((x-mu1)^2-1)*dnorm(x,mu1,1) # second derivative of mixture density with respect to mu1
    d2_mu2=p*((x-mu2)^2-1)*dnorm(x,mu2,1) # second derivative of mixture density with respect to mu2
    
    l1=as.matrix(c(sum(d1_mu1/mix),sum(d1_mu2/mix),sum(d1_p/mix))) #first derivative of log-likelihood
    # second derivative of log-likelihood
    l2=matrix(c(sum((d2_mu1*mix-d1_mu1^2)/mix^2),sum(-d1_mu1*d1_mu2/mix^2),sum(-d1_mu1*d1_p/mix^2),
                sum(-d1_mu1*d1_mu2/mix^2),sum((d2_mu2*mix-d1_mu2^2)/mix^2),sum(-d1_mu2*d1_p/mix^2),
                sum(-d1_mu1*d1_p/mix^2),sum(-d1_mu2*d1_p/mix^2),sum(-d1_p^2/mix^2)),3,3)
    # Newton-Raphson update
    theta=theta-solve(l2)%*%l1
    new_lik=sum(log(p*dnorm(x,mu1,1)+(1-p)*dnorm(x,mu2,1))) # Compute log-likelihood
    hist_lik=c(hist_lik,new_lik)
    if (abs(new_lik-old_lik)<1.0e-10) {
      cat(sprintf("Newton-Raphson is converged in %d steps",i),"\n")
      return(list(param=c(theta),loglik=hist_lik))
    }
  }
}

# Data generation
n=500;p=0.3;mu1=-2;mu2=2
x1=rnorm(n,mu1,1)
x2=rnorm(n,mu2,1)
z=rbinom(n,1,p)
x=z*x1+(1-z)*x2

Kim8(x,c(-1,1,0.5))
