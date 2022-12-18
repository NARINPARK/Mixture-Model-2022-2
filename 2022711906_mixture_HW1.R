


#############################################################################################
# Make R function to compute the MLE of (mu1,mu2,p) for the mixture model using the N-R method
#############################################################################################


Park6=function(x,ini_vector){
  
  mu1 = ini_vector[1]
  
  mu2 = ini_vector[2]
  
  p = ini_vector[3]
  
  logl <- vector()
  
  logl[1] <- sum( log(p*dnorm(x,mu1,1)+(1-p)*dnorm(x,mu2,1)) )

  h = 1e-10
  
  i = 1
  
  while(1){
    
    L_theta =  ( (p/sqrt(2*pi)*exp(-(x-mu1)^2/2) ) +((1-p)/sqrt(2*pi)*exp(-(x-mu2)^2/2)) )
    
    c1 = sum( ( p/sqrt(2*pi) * exp(-(x-mu1)^2/2) * (x-mu1) ) 
              / L_theta )
    
    c2 = sum( ( (1-p)/sqrt(2*pi) * exp(-(x-mu2)^2/2) * (x-mu2) )
              / L_theta )
    
    c3 = sum( ( (1/sqrt(2*pi) * exp(-(x-mu1)^2/2) - 1/sqrt(2*pi) * exp(-(x-mu2)^2/2)))
              /  L_theta) 
    
    b1 = sum( -(p*(1-p)/(2*pi)*exp(-(x-mu1)^2/2)*exp(-(x-mu2)^2/2)*(x-mu1)*(x-mu2) )
              / L_theta^2 )
    
    
    b2 = sum(
      (
        (1/sqrt(2*pi)*exp(-(x-mu1)^2/2)*(x-mu1)*L_theta) - 
          ( 
            ( p/sqrt(2*pi) * exp(-(x-mu1)^2/2) * (x-mu1) ) * ( (1/sqrt(2*pi) * exp(-(x-mu1)^2/2) - 1/sqrt(2*pi) * exp(-(x-mu2)^2/2)))
          )
      ) / L_theta^2 
    )
    
    b3 = sum(
      (
        (-1/sqrt(2*pi)*exp(-(x-mu2)^2/2)*(x-mu2)*L_theta) - 
          ( 
            (1/sqrt(2*pi)*exp(-(x-mu1)^2/2) - 1/sqrt(2*pi)*exp(-(x-mu2)^2/2)) * ((1-p)/sqrt(2*pi)*exp(-(x-mu2)^2/2)*(x-mu2)) 
          )
      ) / L_theta^2 
    )
    
    b4 = sum ( (
      
      (p/sqrt(2*pi)*( x*(x-mu1)*exp(-(x-mu1)^2/2) - exp(-(x-mu1)^2/2)-mu1*(x-mu1)*exp(-(x-mu1)^2/2)))*L_theta -
        (p/sqrt(2*pi)*exp(-(x-mu1)^2/2)*(x-mu1)*p/sqrt(2*pi)*exp(-(x-mu1)^2/2)*(x-mu1)
         
        )
    )
    /L_theta^2)
    
    b5 = sum ( (
      
      ((1-p)/sqrt(2*pi)*( x*(x-mu2)*exp(-(x-mu2)^2/2) - exp(-(x-mu2)^2/2)-mu2*(x-mu2)*exp(-(x-mu2)^2/2)))*L_theta -
        ((1-p)/sqrt(2*pi)*exp(-(x-mu2)^2/2)*(x-mu2)*(1-p)/sqrt(2*pi)*exp(-(x-mu2)^2/2)*(x-mu2)
         
        )
    )
    /L_theta^2)
    
    b6 = sum( -( (1/sqrt(2*pi) * exp(-(x-mu1)^2/2) - 1/sqrt(2*pi) * exp(-(x-mu2)^2/2)))^2
                  / L_theta^2  )
    
    A = matrix(c(mu1,mu2,p),nrow=3)
    
    B = matrix(c(b4,b1,b2,b1,b5,b3,b2,b3,b6),nrow=3)
    
    C = matrix(c(c1,c2,c3),nrow=3)  
    
    theta = A-solve(B)%*%C
    
    ini_vector <- theta
    
    mu1 = ini_vector[1]
    mu2 = ini_vector[2]
    p = ini_vector[3]
    
    logl[i+1]<-sum(log(p*dnorm(x,mu1,1)+(1-p)*dnorm(x,mu2,1)) )
    
    
    if((logl[i+1]-logl[i])<h)break
    
    i = i+1
  }
  
  
  est_mu1= mu1
  est_mu2= mu2
  est_p= p

  print(paste("Newton-Raphson is converged in",length(logl),"steps"))
  
  cat(return(c(est_mu1,est_mu2,est_p)))
}

#############################################################################################
# Data generation
#############################################################################################

n=500;p=0.3;mu1=-2;mu2=2
x1=rnorm(n,mu1,1)
x2=rnorm(n,mu2,1)
z=rbinom(n,1,p)
x=z*x1+(1-z)*x2


Park6(x,c(-1,1,.5))

