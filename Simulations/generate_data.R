generate.data = function(nobs, nvar, method="A"){
  if(method == "A"){
    nvar_true = 5
    true_betas = c(rep(1:5, times=ceiling(nvar_true/5))[1:nvar_true], 
                   rep(0, nvar - nvar_true))
    
    X = matrix(rnorm(nobs*nvar), nrow=nobs)
  }else if(method == "B"){
    nvar_true = 5
    true_betas = c(rep(1:5, times=ceiling(nvar_true/5))[1:nvar_true], 
                   rep(0, nvar - nvar_true))
    
    Sigma = 0.5^abs(as.matrix(expand.grid(i=1:nvar, j=1:nvar))%*%c(1,-1))
    dim(Sigma) = c(nvar, nvar)
    X = MASS::mvrnorm(n=nobs, mu = rep(0, nvar), Sigma = Sigma)
  }else if(method == "ZHa"){
    true_betas = c(c(3,1.5,0,0,2), rep(0, nvar-5))
    Sigma = 0.5^abs(as.matrix(expand.grid(i=1:nvar, j=1:nvar))%*%c(1,-1))
    dim(Sigma) = c(nvar, nvar)
    X = MASS::mvrnorm(n=nobs, mu = rep(0, nvar), Sigma = Sigma)
  }else if(method == "ZHc"){
    true_betas = c(rep(0,10), rep(2,10), rep(0,10), rep(2,10), 
                   rep(0, nvars-40))
    Sigma = 0.5^(abs(as.matrix(expand.grid(i=1:nvar, j=1:nvar))%*%c(1,-1))!=0)
    dim(Sigma) = c(nvar, nvar)
    X = MASS::mvrnorm(n=nobs, mu = rep(0, nvar), Sigma = Sigma)
  }else if(method == "ZHd"){
    true_betas = c(rep(.3,15), rep(0, nvar-15))
    Z1 = matrix(rep(rnorm(nobs, 0, 1),5), nrow=nobs)
    Z2 = matrix(rep(rnorm(nobs, 0, 1),5), nrow=nobs)
    Z3 = matrix(rep(rnorm(nobs, 0, 1),5), nrow=nobs)
    
    X1 = Z1 + matrix(rnorm(nobs*5, 0, 0.01), nrow=nobs)
    X2 = Z2 + matrix(rnorm(nobs*5, 0, 0.01), nrow=nobs)
    X3 = Z3 + matrix(rnorm(nobs*5, 0, 0.01), nrow=nobs)
    X4 = matrix(rnorm(nobs*(nvar-15), 0, 1), nrow=nobs)
    X = cbind(X1,X2,X3,X4)
  }
    
  # se genera el predictor lineal
  y = X%*%true_betas 
  p = exp(y)/(1+exp(y))
  y = rbinom(length(p),1, p)

  return(
      list(x=X, y=y, true_beta = true_betas)
    )
}


list2data.frame = function(data){
  return(
    data.frame(y=data$y, data$x)
  )
}
