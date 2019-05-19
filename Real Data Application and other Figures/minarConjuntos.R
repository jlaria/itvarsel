source("cluster_methods.R")

wlog = function(text,...){
  cat(paste0(date(),"\t", text,...,"\n"), file="log.txt", append = T)
}

split.indexes = function(y, p){
  index.0 <- which(y==0)
  index.1 <- which(y==1)
  n0 <- sum(y==0) 
  n1 <- sum(y==1)
  
  ind.0.split <- c(0,ceiling(n0*cumsum(p)))
  ind.1.split <- c(0,ceiling(n1*cumsum(p)))
  index.0 <- sample(index.0, replace = F)
  index.1 <- sample(index.1, replace = F)
  
  l = list()
  for (k in 1:length(p)) {
    set = c(index.0[(ind.0.split[k]+1):ind.0.split[k+1]],
            index.1[(ind.1.split[k]+1):ind.1.split[k+1]])
    set = sample(set)
    l[[k]] = set
  }
  return(l)
}

optimal.cutpoint = function(ypred, y){
  cuts = unique(sort(ypred))
  ccr = rep(0, length(cuts))
  for(c in 1:length(cuts)){
     ccr[c] = mean( ((ypred>=cuts[c])+0)==y )
  }
  return(cuts[which.max(ccr)])
}

minarConjuntos = function(datos.train, nruns, p=c(0.5, 0.5), 
                          verbose=F){
  if(verbose)wlog("Welcome to minarConjuntos version 0.3")
  if(verbose)wlog("Entering foreach loop with ", nruns, " iterations...")
  
  results = foreach(j = 1:nruns, .combine = "rbind")%dopar%
  {
    indices = split.indexes(datos.train$y, p)
    
    d.train = list(x=as.matrix(datos.train$x[indices[[1]],]),
                   y=datos.train$y[indices[[1]]])
    d.validate = list(x=as.matrix(datos.train$x[indices[[2]],]),
                      y=datos.train$y[indices[[2]]])
    
    group.index = as.numeric(cluster.pca(datos.train$x))
    best.search = sglfast::isgl_simple(d.train, d.validate, index = group.index, 
                                       type="logit")
    pred = sglfast::predict.isgl(best.search, d.validate$x)
    cut = optimal.cutpoint(pred, d.validate$y)
    pred = (pred>=cut)+0
    
    ccr <- mean(pred==d.validate$y)
    tpr <- mean(d.validate$y[pred==1]==1)
    tnr <- mean(d.validate$y[pred==0]==0)
    
    selected.vars = which(best.search$beta!=0)
    selected.coef = best.search$beta[best.search$beta!=0]
    selected.intercept = best.search$intercept
    
    result = data.frame(ccr=0, tpr=0, tnr=0, selected.vars=0, selected.coef=0,
                        selected.intercept=0, cutpoint=0)
    result[1,]$ccr <- ccr
    result[1,]$tpr <- tpr
    result[1,]$tnr <- tnr
    result[1,]$selected.vars = list(selected.vars)
    result[1,]$selected.coef = list(selected.coef)
    result[1,]$selected.intercept = selected.intercept
    result[1,]$cutpoint = cut
    
    if(verbose)wlog("ccr=", ccr, "\t iter ",j)
    result
  }
  if(verbose)wlog("minarConjuntos finished!")
  return(results)
}

getIncidencias = function(result, delta, nvars){
  incidencias = rep(0, nvars)
  for (r in 1:nrow(result)) {
    if(length(result$selected.vars[r][[1]])==0)next()
    inc = result$selected.vars[r][[1]]
    incidencias[inc] = incidencias[inc] + (result$ccr[r] - delta)*abs(result$selected.coef[r][[1]])
  }
  return(incidencias/max(incidencias))
}

getModel = function(result, incidencias, nvar_reduced = 150){
  vars = order(incidencias, decreasing = T)[1:nvar_reduced]
  imp = sort(incidencias, decreasing = T)[1:nvar_reduced]
  power = rep(0, nrow(result))
  for(r in 1:nrow(result)){
    coef = abs(result$selected.coef[[r]][result$selected.vars[[r]]%in% vars])
    if(length(coef)>0)
    {
      coef = coef/max(coef)
      power[r] = sum(imp[vars %in% result$selected.vars[[r]]]*coef)/sum(imp)
    }else{
      power[r] = 0
    }
  }
  best = which.max(result$ccr+power)
  
  return(list(
    intercept = result$selected.intercept[best],
    betas = result$selected.coef[[best]],
    selected.vars = result$selected.vars[[best]],
    cutpoint = result$cutpoint[best],
    best = best,
    Eccr = result$ccr[best]
  ))
}
predict.mC = function(model, X){
  pred = model$intercept + X[,model$selected.vars]%*%model$betas
  pred = (1+exp(-pred))^-1
  pred = (pred>=model$cutpoint)+0
  return(pred)
}