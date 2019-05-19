cluster.pca = function(X){
  pc <- prcomp(X, scale. = F, center = F )
  inc.comp = which(summary(pc)$importance[3,] < 0.99)
  rotation = pc$rotation[,inc.comp]
  index <- apply(abs(rotation),1,which.max)
  return(index)
}

cluster.pls = function(data,
                       type = "logit"
                       ){
  if(type == "logit"){
    plsda.fit = caret::plsda(x=data$x, y=as.factor(data$y), scale=F, ncomp=min(nrow(data$x),ncol(data$x))-1)
    Xvar=sort(plsda.fit$Xvar, decreasing = T)
    inc.comp = which(cumsum(Xvar/plsda.fit$Xtotvar)<0.9)
    loadings = plsda.fit$loadings[,names(inc.comp)]
    index <- apply(abs(loadings),1,which.max)
  }else if(type=="linear"){
    pls.fit = pls::plsr(y~., data=data, scale=F)
    index <- apply(abs(pls.fit$loadings),1,which.max)
  }
  return(index)
}

cluster.random=function(X, nclust=20){
  index = sample(nclust, replace = T, size = ncol(X))
  return(index)
}

cluster.kmeans=function(X, nclust=20){
  # we cluster variables
  km = kmeans(t(X), centers = nclust)
  return(km$cluster)
}
