source("minarConjuntos.R")
source("cluster_methods.R")
library(foreach)


set.seed(2018)

beta = c(1,2,0,5,-2,3, rep(0,394))
X = matrix(rnorm(100*400), nrow = 100)

eta = X%*%beta
p = exp(eta)/(1+exp(eta))
y = rbinom(length(p),1, p)

data.train = list(x=X, y=y)
results = minarConjuntos(data.train, 150, verbose = T)
delta = max(mean(data.train$y), 1-mean(data.train$y))
incidencias = getIncidencias(results, delta, 400)

library(ggplot2)

sinc = sort(incidencias, decreasing = T)[1:20]
sind = factor((beta!=0)[order(incidencias, decreasing = T)[1:20]])
levels(sind) = c("No", "Yes")

ggplot()+
  aes(x = 1:20, y = sinc)+
  geom_point(aes(color = sind, shape = sind), size = 3)+
  geom_line(linetype = "dashed")+
  labs(x = expression("Variable index (sorted by importance "*I[j]*")"),
       y = expression("Importance "*(I[j])), 
       color = "Generating model?", shape= "Generating model?")+
  theme_minimal()
  
