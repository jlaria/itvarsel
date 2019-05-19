# PAPER HOSPITAL -- MORE SIMULATIONS
#==================================
# Required parallel libraries
library(foreach)
library(Rmpi)
library(doMPI)

# Global Functions
source("generate_data.R")
source("minarConjuntos.R")

cl <- startMPIcluster()
registerDoMPI(cl)


# Global variables
nruns = 30
train.size = 100
test.size = 5000
nvars_sim = c(40, 100, 400, 1000, 4000)
nreplicas = 30
methods = c("A", "B", "ZHc", "ZHa", "ZHd")

writeLines(c(""), "log.txt")

set.seed(2018)

for(nvars in nvars_sim){
  for(m in methods){
    wlog("Starting simulation")
    wlog("Global parameters")
    wlog("\tmethod=", m)
    wlog("\tnruns=", nruns)
    wlog("\ttrain.size=", train.size)
    wlog("\ttest.size=", test.size)
    wlog("\tnvars=", nvars)
    
    
    # Comenzamos simulando la matriz test de observaciones (pacientes/genes)
    datos.test = generate.data(test.size, nvars, m)
    true_beta = which(datos.test$true_beta!=0)
    
    global.ret = foreach(replica = 1:nreplicas, .combine = "rbind")%dopar%{
      wlog("Starting replica ", replica, " of ", nreplicas)
      
      datos.train = generate.data(train.size, nvars, m)
      
      wlog("Calling minarConjuntos without verbose")
      result = minarConjuntos(datos.train, nruns)
      wlog("minarConjuntos completed")
      
      delta = max(mean(datos.train$y), 1-mean(datos.train$y))
      incidencias = getIncidencias(result, delta, ncol(datos.train$x))
      
      mC = getModel(result, incidencias, sqrt(train.size/2))
      pred = predict.mC(mC, datos.test$x)
      
      nvars.true = length(true_beta)
      
      ret = data.frame(ccr = mean(datos.test$y==pred),
                       tpr = mean(datos.test$y[pred==1]==1),
                       tnr = mean(datos.test$y[pred==0]==0),
                       nvars_selected = length(mC$selected.vars),
                       Eccr = mC$Eccr,
                       sensitivity = sum(mC$selected.vars%in%true_beta)/length(mC$selected.vars),
                       specificity = (nvars-length(mC$selected.vars)-(nvars.true - sum(mC$selected.vars%in%true_beta)))/(nvars-length(mC$selected.vars))
      )
      wlog("CCR ", replica, ": ", mean(datos.test$y==pred), " nvars.true=", nvars)
      ret
    }
    
    
    save(global.ret, file=paste0(m,
                                 nruns, "-",
                                 train.size, "-",
                                 test.size, "-",
                                 nvars, "-",
                                 nreplicas,
                                 ".RData"   ))
    
    wlog("Successfully saved RData")
    
  }
}
# stop cluster
wlog("\nTHe END")
closeCluster(cl)
mpi.quit()

