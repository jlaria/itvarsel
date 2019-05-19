#==================================
#   REAL DATA CASE STUDY HGUGM
#===============================
# Required parallel libraries
library(foreach)
library(Rmpi)
library(doMPI)

#load("GM-Data.RData")
source("minarConjuntos.R")


cl <- startMPIcluster()
registerDoMPI(cl)

nruns = 200


writeLines(c(""), "log.txt")
wlog("Starting case study")
wlog("Global parameters")
wlog("\tnruns=", nruns)

set.seed(2018)

data.train = list(y=lasRES, x = as.matrix(z))
nvars = ncol(data.train$x)
data.train$x = scale(data.train$x)

result = minarConjuntos(data.train, nruns, verbose = T)
delta = max(mean(data.train$y), 1-mean(data.train$y))
incidencias = getIncidencias(result, delta, nvars)
mC = getModel(result, incidencias, sqrt(nrow(data.train$x)/2))

save.image(file =  "Real.RData")

wlog("\nTHe END")

closeCluster(cl)
mpi.quit()
