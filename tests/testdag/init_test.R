#remotes::install_github("andrew-edwards/hdiAnalysis")
library(here)
library(dplyr)
library(hdiAnalysis)

#load estimates of Smsy, Umsy, and Smax for 50 sockeye stocks - compare ETI/HDI for each metric
par_l=list()
smsy=data.frame(l.hdi.95=NA,u.hdi.95=NA,l.eti.95=NA,u.eti.95=NA)
smax=data.frame(l.hdi.95=NA,u.hdi.95=NA,l.eti.95=NA,u.eti.95=NA)
umsy=data.frame(l.hdi.95=NA,u.hdi.95=NA,l.eti.95=NA,u.eti.95=NA)
for(l in 1:50){
  par_l[[l]]=read.csv(here('pars',list.files(here('pars'))[l]))
  smsy[l,]=create_intervals(par_l[[l]]$Smsy,from=NULL)$intervals[2:5]
  l=16
  smax[l,]=create_intervals(par_l[[l]]$Smax,from=NULL)$intervals[2:5]
  
  umsy[l,]=create_intervals(par_l[[l]]$Umsy[par_l[[l]]$Umsy>0],from=NULL)$intervals[2:5]
  
  }


