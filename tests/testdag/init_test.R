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
  smsy[l,]=create_intervals(par_l[[l]]$Smsy,allow_hdi_zero=T)$intervals[2:5]
  smax[l,]=create_intervals(par_l[[l]]$Smax,allow_hdi_zero=T)$intervals[2:5]
  umsy[l,]=create_intervals(par_l[[l]]$Umsy,allow_hdi_zero=T)$intervals[2:5]
  
  }


