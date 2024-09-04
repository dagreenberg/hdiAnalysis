#remotes::install_github("andrew-edwards/hdiAnalysis")
library(here)
library(dplyr)
library(hdiAnalysis)

#load estimates of Smsy, Umsy, and Smax for 50 sockeye stocks - compare ETI/HDI for each metric
par_l=list()
smsy=data.frame(l.eti.95=NA,u.eti.95=NA,l.hdi.95=NA,u.hdi.95=NA,width.eti=NA,width.hdi=NA,cv.eti=NA,cv.hdi=NA,cv.diff=NA,width.diff=NA)
smax=data.frame(l.eti.95=NA,u.eti.95=NA,l.hdi.95=NA,u.hdi.95=NA,width.eti=NA,width.hdi=NA,cv.eti=NA,cv.hdi=NA,cv.diff=NA,width.diff=NA)
umsy=data.frame(l.eti.95=NA,u.eti.95=NA,l.hdi.95=NA,u.hdi.95=NA,width.eti=NA,width.hdi=NA,cv.eti=NA,cv.hdi=NA,cv.diff=NA,width.diff=NA)
for(l in 1:49){
  par_l[[l]]=read.csv(here('pars',list.files(here('pars'))[l]))
  
  ci_smsy=create_intervals(par_l[[l]]$Smsy)$intervals
  ci_smax=create_intervals(par_l[[l]]$Smax)$intervals
  ci_umsy=create_intervals(par_l[[l]]$Umsy)$intervals
  
  smsy[l,1:6]=ci_smsy[2:7]
  smax[l,1:6]=ci_smax[2:7]
  umsy[l,1:6]=ci_umsy[2:7]
  smsy[l,7]=ci_smsy$width_eti/ci_smsy$median
  smax[l,7]=ci_smax$width_eti/ci_smax$median
  umsy[l,7]=ci_umsy$width_eti/ci_umsy$median
  smsy[l,8]=ci_smsy$width_hdi/ci_smsy$median
  smax[l,8]=ci_smax$width_hdi/ci_smax$median
  umsy[l,8]=ci_umsy$width_hdi/ci_umsy$median
  smsy[l,9]=smsy[l,8]-smsy[l,7]
  smax[l,9]=smax[l,8]-smax[l,7]
  umsy[l,9]=umsy[l,8]-umsy[l,7]
  
  smsy[l,10]=smsy[l,6]-smsy[l,5]
  smax[l,10]=smax[l,6]-smax[l,5]
  umsy[l,10]=umsy[l,6]-umsy[l,5]
  }


