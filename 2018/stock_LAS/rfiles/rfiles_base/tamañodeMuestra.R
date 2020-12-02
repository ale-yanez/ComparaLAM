#____________________________________________________________________
#                      TAMA?O DE MUESTRA                             #
#____________________________________________________________________

rm(list=ls())   
library(stringr)
source('~/Documents/Rwork/Functions/Funciones/functions.R')  
dir.1<-'~/Documents/ADMwork/IFOP/2018/Lama_model/sur/nm'
setwd(dir.1)

#------------------------------------------------
dat.file      = 'lamsur1809.dat'
data.0        <- lisread(paste(dir.1,dat.file, sep='/'));
names(data.0) <-  str_trim(names(data.0), side="right")
data.1        <- data.0
rep           <- reptoRlist('lamsur1809.rep')                                               
#std           <- read.table('MATT1809s4.std',header=T,sep="",na="NA",fill=T) 

#---------------------------------------
# ============================================================================== #
# I. INDICES DE ABUNDANCIA                                                       #
# ============================================================================== #
years   <- data.1$Ind[,1]                                                          
nyears  <- data.1$nyrs                                                                
tallas  <- seq(10,52,1)                    
ntallas <- data.1$ntallas                                                        

x  <-c(years,rev(years))
x1 <-c(years[1],years[nyears]+1,nyears+1/2) #xaxp
x2 <-c(years[1]-1,years[nyears]+1) #xlim

#============================================================#
# II. COMPOSICI?N EDAD DE LAS CAPTURAS                       #
#============================================================#
#Proporci?n observada                                        
pobsFm  <-rep$pobs_mflo                                       
pobsFh  <-rep$pobs_hflo                                             
pobsRm  <-rep$pobs_mcru
pobsRh  <-rep$pobs_hcru
#Proporci?n predicha                                         
ppredFm<-rep$ppred_mflo                                     
ppredFh<-rep$Ppred_hflo                                                                             
ppredRm<-rep$ppred_mcru 
ppredRh<-rep$ppred_hcru                              
                               
resflm <-matrix(ncol=ntallas,nrow=nyears)                         
for(i in 1:nyears){                                          
	for(j in 1:ntallas){                                          
		resflm[,j]<-pobsFm[,j]-ppredFm[,j]}}                        
#Proporciones                                                
pFm   <- c(pobsFm,ppredFm); pFm[pFm==0]  <-NA                     
pFh   <- c(pobsFh,ppredFh); pFh[pFh==0]  <-NA                     
pRm   <- c(pobsRm,ppredRm); pRm[pRm==0]  <-NA                     
pRh   <- c(pobsRh,ppredRh); pRh[pRh==0]  <-NA       

#arreglos                                                    
talla <- rep(gl((length(tallas)),length(years),label=tallas),4)     
años <- rep(years,length(tallas)*4)                             
ind  <- c(rep("capt_obs",length(years)*length(tallas)),         
	rep("capt_est",length(years)*length(tallas)))        
pro  <- data.frame(años,talla,ind,pFm,pFh,pRm,pRh)    
# ==========================================================================

#=================================================================#
# M?todo de Ianelli 2002
#=================================================================#
#Flota machos
Ofl <-ppredFm[rowSums(pobsFm)>0,]*(1-ppredFm[rowSums(pobsFm)>0,])
Efl <-(pobsFm[rowSums(pobsFm)>0,]-ppredFm[rowSums(pobsFm)>0,])^2
wfl <-rep(0,length(Ofl[,1]))
for(i in 1:length(Ofl[,1])){
	wfl[i] <-sum(Ofl[i,])/sum(Efl[i,])}

nmfm_ari <-mean(wfl)                      # MEDIA ARITMETICA
nmfm_geo <-exp(sum(log(wfl))/length(wfl)) # MEDIA GEOM?TRICA
nmfm_arm <-1/mean(1/wfl)                  # MEDIA ARM?NICA

#Flota hembras
Oflh <-ppredFh[rowSums(pobsFh)>0,]*(1-ppredFh[rowSums(pobsFh)>0,])
Eflh <-(pobsFh[rowSums(pobsFh)>0,]-ppredFh[rowSums(pobsFh)>0,])^2
wflh <-rep(0,length(Oflh[,1]))
for(i in 1:length(Oflh[,1])){
        wflh[i] <-sum(Oflh[i,])/sum(Eflh[i,])}

nmf_ari <-mean(wflh)                      # MEDIA ARITMETICA
nmf_geo <-exp(sum(log(wflh))/length(wflh)) # MEDIA GEOM?TRICA
nmf_arm <-1/mean(1/wflh)                  # MEDIA ARM?NICA

#------------------------------------------------------------
#Crucero machos
Ore <-ppredRm[rowSums(pobsRm)>0,]*(1-ppredRm[rowSums(pobsRm)>0,])
Ere <-(pobsRm[rowSums(pobsRm)>0,]-ppredRm[rowSums(pobsRm)>0,])^2
wre <-rep(0,length(Ore[,1]))
for(i in 1:length(Ore[,1])){	
	wre[i] <-sum(Ore[i,])/sum(Ere[i,])}
nmrm_ari <-mean(wre)                      # MEDIA ARITMETICA
nmrm_geo <-exp(sum(log(wre))/length(wre)) # MEDIA GEOM?TRICA
nmrm_arm <-1/mean(1/wre)                  # MEDIA ARM?NICA

#Crucero hembras
Oreh <-ppredRh[rowSums(pobsRh)>0,]*(1-ppredRh[rowSums(pobsRh)>0,])
Ereh <-(pobsRh[rowSums(pobsRh)>0,]-ppredRh[rowSums(pobsRh)>0,])^2
wreh <-rep(0,length(Oreh[,1]))
for(i in 1:length(Oreh[,1])){	
        wreh[i] <-sum(Oreh[i,])/sum(Ereh[i,])}
nmr_ari <-mean(wreh)                      # MEDIA ARITMETICA
nmr_geo <-exp(sum(log(wreh))/length(wreh)) # MEDIA GEOM?TRICA
nmr_arm <-1/mean(1/wreh)                  # MEDIA ARM?NICA


#------------------------------------------------------------
NM_Ian <- data.frame(nmFm=c(nmfm_ari,nmfm_geo,nmfm_arm),nmFh=c(nmf_ari,nmf_geo,nmf_arm),
                     nmRm=c(nmrm_ari,nmrm_geo,nmrm_arm),nmRh=c(nmr_ari,nmr_geo,nmr_arm)); NM_Ian




#------------------------------------------------------------

