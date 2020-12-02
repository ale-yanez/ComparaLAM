#________________________________________________________________________________________________________
# SALIDAS MODELO LAM norte - ACTUALIZACI?N MARZO 2018                                  
# _______________________________________________________________________________________________________
rm(list=ls())   
library(stringr)
library(knitr)
library(dplyr)
library(ggplot2)
library(reshape)
library(ggthemes)

source('~/Documents/Rwork/Functions/Funciones/read.report.R')
source('~/Documents/Rwork/Functions/Funciones/functions.R')
source('~/Documents/Rwork/Functions/multiplot.R')
dir.1<-'~/Documents/ADMwork/IFOP/2018/Lama_model/norte/Lamnor1809'

source('C:/Users/doris.bucarey/Documents/Crustaceos/Funciones/functions.R')
source('C:/Users/doris.bucarey/Documents/Crustaceos/Funciones/read.report.R')
dir.1<-'C:/Users/doris.bucarey/Documents/Crustaceos/CBA_LAM/2019/norte/Lamnor1809'
setwd(dir.1)

#dir.create(file.path(getwd(),'Figuras'))
#____________________Modelo Base________________________________________________________________________
dat.file      = 'lamnor1809.dat'
data.0        <- lisread(paste(dir.1,dat.file, sep='/'));
names(data.0) <- str_trim(names(data.0), side='right')
data.1        <- data.0
rep           <- reptoRlist('lamnor1809.rep')                                               
std           <- read.table('lamnor1809.std', header=T,sep="",na="NA",fill=T)

#____________________Modelo anterior_____________________________________________________________________
dir.2 <-'C:/Users/doris.bucarey/Documents/Crustaceos/CBA_LAM/2019/norte/1805'
setwd(dir.2);file.copy(c('lamnor18.std', 'lamnor18.rep', 'lamnor18.dat'), dir.1)
old <- read.table('lamnor18.std', header=T,sep="",na="NA",fill=T);old
setwd(dir.1)
#____________________Resultados VE_______________________________________________________________________

r  <- std[std[,2]=='Restim',,];    rr  <- matrix(r$value, ncol=1)
bd <- std[std[,2]=='BD',,];        bd1 <- matrix(bd$value, ncol=1)
bt <- std[std[,2]=='BT',,];        bt1 <- matrix(bt$value, ncol=1)
f  <- std[std[,2]=='log_Fh',,];     f1  <- matrix(f$value, ncol=1)

r2  <- old[old[,2]=='Restim',,];    rr2 <- matrix(r2$value, ncol=1)
bd2 <- old[old[,2]=='BD',,];        bdd2 <- matrix(bd2$value, ncol=1)
bt2 <- old[old[,2]=='BT',,];        btt2 <- matrix(bt2$value, ncol=1)
f2  <- old[old[,2]=='log_Fh',,];    ff2  <- matrix(f2$value, ncol=1)

#__________________Arreglo salidas_______________________________________________________________________
years  <- data.1$Ind[,1]
nyears <- data.1$nyrs                                                        
tallas <- data.1$tallas
ntallas<- data.1$ntallas

#__________________Comparaci?n r?pida____________________________________________________________________
x11()
par(mfrow=c(2,2), mar=c(2,4,2,1)+0.5 )
        plot(years, bt1/10^3, t='l', lwd=1, main = 'Biomasa total', ylab = 't x 10e3',ylim = c(0,20))
        lines(years, c(btt2/10^3, NA), col='red', t='l', lty=2)
        #lines(years, c(btt3/10^3,NA), col='blue', t='l', lty=2)
        plot(years, bd1/10^3, t='l', lwd=1, main = 'Biomasa desovante',ylim = c(0,6),ylab = 't x 10e3')
        lines(years, c(bdd2/10^3, NA), col='red', t='l', lty=2)
        legend(1985,6, legend='septiembre 2018', lty=1, lwd=2, bty="n", col=1)
        legend(1985,5.5, legend='enero 2018', lty=2, lwd=1, bty="n", col=2)
        plot(years, rr, t='l', main = 'Reclutas', ylab = 'n? x 10e6')
        lines(years, c(rr2,NA), col='red', t='l', lty=2, ylim=c(0,max(rr2)))
        plot(years, exp(f1), t='l', lwd=1, main = 'Mortalidad por pesca', ylab = '1/a?o')
        lines(years, c(exp(ff2),NA), col='red', t='l', lty=2)
       

#_______________________________________________________________________________________________________
# I. Arreglo Indicadores                       
# ______________________________________________________________________________________________________

#Observado                                                                      
obsR <- rep$BCRU[1,] ; obsR[obsR <=1]  <-NA                                 
obsC <- rep$CPUE[1,] ; obsC[obsC <=0.1]   <-NA 
obsD <- rep$Desemb[1,]

#predicho                   #stdpredicho                                        
predR <- rep$BCRU[2,]    ;stdpredR  <- subset(std,name=="pred_Bcru")$std                    
predC <- rep$CPUE[2,]    ;stdpredC <- subset(std,name=="pred_CPUE")$std                    
predD <- rep$Desemb[2,]                                                  
#Residuos                                                                       
Res_reclan  <-log(obsR)-log(predR)                                             
Res_cpue    <-log(obsC)-log(predC)                                             
Res_Desemb  <-log(obsD)-log(predD)     

x  <-c(years,rev(years))
x1 <-c(years[1],years[nyears]+1,nyears+1/2) #xaxp
x2 <-c(years[1]-1,years[nyears]+1) #xlim

cvreclan <- rep(0.30,nyears)
cvcpue   <- rep(0.15,nyears)
cvdes    <- rep(0.1,nyears)

obsR95i <- obsR*exp(-1.96*cvreclan); obsR95s <-obsR*exp(1.96*cvreclan)
obsC95i <- obsC*exp(-1.96*cvcpue); obsC95s <-obsC*exp(1.96*cvcpue)
obsD95i <- obsD*exp(-1.96*cvdes); obsD95s <- obsD*exp(1.96*cvdes)


#________________________________________________________________________________________________________#
# 1. AJUSTES                                                                    
#________________________________________________________________________________________________________#
y4 <- obsD*10^-3
png(paste(getwd(),"/Figuras/Fig1_desem.png",sep=""),width=600,height=600)              
plot(years,y4,type="p", cex.axis=1.1,xlim=x2, xaxp=x1,cex.lab=1.4, 
  ylim=c(0,max(y4)+1),xaxs="i",yaxs="i",ylab="t x 10e3",las=1,xlab="A?os")
  title(main="Desembarques",font=6,cex.main=1.5,line=0.3,adj=0.5)
  lines(years,predD/10^3,lwd=2,lty=1)
	arrows(x0=years,y0=obsD95i/10^3,x1=years,y1=obsD95s/10^3,length=0.05,angle=90,col=4,lty=1,code=3)
	points(years,obsD/10^3,cex= 1.3,pch=21)
dev.off() 
	
#----------------------------------------------------------------------------------------------------------
png(paste(getwd(),"/Figuras/Fig2_Crucero.png",sep=""),width=600,height=600)      
plot(years,predR/10^3,type="l",cex.axis=1.1,xlim=x2,xaxp=x1,lwd=2,
  ylim=c(0, 50),xaxs= "i",yaxs= "i",ylab = "t x 10e3",las=1,xlab="",cex.lab=1.5)
  title(main="Biomasa cruceros",font=6,cex.main=1.5,line=0.3,adj=0.5)
  arrows(x0=years,y0=obsR95i/10^3,x1=years,y1=obsR95s/10^3,length=0.05,angle=90,col=4,lty=1,code=3)
  points(years,obsR/10^3,cex=1.3,pch=21)
dev.off()

#----------------------------------------------------------------------------------------------------------
png(paste(getwd(),"/Figuras/Fig3_CPUE.png",sep=""),width=600,height=600)      
plot(years,predC,type="l",cex.axis=1.1,xlim=x2,xaxp=x1,lwd=2,
  ylim=c(0,max(predC)+1.5),xaxs= "i",yaxs= "i",ylab="(Kg/h.a.)",las=1,xlab="Año",cex.lab=1.5)
  title(main='CPUE',font=6,cex.main=1.5,line=0.3,adj=0.5)
  arrows(x0=years,y0=obsC95i,x1=years,y1=obsC95s,length=0.05,angle=90,col=4,lty=1,code=3)
  points(years,obsC,cex=1.5,pch=21)
dev.off()

#__________________________________________________________________________________________________________#
# 2. ANALISIS DE RESIDUALES                                  
#__________________________________________________________________________________________________________#

  png(paste(getwd(),'/Figuras/Fig4_resDES.png',sep=''))     
  par(mfrow=c(2,2),mar=c(2,4,2,1)+0.5)
  plot(years,Res_Desemb,xaxp=x1,cex.axis=0.8,type="h",main="Desembarques",ylab="Residuales (escala log)",xlab="")
  abline(h=0,col="darkgray")
  plot(log(predD),Res_Desemb, main="Residuales vs ajustado",ylab="Residuales",xlab="Valor ajustado")
  abline(h=0,col="darkgray")
  hist(Res_Desemb,xlab="Residuales",ylab="Frecuencia",main="Histograma de Residuos")
  qqnorm(Res_Desemb, ylim=c(-0.3,0.3)); qqline(Res_Desemb, col = 2)
  dev.off()
  
  png(paste(getwd(),"/Figuras/Fig5_rescruceros.png",sep="")) 	
  par(mfrow=c(2,2),mar=c(2,4,2,1)+0.5)
  plot(years,Res_reclan,xaxp=x1,cex.axis=0.8,type="h",main="Cruceros",ylab="Residuales (escala log)",xlab="")
  abline(h=0,col="darkgray")
  plot(log(predR),Res_reclan, main="Residuales vs ajustado",ylab="Residuales",xlab="Valor ajustado")
  abline(h=0,col="darkgray")
  hist(Res_reclan,xlab="Residuales",ylab="Frecuencia",main="Histograma de Residuos")
  qqnorm(Res_reclan); qqline(Res_reclan, col = 2)
  dev.off()  

  png(paste(getwd(),"/Figuras/Fig6_rescpue.png",sep="")) 
  par(mfrow=c(2,2),mar=c(2,4,2,1)+0.5)
  plot(years,Res_cpue,xaxp=x1,cex.axis=0.8,type="h",main="CPUE",ylab="Residuales (escala log)",xlab="")
  abline(h=0,col="darkgray")
  plot(log(predC),Res_cpue, main="Residuales vs ajustado",ylab="Residuales",xlab="Valor ajustado")
  abline(h=0,col="darkgray")
  hist(Res_cpue,xlab="Residuales",ylab="Frecuencia",main="Histograma de Residuos")
  qqnorm(Res_cpue); qqline(Res_cpue, col = 2)
  dev.off()
  
  #___________________________________________________________________________________________________________________ 
# II. COMPOSICI?N TALLAS DE LAS CAPTURAS
#____________________________________________________________________________________________________________________

# Composici?n de tallas de la flota machos + residuos
#______________________________________________  
 etfm_obs <- rep$pobs_mflo  #; etfm_obs[etfm_obs==0] <-NA    
 etfm_pre <- rep$ppred_mflo #; etfm_pre[etfm_pre==0] <-NA    
 
 fmobs  <- as.data.frame(etfm_obs) %>%mutate(year=years) %>%melt(id.vars='year') %>%
           mutate(talla=rep(tallas,each=34)) %>% mutate(type='obs')
 fmpred <- as.data.frame(etfm_pre) %>% mutate(year=years) %>% melt(id.vars='year') %>%
           mutate(talla = rep(tallas, each=34)) %>% mutate(type='pred')
  
 matfm <- rbind(fmobs,fmpred)
  
 fig7a <- ggplot(filter(matfm, type=='obs')) + 
           geom_bar(aes(x = talla, y = value), stat="identity", fill='gray66', color = 'gray28') + 
           facet_wrap(~year, dir='v', scale='free', as.table = TRUE) + labs(x = 'Talla', y = 'Proporci?n') #, scale='free'
  
 fig7 <- fig7a + geom_line(data = filter(matfm, type=='pred'), aes(x = talla, y = value),
                           color = 'darkblue', size = 1)
  
 png(paste(getwd(),"/Figuras/Fig7_etfm.png",sep="")) 
 fig7
 dev.off()
 
 # Residuos Flota machos
 Res_etfm <- etfm_obs - etfm_pre
 
 rng <-range(Res_etfm,na.rm=T)
 dd  <-dim(Res_etfm)
 est <-matrix(NA,nrow=dd[1],ncol=dd[2])
 
 for(j in 1:dd[1]){
         for(k in 1:dd[2]){
                 val<-Res_etfm[j,k]
                          if(val>0){
                             est[j,k]<-val/rng[2]}
                          else{
                              est[j,k]<-val/rng[1]*-1}
                   }}
 
 png(paste(getwd(),"/Figuras/Fig8_resetfm.png",sep="")) 
 par(mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
 image(tallas,years,t(est),col=0,yaxt="n",xlab="",ylab="")
 ee  <-dim(est)
 for(n in 1:ee[1]){for(m in 1:ee[2]){vol<-est[n,m]
 if(is.na(vol)==FALSE){
         if(vol>0){points(tallas[m],years[n],pch=19,cex=2.82*sqrt(vol),col=1)}
         if(vol<0){points(tallas[m],years[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
 }}}
 
 mtext("Flota machos",side=3,cex=1.2)
 mtext("Tallas",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
 axis(2,at=years,labels=years,las=2)
 mtext("A?os",side=2,line=4.7,cex=1.1)
 box()
 dev.off()
 
 # Composici?n de tallas de la flota hembras + residuos
 #______________________________________________  
 etfh_obs <- rep$pobs_hflo  
 etfh_pre <- rep$Ppred_hflo 
 
 fhobs  <- as.data.frame(etfh_obs) %>%mutate(year=years) %>%melt(id.vars='year') %>%
         mutate(talla=rep(tallas,each=34)) %>% mutate(type='obs')
 fhpred <- as.data.frame(etfh_pre) %>% mutate(year=years) %>% melt(id.vars='year') %>%
         mutate(talla = rep(tallas, each=34)) %>% mutate(type='pred')
 
 matfh <- rbind(fhobs,fhpred)
 
 fig9a <- ggplot(filter(matfh, type=='obs')) + 
         geom_bar(aes(x = talla, y = value), stat="identity", fill='gray66', color = 'gray28') + 
         facet_wrap(~year, dir='v', scale='free', as.table = TRUE) + labs(x = 'Talla', y = 'Proporci?n') #, scale='free'
 
 fig9 <- fig9a + geom_line(data = filter(matfh, type=='pred'), aes(x = talla, y = value),
                           color = 'darkblue', size = 1)
 
 png(paste(getwd(),"/Figuras/Fig9_etfh.png",sep="")) 
 fig9
 dev.off()
 
 # Residuos Flota hembras
 Res_etfh <- etfh_obs - etfh_pre
 
 rngh <-range(Res_etfh,na.rm=T)
 ddh  <-dim(Res_etfh)
 esth <-matrix(NA,nrow=dd[1],ncol=dd[2])
 
 for(j in 1:ddh[1]){
         for(k in 1:ddh[2]){
                 val<-Res_etfh[j,k]
                 if(val>0){
                         esth[j,k]<-val/rngh[2]}
                 else{
                         esth[j,k]<-val/rngh[1]*-1}
         }}
 
 png(paste(getwd(),"/Figuras/Fig10_resetfh.png",sep="")) 
 par(mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
 image(tallas,years,t(esth),col=0,yaxt="n",xlab="",ylab="")
 eeh  <-dim(esth)
 for(n in 1:eeh[1]){for(m in 1:eeh[2]){vol<-esth[n,m]
 if(is.na(vol)==FALSE){
         if(vol>0){points(tallas[m],years[n],pch=19,cex=2.82*sqrt(vol),col=1)}
         if(vol<0){points(tallas[m],years[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
 }}}
 
 mtext("Flota hembras",side=3,cex=1.2)
 mtext("Tallas",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
 axis(2,at=years,labels=years,las=2)
 mtext("A?os",side=2,line=4.7,cex=1.1)
 box()
 dev.off()
 
 
 # Composici?n de tallas crucero
 #_________________________________________________________________________________________
 etcm_obs <- rep$pobs_mcru[15:34,]
 etcm_pre <- rep$ppred_mcru[15:34,]
 yearc    <- years[15:34]
 
 obsmc    <- as.data.frame(etcm_obs) %>% mutate(year=yearc) %>% melt(id.vars='year') %>%
              mutate(talla = rep(tallas, each=20)) %>% mutate(type='obs')
 predmc   <- as.data.frame(etcm_pre) %>% mutate(year=yearc) %>% melt(id.vars='year') %>%
              mutate(talla = rep(tallas, each=20)) %>% mutate(type='pred')
 mcru     <- rbind(obsmc, predmc)
  
 fig11a   <- ggplot(filter(mcru, type=='obs')) + 
             geom_bar(aes(x = talla, y = value), stat="identity", fill='gray66', 
             color = 'gray28') + 
             facet_wrap(~year, dir='v', scale='free', as.table = TRUE) + labs(x = 'Talla', y = 'Proporci?n') #, scale='free'
  
 fig11 <- fig11a + geom_line(data = filter(mcru, type=='pred'), aes(x = talla, y = value),
                             color = 'firebrick2', size = 1)
  
  png(paste(getwd(),"/Figuras/Fig11_etcm.png",sep="")) 
  fig11
  dev.off()
  
  
  # Residuos Crucero
  Res_etcm <- etcm_obs - etcm_pre
  rng <-range(Res_etcm,na.rm=T)
  dd  <-dim(Res_etcm)
  estc <-matrix(NA,nrow=dd[1],ncol=dd[2])
  
  for(j in 1:dd[1]){
          for(k in 1:dd[2]){
                  val<-Res_etcm[j,k]
                  if(val>0){
                          estc[j,k]<-val/rng[2]}
                  else{
                          estc[j,k]<-val/rng[1]*-1}
          }}
  
  png(paste(getwd(),"/Figuras/Fig12_resetcm.png",sep="")) 
  par(mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
  image(tallas,yearc,t(estc),col=0,yaxt="n",xlab="",ylab="")
  eec  <-dim(estc)
  for(n in 1:eec[1]){for(m in 1:eec[2]){vol<-estc[n,m]
  if(is.na(vol)==FALSE){
          if(vol>0){points(tallas[m],yearc[n],pch=19,cex=2.82*sqrt(vol),col=1)}
          if(vol<0){points(tallas[m],yearc[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
  }}}
  
  mtext("Crucero machos",side=3,cex=1.2)
  mtext("Tallas",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
  axis(2,at=yearc,labels=yearc,las=2)
  mtext("A?os",side=2,line=4.7,cex=1.1)
  box()
  dev.off()
  
# Tallas medias y residuos Falta
#_______________________________________________________
  Lmf    <- subset(std,name=='Lmf_pred')$value
  stdLmf <- subset(std,name=="Lmf_pred")$std
  Lmfi   <- Lmf-1.96*stdLmf;  Lmfs  <- Lmf+1.96*stdLmf 
  bf     <- data.frame(cbind(years,rep$Lm_obs_pred[1,], Lmf,Lmfi,Lmfs))
  names(bf) <- c('yr','fob','fpre','fi','fs')
  Res_Lflo  <-log(bf$fob)-log(bf$fpre) 
  #Res_Lflo2<-Res_Lflo[11:33]; Lmf2 <- Lmf[11:33]
  
  Lhf     <- subset(std,name=='Lhf_pred')$value
  stdLhf  <- subset(std,name=="Lhf_pred")$std
  Lhfi    <- Lhf-1.96*stdLhf;  Lhfs <- Lhf+1.96*stdLhf 
  bfh     <- data.frame(cbind(years,rep$Lh_obs_pred[1,],Lhf,Lhfi,Lhfs))
  names(bfh) <- c('yr','fob','fpre','fi','fs')
  Res_Lfloh  <-log(bfh$fob)-log(bfh$fpre) 
  
  
  Lmc    <- subset(std,name=='Lmc_pred')$value
  stdLmc <- subset(std,name=="Lmc_pred")$std
  Lmci   <- Lmc-1.96*stdLmc;  Lmcs  <- Lmc+1.96*stdLmc 
  bc <- data.frame(cbind(years,rep$Lmc_obs_est[1,], Lmc, Lmci, Lmcs))                 
  names(bc) <- c('yr','cob','cpre','ci','cs')
  #Res_Lcru  <-log(bc$cob)-log(bc$cpre); Res_Lcru2<-Res_Lcru[22:34]
  #Lmc2 <- Lmc[22:34]
  
  png(paste(getwd(),'/Figuras/Fig13_tallasmedias.png',sep=''))
  par(mfrow=c(2,1), mar=c(4,4,3,2)+0.2)
  plot(bf$yr, bf$fob, type = 'p', pch=19, ylim=c(20,40), main= 'Tallas medias Flota',
       xlab='A?os', ylab='LT (cm)')
       arrows(x0=years,y0=bf$fi,x1=years,y1=bf$fs,length=0.05,angle=90,col=1,lty=1,code=3)
       lines(bf$yr,bf$fpre, type = 'l', pch=4, col=4)
  
  plot(bc$yr, bc$cob, type = 'p', pch=19, ylim=c(20,40), main= 'Tallas medias Cruceros',
       xlab='A?os', ylab='LT (cm)')
       arrows(x0=years,y0=bc$ci,x1=years,y1=bc$cs,length=0.05,angle=90,col=1,lty=1,code=3)
       lines(bc$yr,bc$cpre, type = 'l', pch=4, col=2)
  dev.off()
 
 png(paste(getwd(),"/Fig1/Fig17_resflo.png",sep="")) 
 par(mfrow=c(2,2),mar=c(2,4,2,1)+0.5)
 plot(years[11:33],Res_Lflo2,xaxp=x1,cex.axis=0.8,type="h",main="Flota",ylab="Residuales (escala log)",xlab="")
      abline(h=0,col="darkgray")
 plot(log(Lmf2),Res_Lflo2, main="Residuales vs ajustado",ylab="Residuales",xlab="Valor ajustado")
      abline(h=0,col="darkgray")
 hist(Res_Lflo2,xlab="Residuales",ylab="Frecuencia",main="Histograma de Residuos")
      qqnorm(Res_Lflo2); qqline(Res_Lflo2, col = 2)
 dev.off()   
 
 png(paste(getwd(),"/Fig1/Fig18_rescru.png",sep="")) 
 par(mfrow=c(2,2),mar=c(2,4,2,1)+0.5)
 plot(years[22:34],Res_Lcru2,xaxp=x1,cex.axis=0.8,type="h",main="Flota",ylab="Residuales (escala log)",xlab="")
      abline(h=0,col="darkgray")
 plot(log(Lmc2),Res_Lcru2, main="Residuales vs ajustado",ylab="Residuales",xlab="Valor ajustado")
      abline(h=0,col="darkgray")
 hist(Res_Lcru2,xlab="Residuales",ylab="Frecuencia",main="Histograma de Residuos")
      qqnorm(Res_Lcru2); qqline(Res_Lcru2, col = 2)
 dev.off()   
 
