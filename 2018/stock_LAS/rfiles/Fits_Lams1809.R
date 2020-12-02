# Start ####
rm(list=ls(all=TRUE)); ls()
#library(PBSmodelling)
dir.1 <- ('~/Documents/Rwork/IFOP/LAm_2018/sur2/Salidas')
setwd(dir.1)
dir()
dir.2 <- ('~/Documents/ADMwork/IFOP/2018/Lama_model/sur2/Lamsur1809')

##### CODIGO EN CONSTRUCCIÓN #####
#Simbolo apple significa chunck no corriendo

#```{r echo=FALSE, warning=FALSE, include=FALSE}

#load('~/Rwork/IFOP/2017_lamanorolla_Tallas/Reporte.RData')
library(stringr)
library(ggplot2)
library(R2admb)
library(reshape2)
source('~/Documents/Rwork/Functions/read.admb.R')
source('~/Documents/Rwork/Functions/Funciones/read.report.R')
source('~/Documents/Rwork/Functions/Funciones/functions.R')
source('~/Documents/Rwork/Functions/multiplot.R')

#lamanor <- read.admb('~/Documents/ADMWork/IFOP/2018/Lama_model/norte/Lamnor1809/lamnor1809')
#names(lamanor)

#____________________Modelo Base________________________________________________________________________
setwd(dir.2)
dir()
dat.file      = 'lamsur1809.dat'
data.0        <- lisread(dat.file);
names(data.0) <- str_trim(names(data.0), side='right')
data.1        <- data.0
rep           <- reptoRlist('lamsur1809.rep')
std           <- read.table('lamsur1809.std', header=T,sep="",na="NA",fill=T)

#Gráficos
ppi <- 300

years  <- data.1$Ind[,1]
nyears <- data.1$nyrs
tallas <- data.1$tallas
ntallas<- data.1$ntallas
ages   <- data.1$edades

#_______________________________________________________________________________________________________
# I. Arreglo Indicadores  ####                     
# ______________________________________________________________________________________________________

#Observado                                                                      
obsS <- rep$BCRU[1,] ; obsS[obsS <=1]  <-NA
obsC <- rep$CPUE[1,] ; obsC[obsC <=0.01]   <-NA
obsD <- rep$Desemb[1,]

#predicho                   #stdpredicho                                        
predS <- rep$BCRU[2,]    ;stdpredS  <- subset(std,name=="pred_Bcru")$std
predC <- rep$CPUE[2,]    ;stdpredC <- subset(std,name=="pred_CPUE")$std
predD <- rep$Desemb[2,]  ;stdpredD <- subset(std,name=="pred_Desemb")$std
#Residuos                                                                       
Res_Surv  <-log(obsS)-log(predS)                                             
Res_cpue    <-log(obsC)-log(predC)                                             
Res_Desemb  <-log(obsD)-log(predD)     

x  <-c(years,rev(years))
x1 <-c(years[1],years[nyears]+1,nyears+1/2) #xaxp
x2 <-c(years[1]-1,years[nyears]+1) #xlim

cvsurv <- rep(0.30,nyears)
cvcpue   <- rep(0.15,nyears)
cvdes    <- rep(0.1,nyears)

obsS95i <- obsS*exp(-1.96*cvsurv); obsS95s <-obsS*exp(1.96*cvsurv)
obsC95i <- obsC*exp(-1.96*cvcpue); obsC95s <-obsC*exp(1.96*cvcpue)
obsD95i <- obsD*exp(-1.96*cvdes); obsD95s <- obsD*exp(1.96*cvdes)

#S_flom <- rep$Sflom_age



#  Desembarque ####  

land<-read.csv("Desembarques.csv", head=T, sep=',')
head(land)
summary(land)


landings <- with(land, data.frame(value = c(X, XII, TOTAL), region = factor(rep(c('X Región', 'XII Región','Total'), each = NROW(land))), Year = rep(land$Year, 3)))

  p1<- ggplot(landings, aes(x=Year, y=value, colour=region)) + geom_line() + 
    theme_bw() + theme(legend.position = 'bottom') + xlab('Años') + ylab('Desembarque (t)') + 
    scale_x_continuous(breaks=round(seq(min(landings$Year), 2015, by = 10),1)) + theme(legend.title=element_blank()) +
    scale_colour_hue(name='region',l=40) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))

  p1

ggsave(p1, file='Fig1_Desemb300dpi.png', width=5.5, height=6, dpi=300)
dev.off()




#Ploting fits from ADMB ####

#attach(rep)

 #  Desembarques ####

p2 <-  ggplot(NULL, aes(x=years))+
  geom_line(aes(y= predD, colour="Desemb Est"))+
  geom_point(aes(y= obsD, colour="Desemb Obs"), size = 2, shape = 21)+
  geom_errorbar(aes(ymin = obsD95i, ymax = obsD95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Desemb Est'='royalblue3', 'Desemb Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) +
  ylab('Desembarque (t)') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())
  
  p2
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Fig2.eps",height=5, width=7.5) 
  p2
  dev.off()
  
  #  CPUE ####
  
 p3 <-  ggplot(NULL, aes(x=years))+
    geom_line(aes(y= predC, colour="CPUE Est"))+
    geom_point(aes(y= obsC, colour="CPUE Obs"), size = 2, shape = 21)+
    geom_errorbar(aes(ymin = obsC95i, ymax = obsC95s), colour='black', width=0.6) +
    scale_colour_manual(name='', values=c('CPUE Est'='royalblue3', 'CPUE Obs'='black'), guide='legend') +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
    
    xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) +
    ylab('CPUE (t/h.a.)') + scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
    theme(legend.position = 'bottom')  + theme(legend.title=element_blank())
   
    p3
  
    op<-par(no.readonly=TRUE)
    ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
    postscript("Figuras/Fig3.eps",height=5, width=7.5) 
    p3
    dev.off()  
  
  #  Crucero ####
 
    p4 <-  ggplot(NULL, aes(x=years))+
      geom_line(aes(y= predS, colour="Crucero Est"))+
      geom_point(aes(y= obsS, colour="Crucero Obs"), size = 2, shape = 21)+
      geom_errorbar(aes(ymin = obsS95i, ymax = obsS95s), colour='black', width=0.6) +
      scale_colour_manual(name='', values=c('Crucero Est'='royalblue3', 'Crucero Obs'='black'), guide='legend') +
      guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
      
      xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) +
      ylab('Biomasa (t)') +
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
      theme(legend.position = 'bottom')  + theme(legend.title=element_blank())
    
    p4
    
    op<-par(no.readonly=TRUE)
    ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
    postscript("Figuras/Fig4.eps",height=5, width=7.5) 
    p4
    dev.off()
     
  
    
    
    
  # Talla Media ####
    
  std$name
  
  #Machos flota
  Lobs_mf <- rep$Lm_obs_pred[1,] ; Lobs_mf[Lobs_mf <=1]  <-NA
  Lpred_mf <- rep$Lm_obs_pred[2,] ; Lpred_mf[Lpred_mf <=1]  <-NA
  stdL_mf <- subset(std,name=="Lmf_pred")$std
  
  #stdL_mf[3:8] <- NA
  #stdL_mf[29] <- NA
  
  
  df_mf<-data.frame(cbind(years, Lobs_mf, Lpred_mf, stdL_mf))
  colnames(df_mf) <- c('yrs', 'Lobs_mf', 'Lest') #, 'Lstd')
  head(df_mf)
  

  p5 <-  ggplot(df_mf, aes(x=years))+
    geom_line(aes(y= Lest, colour="Talla Media Est."))+
    geom_point(aes(y= Lobs_mf, colour="Talla Media Obs."), size = 2, shape = 21)+
    #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
    scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
    
    xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) + ylim(24, 42) + 
    ylab('Longitud Cefalotórax (mm)') + 
    #scale_y_continuous(breaks=round(seq(24, 40, by = 4),1)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
    theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))
  
  p5
  
  #Hembras flota
  Lobs_hf <- rep$Lh_obs_pred[1,] ; Lobs_hf[Lobs_hf <=1]  <-NA
  Lpred_hf <- rep$Lh_obs_pred[2,] ; Lpred_hf[Lpred_hf <=1]  <-NA
  stdL_hf <- subset(std,name=="Lhf_pred")$std
  
  Lstd[3:8] <- NA
  Lstd[29] <- NA
  
  df_hf<-data.frame(cbind(years, Lobs_hf, Lpred_hf, stdL_hf))
  colnames(df_hf) <- c('yrs', 'Lobs_hf', 'Lest') #, 'Lstd')
  head(df_hf)
  

  
  p6 <-  ggplot(df_hf, aes(x=years))+
    geom_line(aes(y= Lest, colour="Talla Media Est."))+
    geom_point(aes(y= Lobs_hf, colour="Talla Media Obs."), size = 2, shape = 21)+
    #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
    scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
    
    xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) + ylim(24, 42) +
    #ylab('Longitud Cefalotórax (mm)') + scale_y_continuous(breaks=round(seq(24, 40, by = 2),1)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
    theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))
  
  p6
  
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Lmyh_flo.eps",height=10,width=15) 
  multiplot(p5, p6, cols=2)
  dev.off()
  
  
  
  
   #Machos crucero
  
  Lobs_mc <- rep$Lmc_obs_est[1,] ; Lobs_mc[Lobs_mc <=1]  <-NA
  Lpred_mc <- rep$Lmc_obs_est[2,] ; Lpred_mc[Lpred_mc <=1]  <-NA
  stdL_mc <- subset(std,name=="Lmc_pred")$std
  
  stdL_mc[1:14] <- NA
  stdL_mc[16] <- NA
  stdL_mc[26] <- NA
  
  df_mc<-data.frame(cbind(years, Lobs_mc, Lpred_mc, stdL_mc))
  colnames(df_mc) <- c('yrs', 'Lobs_mc', 'Lest') #, 'Lstd')
  head(df_mc)
  
  
  p7 <-  ggplot(df_mc, aes(x=years))+
    geom_line(aes(y= Lest, colour="Talla Media Est."))+
    geom_point(aes(y= Lobs_mc, colour="Talla Media Obs."), size = 2, shape = 21)+
    #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
    scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
    
    xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) +
    ylab('Longitud Cefalotórax (mm)') + ylim(24, 42) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
    theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))
  
  p7
  
  

  
  
  #Hembras crucero
  Lobs_hc <- rep$Lhc_obs_est[1,] ; Lobs_hc[Lobs_hc <=1]  <-NA
  Lpred_hc <- rep$Lhc_obs_est[2,] ; Lpred_hc[Lpred_hc <=1]  <-NA
  stdL_hc <- subset(std,name=="Lhc_pred")$std
  
  stdL_hc[1:14] <- NA
  stdL_hc[16] <- NA
  stdL_hc[26] <- NA
  
  
  df_hc<-data.frame(cbind(years, Lobs_hc, Lpred_hc, stdL_hc))
  colnames(df_hc) <- c('yrs', 'Lobs_hc', 'Lest') #, 'Lstd')
  head(df_hc)
  
  
  
  p8 <-  ggplot(df_hc, aes(x=years))+
    geom_line(aes(y= Lest, colour="Talla Media Est."))+
    geom_point(aes(y= Lobs_hc, colour="Talla Media Obs."), size = 2, shape = 21)+
    #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
    scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
    
    xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), 2018, by = 3),1)) +
    ylab('Longitud Cefalotórax (mm)') + ylim(24,42) + 
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
    theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))
  
  p8
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Lmyh_cru.eps",height=8,width=15) 
  multiplot(p7, p8, cols=2)
  dev.off()
  
  
   #  Residuales de Desembarque y CPUE ####
  error_Y <- df$YObs - df$YEst
  error_CPUE <- df$CPUE[47:55] - df$CPUEest[47:55]
  
  length(error_CPUE)
  qqnorm(error_CPUE, ylim = c(-1, 1), xlab = "Cuantiles teóricos", ylab = "Residuales CPUE lamanorolla", main = "")
  qqline(error_CPUE, col='red')
  
  
  qqnorm(error_Y,  xlim= c(-2,2), xlab = "Cuantiles teóricos", ylab = "Residuales Desemb lamanorolla", main = "")
  qqline(error_Y, col='red')
  
  
  
  
  # Composición de tallas ####
  names(rep)
  
  
  # Machos Flota
  
  df1 <- data.frame(rep$pobs_mflo)
  df1$yr <- as.factor(years)
  df1 <- df1[-c(1:2, 4:7, 9:15, 22:24), ]
  colnames(df1) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df1[, 40])
  
  d <- melt(df1)
  head(d)
  colnames(d) <- c('yr', 'Tallas', 'value')
  
  #Adding fits
  df2 <- data.frame(rep$ppred_mflo)
  df2$yr <- as.factor(years)
  df2 <- df2[-c(1:2, 4:7, 9:15, 22:24), ]
  colnames(df2) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df2[, 40])
  
  dd <- melt(df2)
  head(dd)
  colnames(dd) <- c('yr2', 'Tallas2', 'value2')
  head(d)
  head(dd)

  dfl <- data.frame(d$yr, d$Tallas, d$value, dd$value2)
  head(dfl)
  colnames(dfl) <- c('yrs', 'Tallas', 'pobs', 'ppred')
  
  #Plotting
  p1 <- ggplot(data=dfl, aes(x=Tallas, y=pobs)) +
    geom_bar(stat="identity", colour='grey') + 
    geom_line(data=dfl, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
    #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
    xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))
  
  
  p1 <- p1 + facet_wrap(~ yrs, dir = 'v', scales='free')  + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.16))
  p1

  ggsave(p1, file='Figuras/TallasM_flo.png', width=12, height=14, dpi=300)
  dev.off()
  
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/TallasM_flo.eps",height=14,width=14) 
  p1
  dev.off()
  
  
  # Hembras Flota
  
  df1 <- data.frame(rep$pobs_hflo)
  df1$yr <- as.factor(years)
  df1 <- df1[-c(1:2, 4:7, 9:15, 22:24), ]
  colnames(df1) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df1[, 40])
  
  d <- melt(df1)
  head(d)
  colnames(d) <- c('yr', 'Tallas', 'value')
  
  #Adding fits
  df2 <- data.frame(rep$Ppred_hflo)
  df2$yr <- as.factor(years)
  df2 <- df2[-c(1:2, 4:7, 9:15, 22:24), ]
  colnames(df2) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df2[, 40])
  
  dd <- melt(df2)
  head(dd)
  colnames(dd) <- c('yr2', 'Tallas2', 'value2')
  head(d)
  head(dd)
  
  dfl <- data.frame(d$yr, d$Tallas, d$value, dd$value2)
  head(dfl)
  colnames(dfl) <- c('yrs', 'Tallas', 'pobs', 'ppred')
  
  #Plotting
  p2 <- ggplot(data=dfl, aes(x=Tallas, y=pobs)) +
    geom_bar(stat="identity", colour='grey') + 
    geom_line(data=dfl, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
    #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
    xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))
  
  
  p2 <- p2 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
  p2
  
  ggsave(p2, file='Figuras/TallasH_flo.png', width=12, height=14, dpi=300)
  dev.off()
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/TallasH_flo.eps",height=14,width=14) 
  p2
  dev.off()
  
  
  
  
  # Machos Crucero
  
  df1 <- data.frame(rep$pobs_mcru)
  df1$yr <- as.factor(years)
  df1 <- df1[-c(1:20, 32), ]
  colnames(df1) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df1[, 40])
  
  d <- melt(df1)
  head(d)
  colnames(d) <- c('yr', 'Tallas', 'value')
  
  #Adding fits
  df2 <- data.frame(rep$ppred_mcru)
  df2$yr <- as.factor(years)
  df2 <- df2[-c(1:20, 32), ]
  colnames(df2) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df2[, 40])
  
  dd <- melt(df2)
  head(dd)
  colnames(dd) <- c('yr2', 'Tallas2', 'value2')
  head(d)
  head(dd)
  
  dfl <- data.frame(d$yr, d$Tallas, d$value, dd$value2)
  head(dfl)
  colnames(dfl) <- c('yrs', 'Tallas', 'pobs', 'ppred')
  
  #Plotting
  p3 <- ggplot(data=dfl, aes(x=Tallas, y=pobs)) +
    geom_bar(stat="identity", colour='grey') + 
    geom_line(data=dfl, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
    #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
    xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))
  
  
  p3 <- p3 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.13))
  p3
  
  
  ggsave(p3, file='Figuras/TallasM_cru.png', width=12, height=14, dpi=300)
  dev.off()
  
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/TallasM_cru.eps",height=14,width=14) 
  p3
  dev.off()
  
  
  # Hembras Crucero
  
  df1 <- data.frame(rep$pobs_hcru)
  df1$yr <- as.factor(years)
  df1 <- df1[-c(1:20, 32), ]
  colnames(df1) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df1[, 40])
  
  d <- melt(df1)
  head(d)
  colnames(d) <- c('yr', 'Tallas', 'value')
  
  #Adding fits
  df2 <- data.frame(rep$ppred_hcru)
  df2$yr <- as.factor(years)
  df2 <- df2[-c(1:20, 32), ]
  colnames(df2) <- c(tallas) #, levels = (lamanor$Tallas))
  
  length(df2[, 40])
  
  dd <- melt(df2)
  head(dd)
  colnames(dd) <- c('yr2', 'Tallas2', 'value2')
  head(d)
  head(dd)
  
  dfl <- data.frame(d$yr, d$Tallas, d$value, dd$value2)
  head(dfl)
  colnames(dfl) <- c('yrs', 'Tallas', 'pobs', 'ppred')
  
  #Plotting
  p4 <- ggplot(data=dfl, aes(x=Tallas, y=pobs)) +
    geom_bar(stat="identity", colour='grey') + 
    geom_line(data=dfl, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
    #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
    xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))
  
  
  p4 <- p4 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
  p4
  
  ggsave(p4, file='Figuras/TallasH_cru.png', width=12, height=14, dpi=300)
  dev.off()
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/TallasH_cru.eps",height=14,width=14) 
  p4
  dev.off()
  
  
  
  #  Selectividad, Fn Reclutamiento, Capturas y Madurez ####
  #Mejorarlo más adelante con 'fill'
  
  
  # Selectividad ####
  
  S_flom <- rep$Sflom_age[1,]
  S_floh <- rep$Sfloh_age[1,]
  S_crum <- rep$Scrum_age[1,]
  S_cruh <- rep$Scruh_age[1,]
  ages <- c(1:11)
  
  
  df <- data.frame(ages, S_flom, S_floh, S_crum, S_cruh)
  head(df)
  
  #Plotting Flota
    p6 <- ggplot(df, aes(x = ages)) + 
      geom_line(aes(y = S_floh, colour = 'Hembras', linetype = 'Hembras')) +
      geom_line(aes(y = S_flom, colour = 'Machos', linetype = 'Machos')) +
    
    scale_color_manual(name = '', values = c('black', 'black')) +      #, limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras')) +
    scale_linetype_manual(name = '', values = c('solid', 'dotted'))    # , limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras'))
    
  p6 <- p6 + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
    theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
    ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Flota')
  
  p6
  
  ggsave(p6, file='Figuras/Sel_Flota.png', width=7.5, height=5, dpi=300)
  dev.off()
  

  
  
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Fig6.eps",height=5, width=7.5) 
  p6
  dev.off()
  
  
  
  #Plotting Crucero
    
  
  
  p7 <- ggplot(df, aes(x = ages)) + 
    geom_line(aes(y = S_cruh, colour = 'Hembras', linetype = 'Hembras')) +
    geom_line(aes(y = S_crum, colour = 'Machos', linetype = 'Machos')) +
    
    scale_color_manual(name = '', values = c('black', 'black')) +      #, limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras')) +
    scale_linetype_manual(name = '', values = c('solid', 'dotted'))    # , limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras'))
  
  p7 <- p7 + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
    theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
    ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Crucero')
  
  p7
  
  ggsave(p7, file='Figuras/Sel_Crucero.png', width=7.5, height=5, dpi=300)
  dev.off()

  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Fig7.eps",height=5, width=7.5) 
  p7
  dev.off()
  
  
  
  # Multiplot Selectividades
  op<-par(no.readonly=TRUE)
  ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
  postscript("Figuras/Sel_flocruc.eps",height=8,width=15) 
  multiplot(p6, p7, cols=2)
  dev.off()
  
  
  
  #  Mortalidad por Pesca ####
  Fval <- exp(lamanor$fit$est[61:115])
  stdF <- lamanor$fit$std[61:115]
  lwr <-Fval-1.96*stdF
  upr <-Fval+1.96*stdF 
  
  #Plotting
  p7 <- ggplot(data=NULL, aes(x=lamanor$yrs)) + 
    geom_line(aes(y = Fval), colour = 'royalblue3') +
    geom_ribbon(data=NULL, aes(ymin=lwr, ymax=upr),fill = 'grey70', alpha = 0.4) + 
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    ylab('Mortalidad por Pesca (1/Año)') + xlab('Años') +
    scale_x_continuous(breaks=round(seq(min(lamanor$yrs), 2015, by = 9),1))
  
  p7
   
  ggsave(p7, file='Fig7_F_300dpi.png', width=7.5, height=5, dpi=300)
  dev.off()
  
  
  #  Biomasas ####
  
  #Total
  Btot <- lamanor$fit$est[174:228]
  stdBtot <- lamanor$fit$std[174:228]
  lwr <-Btot-1.96*stdBtot
  upr <-Btot+1.96*stdBtot 
  
  #Plotting
  p8 <- ggplot(data=NULL, aes(x=lamanor$yrs)) + 
    geom_line(aes(y = Btot), colour = 'royalblue3') +
    geom_ribbon(data=NULL, aes(ymin=lwr, ymax=upr),fill = 'grey70', alpha = 0.4) + 
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    ylab('Biomasa Total (Toneladas)') + xlab('Años') +
    scale_x_continuous(breaks=round(seq(min(lamanor$yrs), 2015, by = 9),1))
  
  p8
  
  ggsave(p8, file='Fig8_Btot_300dpi.png', width=7.5, height=5, dpi=300)
  dev.off()
  
  #Desovante
  
  BD <- lamanor$fit$est[119:173]
  stdBD <- lamanor$fit$std[119:173]
  lwr <-BD-1.96*stdBD
  upr <-BD+1.96*stdBD 
  
  #Plotting
  p9 <- ggplot(data=NULL, aes(x=lamanor$yrs)) + 
    geom_line(aes(y = BD), colour = 'royalblue3') +
    geom_ribbon(data=NULL, aes(ymin=lwr, ymax=upr),fill = 'grey70', alpha = 0.4) + 
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    ylab('Biomasa Desovante (Toneladas)') + xlab('Años') +
    scale_x_continuous(breaks=round(seq(min(lamanor$yrs), 2015, by = 9),1))
  
  p9
  
  ggsave(p9, file='Fig9_BD_300dpi.png', width=7.5, height=5, dpi=300)
  dev.off()
  
  
  #Vulnerable
  
  Bflo <- lamanor$fit$est[284:338]
  stdBflo <- lamanor$fit$std[284:338]
  lwr <-Bflo-1.96*stdBflo
  upr <-Bflo+1.96*stdBflo 
  
  #Plotting
  p10 <- ggplot(data=NULL, aes(x=lamanor$yrs)) + 
    geom_line(aes(y = Bflo), colour = 'royalblue3') +
    geom_ribbon(data=NULL, aes(ymin=lwr, ymax=upr),fill = 'grey70', alpha = 0.4) + 
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    ylab('Biomasa Vulnerable (Toneladas)') + xlab('Años') +
    scale_x_continuous(breaks=round(seq(min(lamanor$yrs), 2015, by = 9),1))
  
  p10
  
  ggsave(p10, file='Fig10_Bflo_300dpi.png', width=7.5, height=5, dpi=300)
  dev.off()
  
  
  
  
  #  Reclutamiento ####

  
  #Plotting
  p11 <- ggplot(data = NULL, aes(x = lamanor$yrs)) + 
    geom_line(aes(y = lamanor$Rpred, linetype = 'Relación S-R')) +
    geom_line(aes(y = lamanor$Reclutas, linetype = 'Reclutamiento Est.')) +
    
    scale_linetype_manual(name = '',
                          values = c('dotted', 'solid'),
                          limits = c('Relación S-R', 'Reclutamiento Est.'),
                          breaks = c('Relación S-R', 'Reclutamiento Est.'))   + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = 'bottom') + scale_x_continuous(breaks=round(seq(min(lamanor$yrs), 2015, by = 9),1)) +
    ylab('Reclutamiento (millones)') + xlab('Años') 
  
  
  
  p11 
    
  
  ggsave(p11, file='Fig11_Reclutas_300dpi.png', width=7.5, height=5, dpi=300)
  dev.off()
  
  
  ggplotly()
  ####SAVE_PLACE####
save.image('Reporte.RData')





