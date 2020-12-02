# Funciones y Directorios ####
rm(list=ls())

# library(stringr)
# library(knitr)
# library(dplyr)
 library(ggplot2)
 library(reshape)
# library(ggthemes)
# library(R2admb)
# library(gridExtra)
 library(ggpubr)

# source('~/Documents/Rwork/Functions/Funciones/functions.R')
# source('~/Documents/Rwork/Functions/Funciones/read.report.R')
# source('~/Documents/Rwork/Functions/Funciones/Fn_PBRs.R')
 source('~/Documents/Rwork/Functions/Funciones/Fn_DiagramaFase.R')
 source('~/Documents/Rwork/Functions/multiplot.R')
 source('~/Documents/Rwork/Functions/read.admb.R')

dir.1<-'~/Documents/Rwork/IFOP/Lam_2019/Estatus1910/norte/'
dir.2<-'~/Documents/ADMwork/IFOP/2018/Lama_model/Consolidado/norte/Lamnor1903'
dir.3<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/Lamnor1910/'



# Lee Reportes actual y anterior ####

setwd(dir.3)
dir()
# system('mv LAM.rep LAM_nor1910.rep')
# system('mv LAM.std LAM_nor1910.std')
# system('mv LAM.par LAM_nor1910.par')
# system('mv LAM.cor LAM_nor1910.cor')

out1 <- read.admb("LAM_nor1910")
names(out1)
std1 <- read.table('LAM_nor1910.std', header=T, sep="", na="NA", fill=T)
names(std1)
# --------------------------------------------------------------- 
setwd(dir.2)
out2 <- read.admb("lamnor1903")
names(out2)
std2 <- read.table('lamnor1903.std', header=T, sep="", na="NA", fill=T)
names(std2)
# --- --- --- --- --- --- --- --- --- --- --- ---


# Para graficar ... ####
yrs <- out1$YRS
nyrs <- length(yrs)
tallas <- seq(10,52,1)
class(tallas)
M <- 0.3
Brms <- out1$BDoLP*0.4
Frms <- out1$Fpbr[3]
B0 <- out1$BDoLP

#Observado
obsD <- out1$Desemb[1,]
obsC <- out1$CPUE[1,] ; obsC[obsC <= 0.01]   <-NA
obsS <- out1$BCRU[1,] ; obsS[obsS <= 1]  <-NA


#predichos y estimados 
predD         <- out1$Desemb[2,]
predC         <- out1$CPUE[2,]
predS         <- out1$BCRU[2,]
Rec_est1      <- subset(std1,name=='Restim')$value
Rec_est2      <- subset(std2,name=='Restim')$value
desvRec1      <- subset(std1,name=='dev_log_Ro')$value
desvRec2      <- subset(std2,name=='dev_log_Ro')$value
BT_est1       <- subset(std1,name=='BT')$value
BT_est2       <- subset(std2,name=='BT')$value
BD_est1       <- subset(std1,name=='BD')$value
BD_est2       <- subset(std2,name=='BD')$value
F_est1        <- exp(subset(std1,name=='log_Fh')$value)
F_est2        <- exp(subset(std2,name=='log_Fh')$value)
#F_est1        <- out1$Fm_Fh[2,]
#F_est2        <- out2$Fm_Fh[2,]
Lobs_mf       <- out1$Lm_obs_pred[1,] ; Lobs_mf[Lobs_mf <=1]  <-NA
Lpred_mf      <- out1$Lm_obs_pred[2,] ; Lpred_mf[Lpred_mf <=1]  <-NA
Lobs_hf       <- out1$Lh_obs_pred[1,] ; Lobs_hf[Lobs_hf <=1]  <-NA
Lpred_hf      <- out1$Lh_obs_pred[2,] ; Lpred_hf[Lpred_hf <=1]  <-NA
Lobs_mc       <- out1$Lmc_obs_est[1,] ; Lobs_mc[Lobs_mc <=1]  <-NA
Lpred_mc      <- out1$Lmc_obs_est[2,] ; Lpred_mc[Lpred_mc <=1]  <-NA
Lobs_hc       <- out1$Lhc_obs_est[1,] ; Lobs_hc[Lobs_hc <=1]  <-NA
Lpred_hc      <- out1$Lhc_obs_est[2,] ; Lpred_hc[Lpred_hc <=1]  <-NA




# std 
stdpredD      <- subset(std1,name=="pred_Desemb")$std
stdpredC      <- subset(std1,name=="pred_CPUE")$std
stdpredS      <- subset(std1,name=="pred_Bcru")$std
stdRec1       <- subset(std1,name=='Restim')$std
stdRec2       <- subset(std2,name=='Restim')$std
stddesvRec1   <- subset(std1,name=='dev_log_Ro')$std
stddesvRec2   <- subset(std2,name=='dev_log_Ro')$std
stdBT1        <- subset(std1,name=='BT')$std
stdBT2        <- subset(std2,name=='BT')$std
stdBD1        <- subset(std1,name=='BD')$std
stdBD2        <- subset(std2,name=='BD')$std
stdF1         <- subset(std1,name=='log_Fh')$std
stdF2         <- subset(std2,name=='log_Fh')$std
stdL_mf       <- subset(std1,name=="Lmf_pred")$std
stdL_hf       <- subset(std1,name=="Lhf_pred")$std
stdL_mc       <- subset(std1,name=="Lmc_pred")$std
stdL_hc       <- subset(std1,name=="Lhc_pred")$std


# Confidence Intervals
cvdes         <- rep(0.1,nyrs)
cvcpue        <- rep(0.15,nyrs)
cvsurv        <- rep(0.30,nyrs)
rec1_lwr      <-Rec_est1-1.96*stdRec1
rec1_upr      <-Rec_est1+1.96*stdRec1
rec2_lwr      <-Rec_est2-1.96*stdRec2
rec2_upr      <-Rec_est2+1.96*stdRec2
desvrec1_lwr  <- desvRec1-1.96*stddesvRec1
desvrec1_upr  <- desvRec1+1.96*stddesvRec1
desvrec2_lwr  <- desvRec2-1.96*stddesvRec2
desvrec2_upr  <- desvRec2+1.96*stddesvRec2
BT1_lwr       <-BT_est1-1.96*stdBT1
BT1_upr       <-BT_est1+1.96*stdBT1
BT2_lwr       <-BT_est2-1.96*stdBT2
BT2_upr       <-BT_est2+1.96*stdBT2
BD1_lwr       <-BD_est1-1.96*stdBD1
BD1_upr       <-BD_est1+1.96*stdBD1
BD2_lwr       <-BD_est2-1.96*stdBD2
BD2_upr       <-BD_est2+1.96*stdBD2
F1_lwr        <-exp(log(F_est1)-1.96*stdF1)
F1_upr        <-exp(log(F_est1)+1.96*stdF1)
F2_lwr        <-exp(log(F_est2)-1.96*stdF2)
F2_upr        <-exp(log(F_est2)+1.96*stdF2)



obsD95i   <- obsD*exp(-1.96*cvdes); obsD95s <- obsD*exp(1.96*cvdes)
obsC95i   <- obsC*exp(-1.96*cvcpue); obsC95s <-obsC*exp(1.96*cvcpue)
obsS95i   <- obsS*exp(-1.96*cvsurv); obsS95s <-obsS*exp(1.96*cvsurv)



#FITS LAMA MODEL ####
setwd(dir.1)
dir()

#  Desembarques ####

p1 <-  ggplot(NULL, aes(x=yrs))+
  geom_line(aes(y= predD, colour="Desemb Est"))+
  geom_point(aes(y= obsD, colour="Desemb Obs"), size = 2, shape = 21)+
  geom_errorbar(aes(ymin = obsD95i, ymax = obsD95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Desemb Est'='royalblue3', 'Desemb Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('Desembarque (t)') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig1.eps",height=5, width=7.5) 
p1
dev.off()

# Indice de Abundncia ####

p2 <-  ggplot(NULL, aes(x=yrs))+
  geom_line(aes(y= predC, colour="CPUE Est"))+
  geom_point(aes(y= obsC, colour="CPUE Obs"), size = 2, shape = 21)+
  geom_errorbar(aes(ymin = obsC95i, ymax = obsC95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('CPUE Est'='royalblue3', 'CPUE Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('Indice Relativo') + scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig2.eps",height=5, width=7.5) 
p2
dev.off()

#  Crucero ####

p3 <-  ggplot(NULL, aes(x=yrs))+
  geom_line(aes(y= predS, colour="Crucero Est"))+
  geom_point(aes(y= obsS, colour="Crucero Obs"), size = 2, shape = 21)+
  geom_errorbar(aes(ymin = obsS95i, ymax = obsS95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Crucero Est'='royalblue3', 'Crucero Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('Biomasa (t)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p3

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig3.eps",height=5, width=7.5) 
p3
dev.off()


# Composición de tallas Flota ####

names(out1)

# Machos Flota

df_mflobs <- data.frame(out1$pobs_mflo)
names <- c(tallas)
colnames(df_mflobs) <- names
df_mflobs$yr <- as.factor(yrs)
df_mflobs <- df_mflobs[-c(3:8, 29), ]
#colnames(df_mflobs) <- c(tallas)

length(df_mflobs[, 34])

d_mflobs <- melt(df_mflobs)
head(d_mflobs)
colnames(d_mflobs) <- c('yr', 'Tallas', 'value')



#Adding fits
df_mfloest <- data.frame(out1$ppred_mflo)
names <- c(tallas)
colnames(df_mfloest) <- names
df_mfloest$yr <- as.factor(yrs)
df_mfloest <- df_mfloest[-c(3:8, 29), ]

length(df_mfloest[, 34])

dd_mfloest <- melt(df_mfloest)
head(dd_mfloest)
colnames(dd_mfloest) <- c('yr2', 'Tallas2', 'value2')


#Gran data frame
d_mflo <- data.frame(d_mflobs$yr, d_mflobs$Tallas, d_mflobs$value, dd_mfloest$value2)
head(d_mflo)
colnames(d_mflo) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p1 <- ggplot(data=d_mflo, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_mflo, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))


p1 <- p1 + facet_wrap(~ yrs, dir = 'v', scales='free')  + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.16))
p1

#ggsave(p1, file='Figs/TallasM_flo.png', width=12, height=14, dpi=300)
#dev.off()


op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig4_TallasM_flo.eps",height=14,width=14) 
p1
dev.off()



# Hembras Flota

df_hflobs <- data.frame(out1$pobs_hflo)
names <- c(tallas)
colnames(df_hflobs) <- names
df_hflobs$yr <- as.factor(yrs)
df_hflobs <- df_hflobs[-c(3:8, 29), ]

length(df_hflobs[, 34])

d_hflobs <- melt(df_hflobs)
head(d_hflobs)
colnames(d_hflobs) <- c('yr', 'Tallas', 'value')


#Adding fits
df_hfloest <- data.frame(out1$Ppred_hflo)
names <- c(tallas)
colnames(df_hfloest) <- names
df_hfloest$yr <- as.factor(yrs)
df_hfloest <- df_hfloest[-c(3:8, 29), ]

length(df_hfloest[, 34])

dd_hfloest <- melt(df_hfloest)
head(dd_hfloest)
colnames(dd_hfloest) <- c('yr2', 'Tallas2', 'value2')

d_hflo <- data.frame(d_hflobs$yr, d_hflobs$Tallas, d_hflobs$value, dd_hfloest$value2)
head(d_hflo)
colnames(d_hflo) <- c('yrs', 'Tallas', 'pobs', 'ppred')


#Plotting
p2 <- ggplot(data=d_hflo, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_hflo, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))


p2 <- p2 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
p2

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig5_TallasH_flo.eps",height=14,width=14) 
p2
dev.off()


# Composición de tallas Crucero ####

# Machos Crucero

df_mcruobs <- data.frame(out1$pobs_mcru)
names <- c(tallas)
colnames(df_mcruobs) <- names
df_mcruobs$yr <- as.factor(yrs)
df_mcruobs <- df_mcruobs[-c(1:14, 16, 26), ]

length(df_mcruobs[, 34])

d_mcruobs <- melt(df_mcruobs)
head(d_mcruobs)
colnames(d_mcruobs) <- c('yr', 'Tallas', 'value')


#Adding fits
df_mcruest <- data.frame(out1$ppred_mcru)
names <- c(tallas)
colnames(df_mcruest) <- names
df_mcruest$yr <- as.factor(yrs)
df_mcruest <- df_mcruest[-c(1:14, 16, 26), ]

length(df_mcruest[, 34])

dd_mcruest <- melt(df_mcruest)
head(dd_mcruest)
colnames(dd_mcruest) <- c('yr2', 'Tallas2', 'value2')


d_mcru <- data.frame(d_mcruobs$yr, d_mcruobs$Tallas, d_mcruobs$value, dd_mcruest$value2)
head(d_mcru)
colnames(d_mcru) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p3 <- ggplot(data=d_mcru, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_mcru, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))


p3 <- p3 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.13))
p3

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig6_TallasM_cru.eps",height=14,width=14) 
p3
dev.off()



# Hembras Crucero

df_hcruobs <- data.frame(out1$pobs_hcru)
names <- c(tallas)
colnames(df_hcruobs) <- names
df_hcruobs$yr <- as.factor(yrs)
df_hcruobs <- df_hcruobs[-c(1:14, 16, 26), ]

length(df_hcruobs[, 34])

d_hcruobs <- melt(df_hcruobs)
head(d_hcruobs)
colnames(d_hcruobs) <- c('yr', 'Tallas', 'value')

#Adding fits
df_hcruest <- data.frame(out1$ppred_hcru)
names <- c(tallas)
colnames(df_hcruest) <- names
df_hcruest$yr <- as.factor(yrs)
df_hcruest <- df_hcruest[-c(1:14, 16, 26), ]

length(df_hcruest[, 34])

dd_hcruest <- melt(df_hcruest)
head(dd_hcruest)
colnames(dd_hcruest) <- c('yr2', 'Tallas2', 'value2')

d_hcru <- data.frame(d_hcruobs$yr, d_hcruobs$Tallas, d_hcruobs$value, dd_hcruest$value2)
head(d_hcru)
colnames(d_hcru) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p4 <- ggplot(data=d_hcru, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_hcru, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))


p4 <- p4 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
p4

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig7_TallasH_cru.eps",height=14,width=14) 
p4
dev.off()




# Tallas Medias Flota ####

#out1$Lm_obs_pred

#Machos flota

stdL_mf[3:8] <- NA
stdL_mf[29] <- NA


df_mf<-data.frame(cbind(yrs, Lobs_mf, Lpred_mf, stdL_mf))
colnames(df_mf) <- c('yrs', 'Lobs_mf', 'Lest', 'Lstd')
head(df_mf)


p5 <-  ggplot(df_mf, aes(x=yrs))+
  geom_line(aes(y= Lest, colour="Talla Media Est."))+
  geom_point(aes(y= Lobs_mf, colour="Talla Media Obs."), size = 2, shape = 21)+
  #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('LC machos (mm)') + ylim(24, 40) + 
  #scale_y_continuous(breaks=round(seq(24, 40, by = 4),1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p5

#Hembras flota

Lstd[3:8] <- NA
Lstd[29] <- NA

df_hf<-data.frame(cbind(yrs, Lobs_hf, Lpred_hf, stdL_hf))
colnames(df_hf) <- c('yrs', 'Lobs_hf', 'Lest', 'Lstd')
head(df_hf)


p6 <-  ggplot(df_hf, aes(x=yrs))+
  geom_line(aes(y= Lest, colour="Talla Media Est."))+
  geom_point(aes(y= Lobs_hf, colour="Talla Media Obs."), size = 2, shape = 21)+
  #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) + 
  ylab('LC hembras (mm)') + ylim(24, 40) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))


  
p6

plot <- ggarrange(p5, #+ rremove("x.text"), 
                  p6, 
                  #labels = c("a", "b"),
                  ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

plot
ggexport(plot, filename = "Figs/p5y6.pdf")

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("Figs/Lmyh_flo.eps",height=10,width=15) 
# multiplot(p5, p6, cols=2)
# dev.off()



# Tallas Medias Crucero ####

#Machos crucero

stdL_mc[1:14] <- NA
stdL_mc[16] <- NA
stdL_mc[26] <- NA
stdL_mc[35] <- NA

df_mc<-data.frame(cbind(yrs, Lobs_mc, Lpred_mc, stdL_mc))
colnames(df_mc) <- c('yrs', 'Lobs_mc', 'Lest', 'Lstd')
head(df_mc)


p7 <-  ggplot(df_mc, aes(x=yrs))+
  geom_line(aes(y= Lest, colour="Talla Media Est."))+
  geom_point(aes(y= Lobs_mc, colour="Talla Media Obs."), size = 2, shape = 21)+
  #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('LC machos (mm)') + ylim(24, 40) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p7



#Hembras crucero

stdL_hc[1:14] <- NA
stdL_hc[16] <- NA
stdL_hc[26] <- NA
stdL_hc[35] <- NA

df_hc<-data.frame(cbind(yrs, Lobs_hc, Lpred_hc, stdL_hc))
colnames(df_hc) <- c('yrs', 'Lobs_hc', 'Lest', 'Lstd')
head(df_hc)


p8 <-  ggplot(df_hc, aes(x=yrs))+
  geom_line(aes(y= Lest, colour="Talla Media Est."))+
  geom_point(aes(y= Lobs_hc, colour="Talla Media Obs."), size = 2, shape = 21)+
  #geom_errorbar(aes(ymin = Lest-Lstd, ymax = Lest+Lstd), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1)) +
  ylab('LC hembras (mm)') + ylim(24,40) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p8

plot <- ggarrange(p7, #+ rremove("x.text"), 
                  p8, 
                  #labels = c("a", "b"),
                  ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

plot
ggexport(plot, filename = "Figs/Lm_Crucero.pdf")

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("Figuras/Lmyh_cru.eps",height=8,width=15) 
# multiplot(p7, p8, cols=2)
# dev.off()



# Selectividad ####

S_flom <- out1$Sflom_age[1,]
S_floh <- out1$Sfloh_age[1,]
S_crum <- out1$Scrum_age[1,]
S_cruh <- out1$Scruh_age[1,]
ages <- c(1:11)


df_Sel <- data.frame(ages, S_flom, S_floh, S_crum, S_cruh)
head(df_Sel)

#Plotting Flota
p6 <- ggplot(df_Sel, aes(x = ages)) + 
  geom_line(aes(y = S_flom, colour = 'Machos', linetype = 'Machos')) +
  geom_line(aes(y = S_floh, colour = 'Hembras', linetype = 'Hembras')) +
  scale_color_manual(name = '', values = c('black', 'black')) +      #, limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras')) +
  scale_linetype_manual(name = '', values = c('solid', 'dotted'))    # , limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras'))

p6 <- p6 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
  ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Flota')

p6

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig8_SelFlo.eps",height=5, width=7.5) 
p6
dev.off()




p7 <- ggplot(df_Sel, aes(x = ages)) + 
  geom_line(aes(y = S_crum, colour = 'Machos', linetype = 'Machos')) +
  geom_line(aes(y = S_cruh, colour = 'Hembras', linetype = 'Hembras')) +
  scale_color_manual(name = '', values = c('black', 'black')) +      #, limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras')) +
  scale_linetype_manual(name = '', values = c('solid', 'dotted'))    # , limits = c('Machos', 'Hembras'), breaks = c('Machos', 'Hembras'))

p7 <- p7 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
  ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Crucero')

p7

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Fig7.eps",height=5, width=7.5) 
p7
dev.off()



# Multiplot Selectividades
op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs/Sel_flocru.eps",height=8,width=15) 
multiplot(p6, p7, cols=2)
dev.off()


# Sel Crucero bloques ####

library(plotly)
Machos_Crucero <- out1$Scrum_age
Hembras_Crucero <- out1$Scruh_age
p1 <- plot_ly(x = ages, y = yrs, z = ~Machos_Crucero) %>% add_surface()
p1

p2 <- plot_ly(x = ages, y = yrs, z = ~Hembras_Crucero) %>% add_surface()
p2




# Reclutamiento ####


p8 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = c(Rec_est2,NA), colour = 'mar 2019', linetype = 'mar 2019')) +
  geom_line(aes(y = Rec_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(rec2_lwr,NA), ymax=c(rec2_upr,NA)), fill = 'grey70', alpha = 0.4) + 
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1'),
                     limits = c('oct 2019', 'mar 2019'),
                     breaks = c('oct 2019', 'mar 2019')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash'),
                        limits = c('oct 2019', 'mar 2019'),
                        breaks = c('oct 2019', 'mar 2019'))

p8 <- p8 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p8


# Desvíos Reclutamiento ####

p9 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = desvRec1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(desvRec2,NA), colour = 'mar 2019', linetype = 'mar 2019')) +
  geom_ribbon(data=NULL, aes(ymin=desvrec1_lwr, ymax=desvrec1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(desvrec2_lwr,NA), ymax=c(desvrec2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(0,35)), colour = '', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1','black'),
                     limits = c('oct 2019', 'mar 2019', ''),
                     breaks = c('oct 2019', 'mar 2019', '')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash','dotted'),
                        limits = c('oct 2019', 'mar 2019', ''),
                        breaks = c('oct 2019', 'mar 2019', ''))


p9 <- p9 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Desvíos Reclutamientos') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p9

install.packages("ggarrange", dependencies=TRUE, repos='http://cran.rstudio.com/')


plot <- ggarrange(p8, #+ rremove("x.text"), 
                  p9, 
          #labels = c("A", "B"),
          ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

ggexport(plot, filename = "Figs/p8y9.pdf")


if(FALSE){

  library("cowplot")
  plot_grid(p8, p9 + rremove("x.text"),
            #labels = c("A", "B"),
            ncol = 1, nrow = 2)
  
  
  library("gridExtra")
  grid.arrange(p8, p9 + rremove("x.text"),
               ncol = 1, nrow = 2)  
}


# Biomasa Total ####

p10 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(BT_est2,NA), colour = 'mar 2019', linetype = 'mar 2019')) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(BT2_lwr,NA), ymax=c(BT2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1'),
                     limits = c('oct 2019', 'mar 2019'),
                     breaks = c('oct 2019', 'mar 2019')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash'),
                        limits = c('oct 2019', 'mar 2019'),
                        breaks = c('oct 2019', 'mar 2019'))

p10 <- p10 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p10


# Biomasa Desovante ####

p11 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(BD_est2,NA), colour = 'mar 2019', linetype = 'mar 2019')) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(BD2_lwr,NA), ymax=c(BD2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(Brms,35)), colour = 'Brms', linetype = 'Brms')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse3'),
                     limits = c('oct 2019', 'mar 2019', 'Brms'),
                     breaks = c('oct 2019', 'mar 2019', 'Brms')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash'),
                        limits = c('oct 2019', 'mar 2019', 'Brms'),
                        breaks = c('oct 2019', 'mar 2019', 'Brms'))

p11 <- p11 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p11


plot <- ggarrange(p10, p11, #+ rremove("x.text"), 
                  #labels = c("A", "B"),
                  ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")




# Mortalidad por Pesca ####

p12 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(F_est2,NA), colour = 'mar 2019', linetype = 'mar 2019')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(F2_lwr,NA), ymax=c(F2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(M,35)), colour = 'M', linetype = 'M')) +
  geom_line(aes(y = c(rep(Frms,35)), colour = 'Frms', linetype = 'Frms')) +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse3','black'),
                     limits = c('oct 2019', 'mar 2019', 'M', 'Frms'),
                     breaks = c('oct 2019', 'mar 2019', 'M', 'Frms')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash','dotted'),
                        limits = c('oct 2019', 'mar 2019', 'M', 'Frms'),
                        breaks = c('oct 2019', 'mar 2019', 'M', 'Frms'))

p12 <- p12 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p12
#ggsave(p12, file='Figs/Fig12TRY.pdf', width=7.5, height=5, dpi=300)
#dev.off()

ggexport(p12, filename = "Figs/Fig12.pdf", width=7.5, height=5, dpi=300)




# PBRs y variables de estado ####


p11_2 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_line(aes(y = c(rep(Brms,35)), colour = 'Brms', linetype = 'Brms')) +
  geom_line(aes(y = c(rep(B0,35)), colour = 'BDo', linetype = 'BDo')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse3', 'black'),
                     limits = c('oct 2019',  'Brms', 'BDo'),
                     breaks = c('oct 2019',  'Brms', 'BDo')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash','dotted'),
                        limits = c('oct 2019', 'Brms', 'BDo'),
                        breaks = c('oct 2019', 'Brms', 'BDo'))

p11_2 <- p11_2 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p11_2



p12_2 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey37', alpha = 0.4) +
  geom_line(aes(y = c(rep(Frms,35)), colour = 'Frms', linetype = 'Frms')) +
  
  scale_color_manual(name = '',
                     values = c('royalblue3','red'),
                     limits = c('oct 2019', 'Frms'),
                     breaks = c('oct 2019', 'Frms')) +
  
  scale_linetype_manual(name = '',
                        values = c('solid','dotted'),
                        limits = c('oct 2019', 'Frms'),
                        breaks = c('oct 2019',  'Frms'))

p12_2 <- p12_2 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p12_2

p_1 <- ggarrange(p11_2, p12_2,
                 ncol = 2, nrow = 1)

#p_1 <- ggarrange(p11_2 + font('x.text', size = 5), p12_2 + font('x.text', size = 5),
#                 ncol = 2, nrow = 1, widths = c(1.2, 1.5))



p13 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1/Brms, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(rep(1,35)), colour = 'Brms', linetype = 'Brms')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse3'),
                     limits = c('oct 2019',  'Brms'),
                     breaks = c('oct 2019',  'Brms')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash'),
                        limits = c('oct 2019', 'Brms'),
                        breaks = c('oct 2019', 'Brms'))

p13 <- p13 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('BD/BDrms') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p13


p14 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1/Frms, colour = 'oct 2019', linetype = 'oct 2019')) +
  geom_line(aes(y = c(rep(1,35)), colour = 'Frms', linetype = 'Frms')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse3'),
                     limits = c('oct 2019',  'Frms'),
                     breaks = c('oct 2019',  'Frms')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash'),
                        limits = c('oct 2019', 'Frms'),
                        breaks = c('oct 2019', 'Frms'))

p14 <- p14 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('F/Frms') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2019, by = 2),1))

p14

p_2 <- ggarrange(p13, p14,
                 ncol = 2, nrow = 1)

plot <- ggarrange(p_1, p_2, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")


#ggexport(plot, filename = "Figs/PBRs.pdf", width=7.5, height=6, dpi=300)




# txt ####

VarPobl<- cbind(years=yrs, BD=BD_est1, BT=BT_est1, R=Rec_est1, F_est=F_est1,
                 "F/FRMS"=F_est1/Frms, "BD/BDRMS"=BD_est1/Brms, "Y/BT"=predD/BT_est1)

write.table(VarPobl, 'Var_Pobl.txt', append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


like<- cbind(CPUE=out1$LIKE[1], Crucero=out1$LIKE[2], Desemb=out1$LIKE[3], prop=out1$LIKE[4], 
             prop_mflo=out1$LIKE[5], prop_hflo=out1$LIKE[6], pobs_crum=out1$LIKE[7], pobs_cruh=out1$LIKE[8],
             Ro=out1$LIKE[9], No_m=out1$LIKE[10], No_h=out1$LIKE[11], Lo_m=out1$LIKE[12], Lo_h=out1$LIKE[13], cvage_m=out1$LIKE[14], cvage_h=out1$LIKE[15])
like
write.table(like, 'verosimilitud.txt', append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)



# Diagrama de Fases ####

estatus <- "Asesoría de octubre 2019"

  years1       <- yrs
Bo1           <- rep1$BDoLP              # Paso 4: Obtenci?n de Bo
  BRMS1         <- Brms                       # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
  FRMS1         <- Frms
BLIM1         <- Bo1*0.275                        # Paso 6: Obtenci?n de Blim = 20%Bo 
  FLIM1         <- 1.27                             # Paso 6: Obtenci?n de Flim = 30%SPRo
  SpB1          <- BD_est1                          # BD serie histórica de evaluación de stock 
  SpBSE1        <- stdBD1                         # desviaci?n estandar BD
  ln_Fyr1       <- subset(std1,name=='log_Fh')$value      # logaritmo de Ft
  ln_FSE1       <- subset(std1,name=='log_Fh')$std                           # logaritmo de la desviaci?n standar de Ft

DiagramaFase(estatus,years1,SpB1,SpBSE1,ln_Fyr1,ln_FSE1,FRMS1,BRMS1,BLIM1,FLIM1,color=F,dir.1,etiqueta=F)


Bo2           <- rep2$BDoLP              # Paso 4: Obtenci?n de Bo
BRMS2         <- Bo2*0.4                        # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
FRMS2         <- 0.26
BLIM2         <- Bo2*0.275                        # Paso 6: Obtenci?n de Blim = 20%Bo 
FLIM2         <- 1.27                             # Paso 6: Obtenci?n de Flim = 30%SPRo
SpB2          <- SSBt2                            # BD serie hist?rica de evaluaci?n de stock 
SpBSE2        <- SSBt2std                         # desviaci?n estandar BD
ln_Fyr2       <- Ft2                              # logaritmo de Ft
ln_FSE2       <- Ft2std                           # logaritmo de la desviaci?n standar de Ft



