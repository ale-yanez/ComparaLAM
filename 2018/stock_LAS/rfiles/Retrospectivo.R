rm(list=ls())  
library(stringr)
library(knitr)
library(dplyr)
library(ggplot2)
library(reshape)
library(ggthemes)
source('~/Documents/Rwork/Functions/Funciones/functions.R')
source('~/Documents/Rwork/Functions/Funciones/read.report.R')
dir.0<- '~/Documents/ADMwork/IFOP/2018/Lama_model/sur'
dir.1<-'~/Documents/ADMwork/IFOP/2018/Lama_model/sur/Lamsur1809'
dir.2<-'~/Documents/ADMwork/IFOP/2018/Lama_model/sur/Retrospectivo'


#_______________________________________________________________________________________________
setwd(dir.0)
unlink(dir.2,recursive=T) 
dir.create(file.path('~/Documents/ADMwork/IFOP/2018/Lama_model/sur/','Retrospectivo'))#crea la carpeta  nuevamente
setwd(dir.1)
file.copy(c('Lamsur1809.dat', 'lamsur1809', 'lamsur1809.std', 'lamsur1809.tpl'),dir.2)

# 'lamsur.std' es agregado manualmente desde la carpeta 1805

#_______________________________________________________________________________________________
setwd(dir.2) 
#_______________________________________________________________________________________________
# Retro Emp?rico
#_______________________________________________________________________________________________
# Septiembre 2017 (lamnor), 2017 (lamnor18) y septiembre 2018 (lamnor1809)
#std0       <- read.table('lamnor.std',header=T,sep='',na='NA',fill=T)      
#SSBt0      <- subset(std0,name=='BD')$value
#Reclutas0  <- subset(std0,name=='Restim')$value
#Ft0        <- subset(std0,name=='log_Fh')$value

std1       <- read.table('lamsur.std',header=T,sep='',na='NA',fill=T)  
SSBt1      <- subset(std1,name=='BD')$value
Reclutas1  <- subset(std1,name=='Restim')$value
Ft1       <- subset(std1,name=='log_Fh')$value
#---------------------------------------------------------------------------
dat.file2    = 'lamsur1809.dat'
dat2        <- lisread(paste(dir.1,dat.file2, sep='/'));
names(dat2) <- str_trim(names(dat2), side='right')
rep2        <- reptoRlist('lamsur1809.rep')                                          
std2        <- read.table('lamsur1809.std',header=T,sep='',na='NA',fill=T)  
#--------------------------------------------------------------------------
years1  <- seq(1979,2017,1)                                                              
nyears1 <- length(years1)                                                                
x1      <-c(years1,rev(years1))
x1_1    <-c(years1[1],years1[nyears1]+1,nyears1+1/2) #xaxp
x1_2    <-c(years1[1]-1,years1[nyears1]+1) #xlim
years2  <- seq(1979,2018,1)
nyears2 <- length(years2)                                                                
x2      <-c(years2,rev(years2))
x2_1    <-c(years2[1],years2[nyears2]+1,nyears2+1/2) #xaxp
x2_2    <-c(years2[1]-1,years2[nyears2]+1) #xlim
#---------------------------------------------------------------------------
# BIOMASA DESOVANTE sep 2018
SSBt2    <- subset(std2,name=='BD')$value
SSBt2std <- subset(std2,name=='BD')$std
ssbt2    <- c((SSBt2-1.96*SSBt2std)*10^-3,rev((SSBt2+1.96*SSBt2std)*10^-3))
# RECLUTAMIENTOS
Reclutas2     <- subset(std2,name=='Restim')$value
Reclutas2std  <- subset(std2,name=='Restim')$std
rt2           <- c((Reclutas2-1.96*Reclutas2std)*10^-3,rev(Reclutas2+1.96*Reclutas2std)*10^-3)
# MORTALIDAD POR PESCA
Ft2     <- subset(std2,name=='log_Fh')$value
Ft2std  <- subset(std2,name=='log_Fh')$std
ft2     <- c(exp((Ft2)-1.96*(Ft2std)),rev(exp((Ft2)+1.96*(Ft2std)))) 
#____________________________________________________________________________________________
## Indicadores del stock de anchoveta centro-norte
#### *Figuras Comparativas*
png(paste(getwd(),'/Fig1_retroempririco.png',sep=""),width=400,height=500)
par(mfrow=c(3,1),mar=c(2,4,1,1)+0.5)
plot(x2,ssbt2,type="n",ylim=c(0,max(ssbt2)+1.08),cex.axis=0.8,xaxs="i",yaxs="i",xlim=x2_2,xaxp=x2_1,
     ylab="Biomasa desovante (t*10^3)",las=1,xlab="Año",cex.lab=1.1)  
polygon(x2,ssbt2,col=gray(.8,0.5), border="gray85")
#lines(years1,SSBt1*10^-3,lwd=2,col='blue2',lty=2)   
lines(years1,SSBt1*10^-3,lwd=2,col='red2',lty=1)   
lines(years2,SSBt2*10^-3,lwd=2,col='black',lty=1)  
legend(1985, 4.5, c('2017','sept 2018'),lty=c(2,1,1),col=c('red2','black'),
       title="Asesoría",bty="n",lwd=2)
text(2016, 4.5, 'a)',cex=1.2)

plot(x2,rt2 , type="n", xaxp=x2_1, cex.axis=0.8, xaxs= "i", yaxs= "i",
     xlim=x2_2, ylim = c(0,max(rt2)+0.14), ylab="Reclutamientos x 10^9",las=1,xlab="Año", cex.lab=1.1)
polygon(x2, rt2 , col=gray(0.8, 0.5), border= 'gray85')
#lines(years1, Reclutas0*10^-3, lwd=2, col = 'blue2', lty=2)
lines(years1, Reclutas1*10^-3, lwd=2, col = 'red2', lty=1)
lines(years2, Reclutas2*10^-3, lwd=2, col = 'black', lty=1)
text(2016, 0.5,'b)', cex=1.2)

plot(x2, ft2, xaxp=x2_1, cex.axis=0.8, xaxs= "i", yaxs= "i", ylim=c(0,max(rt2)+0.54),
     xlim=x2_2, type="n", ylab="Mortalidad por pesca (F)", las=1, xlab="A?o", cex.lab=1.1)
polygon(x2, ft2,  col=gray(0.8, 0.5), border="gray85")
#lines(years1, exp(Ft0), lwd=2, col='blue2', lty=2)
lines(years1, exp(Ft1), lwd=2, col='red2',  lty=1)
lines(years2, exp(Ft2), lwd=2, col='black', lty=1)
text(2016, 0.8, "c)", cex=1.2)
dev.off()

#___________________________________________________________________________________________
# Retro Tradicional
data.1  <- dat2
retros  <- c(0:4)
n       <- length(retros)

for(i in 1:length(retros)){
	data.1$nyrs    <- dat2$nyrs-retros[i]
	data.1$Ind     <- dat2$Ind[1:(dat2$nyrs-retros[i]),]
	data.1$ETflom  <- dat2$ETflom[1:(dat2$nyrs-retros[i]),]
	data.1$ETfloh  <- dat2$ETfloh[1:(dat2$nyrs-retros[i]),]
	data.1$ETcrum  <- dat2$ETcrum[1:(dat2$nyrs-retros[i]),]
	data.1$ETcruh  <- dat2$ETcruh[1:(dat2$nyrs-retros[i]),]
	writeData(paste('lamsur1809','s',i,'.dat',sep=''), data.1, append=F)}

#___________________________________________________________________________________________
run<-rbind('lamsur1809 -ind %1.dat',' copy lamsur1809.rep %1.rep',' copy lamsur1809.std %1.std')
cat(run,file=(can<-file('run.bash','wb',encoding='UTF-8')),sep='\n')
close(can)

corre <- rep(NA,n)

s     <- seq(1,n,1)
for(i in 1:n){corre[i]<-paste('./run lamsur1809s',s[i],sep='')}
cat(corre,file=(can<-file('corre.bash','wb',encoding='UTF-8')),sep='\n');
close(can)

system('./corre.bash')
#_____________________________________________________________________________________________
# GRAFICO RETROSPECTIVO
#_____________________________________________________________________________________________

png(paste(getwd(), 'Fig2_Retrospectivo.png',sep ="/"),width=350,height=520)
par(mfrow=c(3,1),mar=c(2,4,3,2)+0.1)
plot(x2,ssbt2 , type='n', xaxp=x2_1,cex.axis=0.8,xaxs= 'i',yaxs= 'i', main = 'Biomasa desovante',
     xlim=x2_2, ylim = c(0,max(ssbt2)+1.08), ylab='t x 10e3',las=1,xlab='A?o',cex.lab=1.1)
for(i in 1:length(retros)){
        rep<-reptoRlist(paste('lamsur1809s',i,'.rep',sep=''))
        lines(rep$YRS,rep$BD/10^3,type="l",col=i,lwd=2)}
legend (1979,22, c('BDt','BDt-1','BDt-2','BDt-3','BDt-4'),lty=1,col=seq(1,5,1),bty='n',cex=1)

plot(x2,rt2 , type='n', xaxp=x2_1,cex.axis=0.8,xaxs= 'i',yaxs= 'i', xlim=x2_2,
     ylim = c(0,max(rt2)+0.14), ylab='nº x 10e9',las=1,xlab='Año',cex.lab=1.1, main = 'Reclutamiento')
for(i in 1:length(retros)){
        rep<-reptoRlist(paste('lamsur1809s',i,'.rep',sep=''))
        lines(rep$YRS,rep$Rech_pre_est[2,]*10^-3,type="l",col=i,lwd=2)}
legend (1979,1.7,c('Rt','Rt-1','Rt-2','Rt-3','Rt-4'),lty=1,col=seq(1,5,1),bty='n',cex=1)

plot(x2,ft2 , type='n', xaxp=x2_1,cex.axis=0.8,xaxs= 'i',yaxs= 'i', xlim=x2_2,
     ylim = c(0,max(rt2)+0.54), ylab='F (1/año)',las=1,xlab='Año',cex.lab=1.1, main = 'Mortalidad por pesca')
for(i in 1:length(retros)){
        rep<-reptoRlist(paste('lamsur1809s',i,'.rep',sep=''))
        lines(rep$YRS,rep$Fm_Fh[2,],type="l",col=i,lwd=2)}
legend (1979,2.0,c('Ft','Ft-1','Ft-2','Ft-3','Ft-4'),lty=1,col=seq(1,5,1),bty='n',cex=1)
dev.off()


# ___________________________________________________________________________________
# AN?LISIS DE PATRONES RETROSPECTIVOS "MOHN RHO"
# ___________________________________________________________________________________		

library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

retroR <- retroBD  <- retroF  <- matrix(nrow=length(years2),ncol=length(retros))

for(i in 1:length(retros)){
        rep            <-reptoRlist(paste('lamnor1809s', i, '.rep',sep=''))
        retroBD[,i]  <-c(rep$BD, rep(0,i-1))
        retroR[,i]   <-c(rep$Rech_pre_est[2,], rep(0,i-1))
        retroF[,i]   <-c(rep$Fm_Fh[2,], rep(0,i-1))
                          }
retroBD[retroBD == 0] <- NA
retroR[retroR == 0] <- NA
retroF[retroF == 0] <- NA


rbd <- as.data.frame(retroBD) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
        mutate(type='B desovante') %>% mutate (Periodo=c(rep('t',34),rep('t-1',34),rep('t-2',34),rep('t-3',34),rep('t-4',34))) 
rbR <- as.data.frame(retroR) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
        mutate(type='Reclutas') %>% mutate (Periodo=c(rep('t',34),rep('t-1',34),rep('t-2',34),rep('t-3',34),rep('t-4',34))) 
rbF <- as.data.frame(retroF) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
        mutate(type='Mortalidad por pesca') %>% mutate (Periodo=c(rep('t',34),rep('t-1',34),rep('t-2',34),rep('t-3',34),rep('t-4',34))) 

matretro <- rbind(rbd,rbR,rbF)

figretro <- ggplot(matretro) + geom_line(aes(x = year, y = value, color = Periodo)) + 
        facet_wrap(~type, dir='v',scale='free') + labs(x = 'a?o', y = '')

png(paste(getwd(), 'Fig3_Retrospectivo2.png',sep ="/"),width=350,height=520)
figretro
dev.off()


#C?digo de Francisco y Fernando 
# Mohn rho
years      <- years2
nyears     <- length(years)
nyrs.retro <- length(retros)

mohn.ssb     <- rep(NA, nyrs.retro) 
rel.diff.ssb <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))
mohn.f       <- rep(NA, nyrs.retro)
rel.diff.f   <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))
mohn.r       <- rep(NA, nyrs.retro)
rel.diff.r   <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))

for (j in 1:nyrs.retro) { 
	rel.diff.ssb[,j] <- (retroBD[,(j)]-retroBD[,1])/retroBD[,1]
	mohn.ssb[j]      <- rel.diff.ssb[(nyears-j),j]
	rel.diff.f[,j]   <- (retroF[,(j)]-retroF[,1])/retroF[,1]
	mohn.f[j]        <- rel.diff.f[(nyears-j),j]
        rel.diff.r[,j]   <- (retroR[,(j)]-retroR[,1])/retroR[,1]
        mohn.r[j]        <- rel.diff.r[(nyears-j),j]}


#biomasa desovante
ave.mohn.ssb  <- mean(mohn.ssb)
a0            <- which(rel.diff.ssb==-1,arr.ind=TRUE)
junk          <- rel.diff.ssb
a1            <- which(rel.diff.ssb==-1,arr.ind=TRUE)
ff            <- dim(a1)
for(l in 1:ff[1]){
        rel.diff.ssb[a1[l,1], a1[l,2]]<-NA }

#mortalidad por pesca
ave.mohn.f    <- mean(mohn.f)
a0f           <- which(rel.diff.f==-1,arr.ind=TRUE)
junkf         <- rel.diff.f
a1f           <- which(rel.diff.f==-1,arr.ind=TRUE)
fff           <- dim(a1f)
for(l in 1:fff[1]){
        rel.diff.f[a1f[l,1],a1f[l,2]]<-NA}

#reclutas
ave.mohn.r    <- mean(mohn.r)
a0r           <- which(rel.diff.r==-1,arr.ind=TRUE)
junkr         <- rel.diff.r
a1r           <- which(rel.diff.r==-1,arr.ind=TRUE)
ffr           <- dim(a1r)
for(l in 1:ffr[1]){
        rel.diff.r[a1r[l,1],a1r[l,2]]<-NA}

# Gr?fica 
png(paste(getwd(), 'Fig4_Mohn.png',sep ="/"),width=350,height=520)
par(mfrow=c(3,1),mar=c(2,4,2,2)+0.1)
plot(years,rel.diff.ssb[,1], type = 'p', ylab = 'rel.diff.ssb', xlab = 'a?os' , main='Biomasa desovante', ylim = c(-1.2,1.2))
   for (i in 1:nyears){
        lines(years,rel.diff.ssb[,i],col=i,lwd=1)}
legend(2012,0.0, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989, 0.9,'Rho = 0,063', bty='n', cex=1.2)

plot(years,rel.diff.f[,1], type = 'p', ylab = 'rel.diff.F', xlab = 'a?os' , main='Mortalidad por pesca', ylim = c(-1.2,1.2))
for (i in 1:nyears){
        lines(years,rel.diff.f[,i],col=i,lwd=1)}
#legend(2012,0.05, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989,0.9, 'Rho = -0,032', bty='n', cex=1.2)

plot(years,rel.diff.r[,1], type = 'p', ylab = 'rel.diff.R', xlab = 'a?os' , main='Reclutas', ylim = c(-1.2,1.2))
for (i in 1:nyears){
        lines(years,rel.diff.r[,i],col=i,lwd=1)}
#legend(2012,0.05, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989, 0.9, 'Rho = 0,239', bty='n', cex=1.2)
dev.off()


