rm(list=ls())

# Funciones y Directorios ####

 library(stringr)
# library(knitr)
 library(dplyr)
 library(ggplot2)
 library(reshape)
# library(ggthemes)

 source('~/Documents/Rwork/Functions/Funciones/functions.R')
 source('~/Documents/Rwork/Functions/Funciones/read.report.R')
# source('~/Documents/Rwork/Functions/multiplot.R')

dir.1<-'~/Documents/Rwork/IFOP/LAm_2019/Estatus1910/norte/'
dir.2<-'~/Documents/ADMwork/IFOP/2018/Lama_model/Consolidado/norte/Lamnor1903/'
dir.3<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/Lamnor1910/'
dir.4<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/Retrospectivo'



unlink(dir.4,recursive=T)
dir.create(file.path('~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/','Retrospectivo'))#crea la carpeta  nuevamente
setwd(dir.2)
file.copy(c('lamnor1903.std', 'lamnor1903.tpl'), dir.4)
setwd(dir.3)
file.copy(c('Lamnor1910.dat', 'LAM', 'LAM_nor1910.std', 'LAM_nor1910.rep'),dir.4) #'lamnor1903.tpl',


# Lectura de datos ####

setwd(dir.4)

std1       <- read.table('lamnor1903.std',header=T,sep='',na='NA',fill=T)  
SSBt1      <- subset(std1,name=='BD')$value
Reclutas1  <- subset(std1,name=='Restim')$value
Ft1       <- subset(std1,name=='log_Fh')$value

dat.file2    = 'Lamnor1910.dat'
dat2        <- lisread(paste(dir.4,dat.file2, sep='/'));
names(dat2) <- str_trim(names(dat2), side='right')
rep2        <- reptoRlist('LAM_nor1910.rep')                                        
std2        <- read.table('LAM_nor1910.std',header=T,sep='',na='NA',fill=T)  


# Arreglo de datos ####

# years1  <- seq(1985,2019,1)
# nyears1 <- length(years1)
# x1      <-c(years1,rev(years1))
# x1_1    <-c(years1[1],years1[nyears1]+1,nyears1+1/2) #xaxp
# x1_2    <-c(years1[1]-1,years1[nyears1]+1) #xlim
years2  <- seq(1985,2019,1)
# nyears2 <- length(years2)
# x2      <-c(years2,rev(years2))
# x2_1    <-c(years2[1],years2[nyears2]+1,nyears2+1/2) #xaxp
# x2_2    <-c(years2[1]-1,years2[nyears2]+1) #xlim


# BIOMASA DESOVANTE oct 2019
SSBt2    <- subset(std2,name=='BD')$value
SSBt2std <- subset(std2,name=='BD')$std
#ssbt2    <- c((SSBt2-1.96*SSBt2std)*10^-3,rev((SSBt2+1.96*SSBt2std)*10^-3))

# RECLUTAMIENTOS
Reclutas2     <- subset(std2,name=='Restim')$value
Reclutas2std  <- subset(std2,name=='Restim')$std
#rt2           <- c((Reclutas2-1.96*Reclutas2std)*10^-3,rev(Reclutas2+1.96*Reclutas2std)*10^-3)

# MORTALIDAD POR PESCA
Ft2     <- subset(std2,name=='log_Fh')$value
Ft2std  <- subset(std2,name=='log_Fh')$std
#ft2     <- c(exp((Ft2)-1.96*(Ft2std)),rev(exp((Ft2)+1.96*(Ft2std)))) 



# Retro Empirico ####
##Figuras Comparativas ####

# png(paste(getwd(),'/Figuras/Fig1_retroempririco.png',sep=""),width=400,height=500)
# par(mfrow=c(3,1),mar=c(2,4,1,1)+0.5)
# plot(x2,ssbt2,type="n",ylim=c(0,max(ssbt2)+1.08),cex.axis=0.8,xaxs="i",yaxs="i",xlim=x2_2,xaxp=x2_1,
#      ylab="Biomasa desovante (t*10^3)",las=1,xlab="A?o",cex.lab=1.1)  
# polygon(x2,ssbt2,col=gray(.8,0.5), border="gray85")
# #lines(years1,SSBt1*10^-3,lwd=2,col='blue2',lty=2)   
# lines(years1,SSBt1*10^-3,lwd=2,col='red2',lty=1)   
# lines(years2,SSBt2*10^-3,lwd=2,col='black',lty=1)  
# legend(1985, 4.5, c('2017','sept 2018'),lty=c(2,1,1),col=c('red2','black'),
#        title="Asesor?a",bty="n",lwd=2)
# text(2016, 4.5, 'a)',cex=1.2)
# 
# plot(x2,rt2 , type="n", xaxp=x2_1, cex.axis=0.8, xaxs= "i", yaxs= "i",
#      xlim=x2_2, ylim = c(0,max(rt2)+0.14), ylab="Reclutamientos x 10^9",las=1,xlab="A?o", cex.lab=1.1)
# polygon(x2, rt2 , col=gray(0.8, 0.5), border= 'gray85')
# #lines(years1, Reclutas0*10^-3, lwd=2, col = 'blue2', lty=2)
# lines(years1, Reclutas1*10^-3, lwd=2, col = 'red2', lty=1)
# lines(years2, Reclutas2*10^-3, lwd=2, col = 'black', lty=1)
# text(2016, 0.5,'b)', cex=1.2)
# 
# plot(x2, ft2, xaxp=x2_1, cex.axis=0.8, xaxs= "i", yaxs= "i", ylim=c(0,max(rt2)+0.54),
#      xlim=x2_2, type="n", ylab="Mortalidad por pesca (F)", las=1, xlab="A?o", cex.lab=1.1)
# polygon(x2, ft2,  col=gray(0.8, 0.5), border="gray85")
# #lines(years1, exp(Ft0), lwd=2, col='blue2', lty=2)
# lines(years1, exp(Ft1), lwd=2, col='red2',  lty=1)
# lines(years2, exp(Ft2), lwd=2, col='black', lty=1)
# text(2016, 0.8, "c)", cex=1.2)
# dev.off()


# Retro Tradicional ####

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
	writeData(paste('LAM','s',i,'.dat',sep=''), data.1, append=F)}

#___________________________________________________________________________________________

# Corre todos los dat

run<-rbind('./LAM -ind $1.dat -r',' cp LAM.rep $1.rep','cp LAM.std $1.std')
cat(run,file = (can <- file('run.sh','wb',encoding='UTF-8')),sep='\n')
close(can)

n <- 5
casos <- rep(NA,n)
s     <- seq(1,n,1)
for(i in 1:n){
  casos[i]<-paste('./run.sh LAMs',s[i],sep='')
  }
cat(casos,file=(can<-file('casos.sh','wb',encoding='UTF-8')),sep='\n');
close(can)

system("chmod 755 run.sh")
system("bash ./casos.sh") 



# GRAFICO RETROSPECTIVO ####


retroR <- retroBD  <- retroF  <- matrix(nrow=length(years2),ncol=length(retros))

for(i in 1:length(retros)){
  rep            <-reptoRlist(paste('LAMs', i, '.rep',sep=''))
  retroBD[,i]  <-c(rep$BD, rep(0,i-1))
  retroR[,i]   <-c(rep$Rech_pre_est[2,], rep(0,i-1))
  retroF[,i]   <-c(rep$Fm_Fh[2,], rep(0,i-1))
}
retroBD[retroBD == 0] <- NA
retroR[retroR == 0] <- NA
retroF[retroF == 0] <- NA


rbd <- as.data.frame(retroBD) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
  mutate(type='B desovante') %>% mutate (Periodo=c(rep('t',35),rep('t-1',35),rep('t-2',35),rep('t-3',35),rep('t-4',35))) 
rbR <- as.data.frame(retroR) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
  mutate(type='Reclutas') %>% mutate (Periodo=c(rep('t',35),rep('t-1',35),rep('t-2',35),rep('t-3',35),rep('t-4',35))) 
rbF <- as.data.frame(retroF) %>% mutate(year=years2) %>% melt(id.vars='year') %>%
  mutate(type='Mortalidad por pesca') %>% mutate (Periodo=c(rep('t',35),rep('t-1',35),rep('t-2',35),rep('t-3',35),rep('t-4',35))) 

matretro <- rbind(rbd,rbR,rbF)
names(matretro)


figretro <- ggplot(matretro) + geom_line(aes(x = year, y = value, color = Periodo)) + 
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(years2), max(years2), by = 2),1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  
  facet_wrap(~type, dir='v',scale='free') + labs(x = 'año', y = '') 


setwd(dir.1)

op<-par(no.readonly=TRUE)
ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
postscript("Figs_Retro/Fig1.eps",height=5, width=7.5) 
figretro
dev.off()





# ANÁLISIS DE PATRONES RETROSPECTIVOS ####


#png(paste(getwd(), 'Fig3_Retrospectivo2.png',sep ="/"),width=350,height=520)
#figretro
#dev.off()


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


