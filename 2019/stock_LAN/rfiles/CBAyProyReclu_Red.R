# Funciones y Directorios ####

rm(list=ls())

 library(stringr)
 library(ggplot2)
 library(reshape)
 library(ggpubr)

 source('~/Documents/Rwork/Functions/Funciones/functions.R')
 source('~/Documents/Rwork/Functions/Funciones/read.report.R')

 dir.1 <-'~/Documents/Rwork/IFOP/Lam_2019/Estatus1910/norte/'
 dir.2<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/Lamnor1910/'
 dir.3<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/CBA_Proy_Red'



 # Estimación CBA #### 
 
#Tabla 1: CBA 2019 Octubre 2019
 
setwd(dir.2) # Directorio de Modelo Base !!!! 
dir()

std1        <- read.table('LAM_nor1910.std', header=T, sep="", na="NA", fill=T)

r    <- seq(0.1,0.5,0.1) # niveles de riesgo (cuantiles)                                
nr   <- length(r)                                                                                   
CBA  <- matrix(ncol=nr)
 
CBAp    <-subset(std1,name=='CBA')$value[3]
CBApstd <-subset(std1,name=='CBA')$std[3]

 for(j in 1:nr){	
   CBA[j]<-qnorm(r[j],CBAp,CBApstd)
   }
 
 tCBA <- round(cbind(CBAp,CBApstd,CBA),0)
 colnames(tCBA) <- c("mean","std",seq(10,50,10))
 tCBA
 
setwd(dir.1)
write.table(tCBA, 'CBA.txt', append = FALSE, sep = " ", dec = ".",
             row.names = TRUE, col.names = TRUE)
 
 

# Seguir con proyecciones para el informe
 
 
# Proyección CBA bajo distintos F ####

unlink(dir.3,recursive=T)
dir.create(file.path('~/Documents/ADMwork/IFOP/2019/Lama_model/Estatus_1910/norte/','CBA_Proy_Red'))
setwd(dir.2);file.copy(c('lamnor1910.dat', 'LAM_nor1910.rep','LAM'), dir.3)
#setwd(dir.5);file.copy(c('lamnor1903.rep','lamnor1903'), dir.2)
# setwd(dir.2)
# system('mv lamnor1809s6.dat lamnor1903.dat')

setwd(dir.3)
data        <- lisread('lamnor1910.dat')
names(data) <- str_trim(names(data), side="right")
dat         <- data


# EXPLORATORIO DE Fsq ####

setwd(dir.3)
system('cat LAM_nor1910.rep')
# 0.4894


# Tasa_bdpr <- c(0.3,0.4,0.45,c(seq(0.65,0.74,0.01)))
# l_npbr <- length(Tasa_bdpr)
# dat$npbr   <- l_npbr
# dat$Tasa_bdpr <- Tasa_bdpr
# writeData(paste("lamnor1910s",1,'.dat', sep=''), dat, append=FALSE)
# 
# #system('mv lamnor1903s1.dat lamnor1903.dat')
# system('./LAM -ind lamnor1910s1.dat')
# system('./LAM -ind lamnor1910.dat')

# Tasa_bdpr <- c(0.3,0.4,0.45,c(seq(0.78,0.79,0.001)))
# l_npbr <- length(Tasa_bdpr)
# dat$npbr   <- l_npbr
# dat$Tasa_bdpr <- Tasa_bdpr
# writeData(paste("lamnor1910s",2,'.dat', sep=''), dat, append=FALSE)
# system('./LAM -ind lamnor1910s2.dat')
# system('cat LAM.rep')
# 
# Tasa_bdpr <- c(0.3,0.4,0.45,c(seq(0.79,0.81,0.001)))
# l_npbr <- length(Tasa_bdpr)
# dat$npbr   <- l_npbr
# dat$Tasa_bdpr <- Tasa_bdpr
# writeData(paste("lamnor1910s",3,'.dat', sep=''), dat, append=FALSE)
# system('./LAM -ind lamnor1910s3.dat')
# system('cat LAM.rep')

setwd(dir.3)
system('./LAM -ind lamnor1910.dat')

# Run Model con NUMERO Y TASA DE PROYECCIÓN PBRs ESCENARIOS ####
setwd(dir.3)

# escenario 1: Ro*1
Tasa_bdpr <- c(0.3,0.4,0.45, 0.4894)
l_npbr <- length(Tasa_bdpr)
dat$npbr   <- l_npbr
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste("LAMs",1,'.dat', sep=''), dat, append=FALSE)


# escenario 2: Ro/2
dat$pry_nR <- 0.5
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste('LAMs',2,'.dat', sep=''), dat, append=FALSE)

# escenario 3: Ro*1.5
dat$pry_nR <- 1.5
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste('LAMs',3,".dat",sep=""), dat, append=FALSE)


# Corre Escenarios ####

setwd(dir.3)
run <- rbind("./LAM -ind  $1.dat -r","cp LAM.rep $1.rep","cp LAM.std $1.std")
cat(run, file = (can <- file("run.sh","wb",encoding="UTF-8")),sep="\n")
close(can)

n<-3
casos <- rep(NA,n)
s     <- seq(1,n,1)
for(i in 1:n){
  casos[i]<-paste("./run.sh LAMs",s[i],sep="")
}
cat(casos,file=(can<-file("casos.sh","wb",encoding="UTF-8")),sep="\n")
close(can)

system("chmod 755 run.sh")
system("bash ./casos.sh")


# Lee reportes escenarios ####

setwd(dir.3)
yrs  <- seq(2019,2028,1)
nyr  <- length(yrs)


rep1      <-reptoRlist('LAMs1.rep')   
rep2      <-reptoRlist('LAMs2.rep')   
rep3      <-reptoRlist('LAMs3.rep')   

Brms <- rep1$BDoLP*0.4

BD_Proy <- data.frame(cbind(yrs,rep1$BD_proy[1:10,]))
colnames(BD_Proy) <- c('yrs', 'F30', 'F40', 'F45', 'Fsq')

C_Proy <- data.frame(cbind(yrs,rep1$C_proy[1:10,]))
colnames(C_Proy) <- c('yrs', 'F30', 'F40', 'F45', 'Fsq')



# Graficos ####

setwd(dir.1)

# BD Proyectada
p1 <- ggplot(BD_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = F40, colour = 'F40', linetype = 'F40')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  geom_line(aes(y = c(rep(Brms,10)), colour = 'Brms', linetype = 'Brms')) +
  
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3', 'black'),
                     limits = c('F45', 'F40',  'Fsq', 'Brms'),
                     breaks = c('F45', 'F40',  'Fsq', 'Brms')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash', 'dotted'),
                        limits = c('F45', 'F40', 'Fsq', 'Brms'),
                        breaks = c('F45', 'F40', 'Fsq', 'Brms')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2029, by = 1),1)) +
  ylab('Biomasa Desovante (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1
ggexport(p1, filename = "Figs_Proyec_Red/Fig1.pdf", width=7.5, height=5, dpi=300)

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("Figs_Proyec/Fig1.eps",height=5, width=7.5) 
# p1
dev.off()


# Captura Proyectada
p2 <- ggplot(C_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = F40, colour = 'F40', linetype = 'F40')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3'),
                     limits = c('F45', 'F40',  'Fsq'),
                     breaks = c('F45', 'F40',  'Fsq')) +
  
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'F40', 'Fsq'),
                        breaks = c('F45', 'F40', 'Fsq')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2028, by = 2),1)) +
  ylab('Captura (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2
ggexport(p2, filename = "Figs_Proyec_Red/Fig2.pdf", width=7.5, height=5, dpi=300)

dev.off()


# Escenarios Reclutamientos ####

outBD2 <- data.frame(rbind(rep1$BD_proy[1:10,],rep2$BD_proy[1:10,],rep3$BD_proy[1:10,]))
colnames(outBD2) <- c('F30', 'F40','F45','Fsq')
outYP2 <- data.frame(rbind(rep1$C_proy[2:11,],rep2$C_proy[2:11,],rep3$C_proy[2:11,]))
colnames(outYP2) <- c('F30', 'F40','F45','Fsq')


Bd2 <- as.data.frame(outBD2[,2:4]) %>% mutate (year=rep(yrs,3)) %>% 
  mutate(Rec=c(rep('Rmed',each=10),rep('Rmed / 2',each=10),rep('Rmed x 1.5',each=10))) %>% 
  melt(id.vars=c('year','Rec'))

Yp2 <- as.data.frame(outYP2[,2:4]) %>% mutate (year=rep(yrs,3)) %>% 
  mutate(Rec=c(rep('Rmed',each=10),rep('Rmed / 2',each=10), rep('Rmed x 1.5',each=10))) %>% 
  melt(id.vars=c('year','Rec'))






figBD  <- ggplot(Bd2) + aes(x = yrs) + 
  geom_line(aes(x=year, y=value, color = variable, linetype = variable)) +
  
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3'),
                     limits = c('F45', 'F40',  'Fsq'),
                     breaks = c('F45', 'F40',  'Fsq')) +
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'F40', 'Fsq'),
                        breaks = c('F45', 'F40', 'Fsq')) +

  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2028, by = 1),1)) +
  ylab('Biomasa desovante proyectada (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) +

  facet_wrap(~Rec, dir='v') 

figBD



figYp  <- ggplot(Yp2) + aes(x = yrs) + 
  geom_line(aes(x=year, y=value, color = variable, linetype = variable)) +
  
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3'),
                     limits = c('F45', 'F40',  'Fsq'),
                     breaks = c('F45', 'F40',  'Fsq')) +
  
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'F40', 'Fsq'),
                        breaks = c('F45', 'F40', 'Fsq')) +

  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2028, by = 1),1)) +
  ylab('Captura proyectada (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) +
  
  facet_wrap(~Rec, dir='v') 

  figYp


p3 <-  ggarrange(figBD, figYp, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggexport(p3, filename = "Figs_Proyec_Red/Fig3.pdf", width=8, height=6.5, dpi=300)

