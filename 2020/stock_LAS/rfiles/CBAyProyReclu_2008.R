# Funciones y Directorios ####

rm(list=ls())

 library(stringr)
 library(ggplot2)
# library(reshape)
 library(ggpubr)

source('~/Documents/Rwork/Functions/Funciones/functions.R')
source('~/Documents/Rwork/Functions/Funciones/read.report.R')

dir.1 <-'~/Documents/Rwork/IFOP/Lam_2020/Estatus_2008/sur'
dir.2<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/sur/Lamsur2008/'
dir.3<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/sur/CBA_Proy/'


  # Estimación CBA #### 
  
#Tabla 1: CBA Agosto 2020
setwd(dir.2) # Directorio de Modelo Base !!!! 
dir()

std1        <- read.table('LAM_sur2008.std', header=T, sep="", na="NA", fill=T)
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
  write.table(tCBA, 'CBA.txt', append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)
  
 
# Proyección CBA bajo distintos F ####

dir.create(file.path('~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/sur/','CBA_Proy'))
setwd(dir.2);file.copy(c('lamsur2008.dat', 'LAM_sur2008.rep','LAmS'), dir.3)

setwd(dir.3)
data        <- lisread('lamsur2008.dat')
names(data) <- str_trim(names(data), side="right")
dat         <- data


# EXPLORATORIO DE Fsq ####

system('cat LAM_sur2008.rep')
# 0.519027

system('./LAmS -ind lamsur2008.dat')

 # Run Model con NUMERO Y TASA DE PROYECCIÓN PBRs ESCENARIOS ####

# escenario 1: Ro*1
Tasa_bdpr <- c(0.3,0.4,0.45, 0.519027)
l_npbr <- length(Tasa_bdpr)
dat$npbr   <- l_npbr
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste("LAmSs",1,'.dat', sep=''), dat, append=FALSE)

# escenario 2: Ro/2
dat$pry_nR <- 0.5
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste('LAmSs',2,'.dat', sep=''), dat, append=FALSE)

# escenario 3: Ro*1.5
dat$pry_nR <- 1.5
dat$Tasa_bdpr <- Tasa_bdpr
writeData(paste('LAmSs',3,".dat",sep=""), dat, append=FALSE)


# Corre Escenarios ####
run <- rbind("./LAmS -ind  $1.dat -r","cp LAmS.rep $1.rep","cp LAmS.std $1.std")
cat(run, file = (can <- file("run.sh","wb",encoding="UTF-8")),sep="\n")
close(can)

n<-3
casos <- rep(NA,n)
s     <- seq(1,n,1)
for(i in 1:n){
  casos[i]<-paste("./run.sh LAmSs",s[i],sep="")
}
cat(casos,file=(can<-file("casos.sh","wb",encoding="UTF-8")),sep="\n")
close(can)

system("chmod 755 run.sh")
system("bash ./casos.sh")

# Lee reportes escenarios ####

yrs  <- seq(2020,2030,1)
nyr  <- length(yrs)


rep1      <-reptoRlist('LAmSs1.rep')   
rep2      <-reptoRlist('LAmSs2.rep')   
rep3      <-reptoRlist('LAmSs3.rep')   

Brms <- rep1$BDoLP*0.4
Bo <- rep1$BDoLP

BD_Proy <- data.frame(cbind(yrs,rep1$BD_proy[1:11,]))
colnames(BD_Proy) <- c('yrs', 'F30', 'F40', 'F45', 'Fsq')

C_Proy <- data.frame(cbind(yrs,rep1$C_proy[1:11,]))
colnames(C_Proy) <- c('yrs', 'F30', 'F40', 'F45', 'Fsq')



# Graficos ####

setwd(dir.1)
dir.create('Figs_Proyec')

# BD Proyectada

p1 <- ggplot(BD_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = F40, colour = 'F40', linetype = 'F40')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  geom_line(aes(y = c(rep(Brms,11)), colour = 'Brms', linetype = 'Brms')) +
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3', 'black'),
                     limits = c('F45', 'F40',  'Fsq', 'Brms'),
                     breaks = c('F45', 'F40',  'Fsq', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash', 'dotted'),
                        limits = c('F45', 'F40', 'Fsq', 'Brms'),
                        breaks = c('F45', 'F40', 'Fsq', 'Brms')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Biomasa Desovante (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1
ggexport(p1, filename = "Figs_Proyec/Fig1_BD.pdf", width=8, height=4.5, dpi=300)


# BD Proyectada otra opcion

p1_2 <- ggplot(BD_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  geom_line(aes(y = c(rep(Brms,11)), colour = 'Brms', linetype = 'Brms')) +
  #geom_line(aes(y = c(rep(Bo,11)), colour = '', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('green4', 'royalblue3', 'black'),
                     limits = c('F45', 'Fsq', 'Brms'),
                     breaks = c('F45', 'Fsq', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'Fsq', 'Brms'),
                        breaks = c('F45', 'Fsq', 'Brms')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Biomasa Desovante (t)') + #ylim(2000,6000) + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10)) + 
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12)) 

p1_2
ggexport(p1_2, filename = "Figs_Proyec/Fig1_2_1BD.pdf", width=8, height=6.5, dpi=300)
ggexport(p1_2, filename = "Figs_Proyec/Fig1_2_2BD.pdf", width=8, height=6.5, dpi=300)


# BD Proyectada tercera opcion

p1_3 <- ggplot(BD_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  geom_line(aes(y = c(rep(Brms,11)), colour = 'Brms', linetype = 'Brms')) +
  #geom_line(aes(y = c(rep(Bo,11)), colour = '', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('green4', 'royalblue3', 'black'),
                     limits = c('F45', 'Fsq', 'Brms'),
                     breaks = c('F45', 'Fsq', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'Fsq', 'Brms'),
                        breaks = c('F45', 'Fsq', 'Brms')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Biomasa Desovante (t)') + ylim(0,6000) + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10)) + 
  theme(axis.title.x = element_text(size = 12)) + 
  theme(axis.title.y = element_text(size = 12)) 

p1_3
ggexport(p1_3, filename = "Figs_Proyec/Fig1_3BD.pdf", width=8, height=6.5, dpi=300)

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
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Captura (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2
ggexport(p2, filename = "Figs_Proyec/Fig2_catch.pdf", width=8, height=4.5, dpi=300)


# Captura Proyectada otra opcion

p2_2 <- ggplot(C_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  geom_line(aes(y = c(rep(CBA[5],11)), colour = 'CBA', linetype = 'CBA')) +
  scale_color_manual(name = '',
                     values = c('green4', 'red', 'royalblue3'),
                     limits = c('F45', 'CBA',  'Fsq'),
                     breaks = c('F45', 'CBA',  'Fsq')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash',  'twodash'),
                        limits = c('F45', 'CBA', 'Fsq'),
                        breaks = c('F45', 'CBA', 'Fsq')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Captura (t)') + ylim(0, max(C_Proy)) + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=11)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2_2
ggexport(p2_2, filename = "Figs_Proyec/Fig2_2BD.pdf", width=8, height=6.5, dpi=300)


# Captura Proyectada tercera opcion

p2_2 <- ggplot(C_Proy, aes(x = yrs)) + 
  geom_line(aes(y = F45, colour = 'F45', linetype = 'F45')) +
  geom_line(aes(y = Fsq, colour = 'Fsq', linetype = 'Fsq')) + 
  #geom_line(aes(y = c(rep(CBA[5],11)), colour = 'CBA', linetype = 'CBA')) +
  scale_color_manual(name = '',
                     values = c('green4',  'royalblue3'),
                     limits = c('F45',   'Fsq'),
                     breaks = c('F45',  'Fsq')) +
  scale_linetype_manual(name = '',
                        values = c('solid',  'twodash'),
                        limits = c('F45',  'Fsq'),
                        breaks = c('F45',  'Fsq')) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Captura (t)') + #ylim(1000, 4000) + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=11)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2_2
ggexport(p2_2, filename = "Figs_Proyec/Fig2_2_1_CP.pdf", width=8, height=6.5, dpi=300)
ggexport(p2_2, filename = "Figs_Proyec/Fig2_2_2_CP.pdf", width=8, height=6.5, dpi=300)



# Escenarios Reclutamientos ####

outBD2 <- data.frame(rbind(rep1$BD_proy[1:11,],rep2$BD_proy[1:11,],rep3$BD_proy[1:11,]))
colnames(outBD2) <- c('F30', 'F40','F45','Fsq')
outYP2 <- data.frame(rbind(rep1$C_proy[1:11,],rep2$C_proy[1:11,],rep3$C_proy[1:11,]))
colnames(outYP2) <- c('F30', 'F40','F45','Fsq')


Bd2 <- as.data.frame(outBD2[,2:4]) %>% mutate (year=rep(yrs,3)) %>% 
  mutate(Rec=c(rep('Rmed',each=11),rep('Rmed / 2',each=11),rep('Rmed x 1.5',each=11))) %>% 
  melt(id.vars=c('year','Rec'))

Yp2 <- as.data.frame(outYP2[,2:4]) %>% mutate (year=rep(yrs,3)) %>% 
  mutate(Rec=c(rep('Rmed',each=11),rep('Rmed / 2',each=11), rep('Rmed x 1.5',each=11))) %>% 
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

  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Biomasa desovante proyectada (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=7)) +
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

  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2030, by = 1),1)) +
  ylab('Captura proyectada (t)') + # scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=7)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) +
  
  facet_wrap(~Rec, dir='v') 

  figYp

p3 <-  ggarrange(figBD, figYp, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggexport(p3, filename = "Figs_Proyec/Fig3.pdf", width=7, height=5.5, dpi=300)

