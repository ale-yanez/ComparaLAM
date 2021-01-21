# Funciones y Directorios ####
library(rstudioapi)
library(ggplot2)
library(reshape)
library(ggpubr)
library(devtools)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

#out1 <- read.admb("../LAM_nor2008")
baserep <- read.admb("~/Documents/ADMwork/IFOP/2019/Lama_model/Cons_2003/norte/Lamnor2003/LAM_nor2003")
basestd <- read.table('~/Documents/ADMwork/IFOP/2019/Lama_model/Cons_2003/norte/Lamnor2003/LAM_nor2003.std', header=T, sep="", na="NA", fill=T)

Coqstd      <- read.table("~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Sens/IndCoq_2008/LAmN.std", header=T, sep="", na="NA", fill=T)
OjFisiostd  <- read.table("~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Sens/ojivas_2008/LAmN_Fisio.std", header=T, sep="", na="NA", fill=T)
OjFunstd    <- read.table("~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Sens/ojivas_2008/LAmN_Fun.std", header=T, sep="", na="NA", fill=T)
Franstd     <- read.table("~/Documents/ADMwork/IFOP/2019/Lama_model/Cons_2003/norte/A_Sens/A_Sens_Francis/LAM.std", header=T, sep="", na="NA", fill=T)
FullCoqstd  <- read.table("~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Sens/FullCoq_2008/LAmN.std", header=T, sep="", na="NA", fill=T)

# Para graficar ... ####
 yrs <- baserep$YRS
 #yrs <- head(baserep$YRS, -1)
 M <- 0.3
 Brms <- baserep$BDoLP*0.4
 Frms <- baserep$Fpbr[3]

# #predichos y estimados 
 Rec_base                 <- subset(basestd    ,name=='Restim')$value
 Rec_Coq                  <- subset(Coqstd     ,name=='Restim')$value
 Rec_OjFisio              <- subset(OjFisiostd ,name=='Restim')$value
 Rec_OjFun                <- subset(OjFunstd   ,name=='Restim')$value
 Rec_Fran                 <- subset(Franstd    ,name=='Restim')$value
 Rec_FullCoq              <- subset(FullCoqstd ,name=='Restim')$value
 
 desvRec_base             <- subset(basestd    ,name=='log_dev_Ro')$value
 desvRec_Coq              <- subset(Coqstd     ,name=='log_dev_Ro')$value
 desvRec_OjFisio          <- subset(OjFisiostd ,name=='log_dev_Ro')$value
 desvRec_OjFun            <- subset(OjFunstd   ,name=='log_dev_Ro')$value
 desvRec_Fran             <- subset(Franstd    ,name=='dev_log_Ro')$value
 desvRec_FullCoq          <- subset(FullCoqstd ,name=='log_dev_Ro')$value
 
 BT_base                  <- subset(basestd    ,name=='BT')$value
 BT_Coq                   <- subset(Coqstd     ,name=='BT')$value
 BT_OjFisio               <- subset(OjFisiostd ,name=='BT')$value
 BT_OjFun                 <- subset(OjFunstd   ,name=='BT')$value
 BT_Fran                  <- subset(Franstd    ,name=='BT')$value
 BT_FullCoq               <- subset(FullCoqstd ,name=='BT')$value
 
 BD_base                  <- subset(basestd    ,name=='BD')$value
 BD_Coq                   <- subset(Coqstd     ,name=='BD')$value
 BD_OjFisio               <- subset(OjFisiostd ,name=='BD')$value
 BD_OjFun                 <- subset(OjFunstd   ,name=='BD')$value
 BD_Fran                  <- subset(Franstd    ,name=='BD')$value
 BD_FullCoq               <- subset(FullCoqstd ,name=='BD')$value
 
 F_base               <- exp(subset(basestd    ,name=='log_Fh')$value)
 F_Coq                <- exp(subset(Coqstd     ,name=='log_Fh')$value)
 F_OjFisio            <- exp(subset(OjFisiostd ,name=='log_Fh')$value)
 F_OjFun              <- exp(subset(OjFunstd   ,name=='log_Fh')$value)
 F_Fran               <- exp(subset(Franstd    ,name=='log_Fh')$value)
 F_FullCoq            <- exp(subset(FullCoqstd ,name=='log_Fh')$value)
 
# # std 
 stdRec_base       <- subset(basestd,name=='Restim')$std
 stddesvRec_base   <- subset(basestd,name=='log_dev_Ro')$std
 stdBT_base        <- subset(basestd,name=='BT')$std
 stdBD_base        <- subset(basestd,name=='BD')$std
 stdF_base         <- subset(basestd,name=='log_Fh')$std
 
 
# # Confidence Intervals
 rec1_lwr      <-Rec_base-1.96*stdRec_base
 rec1_upr      <-Rec_base+1.96*stdRec_base
 
 desvrec1_lwr  <- desvRec_base-1.96*stddesvRec_base
 desvrec1_upr  <- desvRec_base+1.96*stddesvRec_base
 
 BT1_lwr       <-BT_base-1.96*stdBT_base
 BT1_upr       <-BT_base+1.96*stdBT_base
 
 BD1_lwr       <-BD_base-1.96*stdBD_base
 BD1_upr       <-BD_base+1.96*stdBD_base
 
 F1_lwr        <-exp(log(F_base)-1.96*stdF_base)
 F1_upr        <-exp(log(F_base)+1.96*stdF_base)
 

#Var Pop LAM MODEL ###

# Reclutamiento ####

p8 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(Rec_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(Rec_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(Rec_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(1,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA

   scale_color_manual(name = '',
                      values = c('royalblue3', 'darkorange2', 'deepskyblue2','gold2', 'purple2'),
                      limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                      breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) +
   scale_linetype_manual(name = '',
                         values = c('solid','solid','solid','solid','solid'),
                         limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                         breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) 

p8 <- p8 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p8

ggsave(p8, filename = "VarPop_Rec.png", width=9, height=4.5, dpi=300)

# Biomasa Total ####

p10 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BT_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(BT_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(BT_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(5000,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
 
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2'),
                     limits = c('base','Coquimbo','Funcional','Francis'),
                     breaks = c('base','Coquimbo','Funcional','Francis')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid'),
                        limits = c('base','Coquimbo','Funcional','Francis'),
                        breaks = c('base','Coquimbo','Funcional','Francis'))
  
p10 <- p10 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'none') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p10

ggsave(p10, filename = "VarPop_BT.png", width=9, height=4.5, dpi=300)

# Biomasa Desovante ####

p11 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BD_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(BD_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(BD_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(Brms,35)), colour = 'Brms', linetype = 'Brms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  annotate("text", x=1986, y=1600, label="Brms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2', 'chartreuse'),
                     limits = c('base','Coquimbo','Funcional','Francis', 'Brms'),
                     breaks = c('base','Coquimbo','Funcional','Francis', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid', 'twodash'),
                        limits = c('base','Coquimbo','Funcional','Francis', 'Brms'),
                        breaks = c('base','Coquimbo','Funcional','Francis', 'Brms'))

p11 <- p11 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p11

ggsave(p11, filename = "VarPop_BD.png", width=9, height=4.5, dpi=300)

 plot_B <- ggarrange(p10, p11, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
 ggsave(plot_B, filename = "VarPop2_Biom.png", width=8, height=6.5, dpi=300)


# Mortalidad por Pesca ####

p12 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(F_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(F_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(F_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(Frms,35)), colour = 'Frms', linetype = 'Frms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
   annotate("text", x=1986, y=0.24, label="Frms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2', 'red'),
                     limits = c('base','Coquimbo','Funcional','Francis', 'Frms'),
                     breaks = c('base','Coquimbo','Funcional','Francis', 'Frms')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid', 'twodash'),
                        limits = c('base','Coquimbo','Funcional','Francis', 'Frms'),
                        breaks = c('base','Coquimbo','Funcional','Francis', 'Frms'))

p12 <- p12 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p12

ggsave(p12, filename = "VarPop_Fh.png", width=9, height=4.5, dpi=300)

plot_RF <- ggarrange(p8, p12, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_RF, filename = "VarPop2_RecF.png", width=8, height=6.5, dpi=300)

#




#Var Pop LAM MODEL con Fisio ###


# Agregando Madurez Fisiologica ... 
# Reclutamiento ####

p1 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(Rec_OjFisio), colour = 'Fisiologica', linetype = 'Fisiologica')) +
  geom_line(aes(y = c(Rec_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(Rec_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(Rec_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(1,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2','gold2', 'purple2'),
                     limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                     breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid','solid'),
                        limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                        breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) 

p1 <- p1 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p1

ggsave(p1, filename = "VarPop_RecFisio.png", width=9, height=4.5, dpi=300)

# Biomasa Total ####

p2 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BT_OjFisio), colour = 'Fisiologica', linetype = 'Fisiologica')) +
  geom_line(aes(y = c(BT_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(BT_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(BT_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(5000,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2','gold2', 'purple2'),
                     limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                     breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid','solid'),
                        limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                        breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) 

p2 <- p2 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'none') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p2

ggsave(p2, filename = "VarPop_BT.png", width=9, height=4.5, dpi=300)

# Biomasa Desovante ####

p3 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BD_OjFisio), colour = 'Fisiologica', linetype = 'Fisiologica')) +
  geom_line(aes(y = c(BD_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(BD_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(BD_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(Brms,35)), colour = 'Brms', linetype = 'Brms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  annotate("text", x=1986, y=1600, label="Brms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2','gold2', 'purple2'),
                     limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                     breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid','solid'),
                        limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                        breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) 

p3 <- p3 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p3

ggsave(p3, filename = "VarPop_BD.png", width=9, height=4.5, dpi=300)

plot_BFisio <- ggarrange(p2, p3, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_BFisio, filename = "VarPop2_BiomFisio.png", width=8, height=6.5, dpi=300)


# Mortalidad por Pesca ####

p4 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(F_OjFisio), colour = 'Fisiologica', linetype = 'Fisiologica')) +
  geom_line(aes(y = c(F_OjFun), colour = 'Funcional', linetype = 'Funcional')) +
  geom_line(aes(y = c(F_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  geom_line(aes(y = c(F_Fran), colour = 'Francis', linetype = 'Francis')) +
  geom_line(aes(y = c(rep(Frms,35)), colour = 'Frms', linetype = 'Frms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  annotate("text", x=1986, y=0.24, label="Frms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2','gold2', 'purple2'),
                     limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                     breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid','solid'),
                        limits = c('base','Fisiologica','Funcional','Coquimbo','Francis'),
                        breaks = c('base','Fisiologica','Funcional','Coquimbo','Francis')) 

p4 <- p4 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p4

ggsave(p4, filename = "VarPop_Fh.png", width=9, height=4.5, dpi=300)

plot_RF_Fisio <- ggarrange(p1, p4, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_RF_Fisio, filename = "VarPop2_RecF_Fisio.png", width=8, height=6.5, dpi=300)

#



# Agregando Full Cquimbo ... 
# Reclutamiento ####

p1_C <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(Rec_FullCoq), colour = 'Full_Cqbo', linetype = 'Full_Cqbo')) +
  geom_line(aes(y = c(Rec_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  #geom_line(aes(y = c(rep(1,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'tomato2', 'gold2'),
                     limits = c('base', 'Full_Cqbo','Coquimbo'),
                     breaks = c('base','Full_Cqbo','Coquimbo')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'solid','solid'),
                        limits = c('base', 'Full_Cqbo','Coquimbo'),
                        breaks = c('base', 'Full_Cqbo','Coquimbo')) 

p1_C <- p1_C + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p1_C

#ggsave(p1_C, filename = "VarPop_Rec_FullCqbo.png", width=9, height=4.5, dpi=300)

# Mortalidad por Pesca ####

p2_C <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(F_FullCoq), colour = 'Full_Cqbo', linetype = 'Full_Cqbo')) +
  geom_line(aes(y = c(F_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) + 
  #geom_line(aes(y = c(rep(Frms,35)), colour = 'Frms', linetype = 'Frms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  #annotate("text", x=1986, y=0.24, label="Frms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'tomato2', 'gold2'),
                     limits = c('base', 'Full_Cqbo','Coquimbo'),
                     breaks = c('base','Full_Cqbo','Coquimbo')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'solid','solid'),
                        limits = c('base', 'Full_Cqbo','Coquimbo'),
                        breaks = c('base', 'Full_Cqbo','Coquimbo')) 


p2_C <- p2_C + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p2_C

#ggsave(p2_C, filename = "VarPop_Fh_FullCqbo.png", width=9, height=4.5, dpi=300)

plot_RF_Cqbo <- ggarrange(p1_C, p2_C, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_RF_Cqbo, filename = "VarPop_RF_Cqbo.png", width=8, height=6.5, dpi=300)

#


# Biomasa Total ####

p3_C <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BT_FullCoq), colour = 'Full_Cqbo', linetype = 'Full_Cqbo')) +
  geom_line(aes(y = c(BT_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  #geom_line(aes(y = c(rep(5000,35)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'tomato2', 'gold2'),
                     limits = c('base', 'Full_Cqbo','Coquimbo'),
                     breaks = c('base','Full_Cqbo','Coquimbo')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'solid','solid'),
                        limits = c('base', 'Full_Cqbo','Coquimbo'),
                        breaks = c('base', 'Full_Cqbo','Coquimbo')) 

p3_C <- p3_C + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'none') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p3_C

#ggsave(p3_C, filename = "VarPop_BT_Cqbo.png", width=9, height=4.5, dpi=300)

# Biomasa Desovante ####

p4_C <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_base, colour = 'base', linetype = 'base'), size = 1.2) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.4) + #fill = 'grey37'
  geom_line(aes(y = c(BD_FullCoq), colour = 'Full_Cqbo', linetype = 'Full_Cqbo')) +
  geom_line(aes(y = c(BD_Coq), colour = 'Coquimbo', linetype = 'Coquimbo')) +
  #geom_line(aes(y = c(rep(Brms,35)), colour = 'Brms', linetype = 'Brms')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA
  #annotate("text", x=1986, y=1600, label="Brms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'tomato2', 'gold2'),
                     limits = c('base', 'Full_Cqbo','Coquimbo'),
                     breaks = c('base','Full_Cqbo','Coquimbo')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'solid','solid'),
                        limits = c('base', 'Full_Cqbo','Coquimbo'),
                        breaks = c('base', 'Full_Cqbo','Coquimbo')) 


p4_C <- p4_C + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p4_C

#ggsave(p4_C, filename = "VarPop_BD.png", width=9, height=4.5, dpi=300)

plot_BCqbo <- ggarrange(p3_C, p4_C, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_BCqbo, filename = "VarPop_BiomCqbo.png", width=8, height=6.5, dpi=300)

