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

out20 <- read.admb("../2020/stock_LAN/output/base")
std20 <- read.table("../2020/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out19 <- read.admb("../2019/stock_LAN/output/base")
std19 <- read.table("../2019/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out18 <- read.admb("../2018/stock_LAN/output/base")
std18 <- read.table("../2018/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out17 <- read.admb("../2017/stock_LAN/output/base")
std17 <- read.table("../2017/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)


# Para graficar ... ####
 yrs <- out20$YRS
 #yrs <- head(baserep$YRS, -1)
 M <- 0.3
 #Brms <- baserep$BDoLP*0.4
 #Frms <- baserep$Fpbr[3]

# #predichos y estimados 
 
 BT_20                  <- subset(std20 ,name=='BT')$value
 BT_19                  <- subset(std19 ,name=='BT')$value
 BT_18                  <- subset(std18 ,name=='BT')$value
 BT_17                  <- subset(std17 ,name=='BT')$value

 BD_20                  <- subset(std20 ,name=='BD')$value
 BD_19                  <- subset(std19 ,name=='BD')$value
 BD_18                  <- subset(std18 ,name=='BD')$value
 BD_17                  <- subset(std17 ,name=='BD')$value
 
 Rec_20                 <- subset(std20 ,name=='Restim')$value
 Rec_19                 <- subset(std19 ,name=='Restim')$value
 Rec_18                 <- subset(std18 ,name=='Restim')$value
 Rec_17                 <- subset(std17 ,name=='Restim')$value
 
 F_20                   <- exp(subset(std20 ,name=='log_Fh')$value)
 F_19                   <- exp(subset(std19 ,name=='log_Fh')$value)
 F_18                   <- exp(subset(std18 ,name=='log_Fh')$value)
 F_17                   <- exp(subset(std17 ,name=='log_Fh')$value)
 
 
# # std 
 stdRec_20       <- subset(std20,name=='Restim')$std
 stdBT_20        <- subset(std20,name=='BT')$std
 stdBD_20        <- subset(std20,name=='BD')$std
 stdF_20         <- subset(std20,name=='log_Fh')$std
 
 
# # Confidence Intervals
 rec1_lwr      <-Rec_20-1.96*stdRec_20
 rec1_upr      <-Rec_20+1.96*stdRec_20
 
 BT1_lwr       <-BT_20-1.96*stdBT_20
 BT1_upr       <-BT_20+1.96*stdBT_20
 
 BD1_lwr       <-BD_20-1.96*stdBD_20
 BD1_upr       <-BD_20+1.96*stdBD_20
 
 F1_lwr        <-exp(log(F_20)-1.96*stdF_20)
 F1_upr        <-exp(log(F_20)+1.96*stdF_20)
 

#Var Pop LAM MODEL ###

# Reclutamiento ####

p8 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_20, colour = '2020', linetype = '2020')) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey60', alpha = 0.3) + #fill = 'grey37'
  geom_line(aes(y = c(Rec_19,NA), colour = '2019', linetype = '2019')) +
  geom_line(aes(y = c(Rec_18, NA, NA), colour = '2018', linetype = '2018')) +
  geom_line(aes(y = c(Rec_17, NA, NA, NA), colour = '2017', linetype = '2017')) +
  geom_line(aes(y = c(rep(1,36)), colour = 'Brms', linetype = '')) + #EN CASO DE QUERER UNIR CON BD MANTENGO ESTA LINEA

   scale_color_manual(name = '',
                      values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2'),
                      limits = c('2020','2019','2018','2017'),
                      breaks = c('2020','2019','2018','2017')) +
   scale_linetype_manual(name = '',
                         values = c('solid','solid','solid','solid'),
                         limits = c('2020','2019','2018','2017'),
                         breaks = c('2020','2019','2018','2017')) 

p8 <- p8 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p8

ggsave(p8, filename = "../figures/LAN/figure_11.png", width=9, height=4.5, dpi=300)
dir()

# Mortalidad por Pesca ####

p12 <-  ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_20, colour = '2020', linetype = '2020')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.3) + #fill = 'grey37'
  geom_line(aes(y = c(F_19,NA), colour = '2019', linetype = '2019')) +
  geom_line(aes(y = c(F_18, NA, NA), colour = '2018', linetype = '2018')) +
  geom_line(aes(y = c(F_17, NA, NA, NA), colour = '2017', linetype = '2017')) +
  geom_line(aes(y = c(rep(1,36)), colour = 'Brms', linetype = '')) +
  
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2'),
                     limits = c('2020','2019','2018','2017'),
                     breaks = c('2020','2019','2018','2017')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid'),
                        limits = c('2020','2019','2018','2017'),
                        breaks = c('2020','2019','2018','2017'))

p12 <- p12 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p12

ggsave(p12, filename = "../figures/LAN/figure_12.png", width=9, height=4.5, dpi=300)

plot_RF <- ggarrange(p8, p12, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
ggsave(plot_RF, filename = "../figures/LAN/figure_13.png", width=8, height=6.5, dpi=300)



# Biomasa Total ####

p14 <-  ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_20, colour = '2020', linetype = '2020')) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey60', alpha = 0.3) + #fill = 'grey37'
  geom_line(aes(y = c(BT_19,NA), colour = '2019', linetype = '2019')) +
  geom_line(aes(y = c(BT_18, NA, NA), colour = '2018', linetype = '2018')) +
  geom_line(aes(y = c(BT_17, NA, NA, NA), colour = '2017', linetype = '2017')) +
  geom_line(aes(y = c(rep(1,36)), colour = 'Brms', linetype = '')) +
  
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2'),
                     limits = c('2020','2019','2018','2017'),
                     breaks = c('2020','2019','2018','2017')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid'),
                        limits = c('2020','2019','2018','2017'),
                        breaks = c('2020','2019','2018','2017'))

p14 <- p14 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p14

ggsave(p14, filename = "../figures/LAN/figure_14.png", width=9, height=4.5, dpi=300)


# Biomasa Desovante ####

p15 <-  ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_20, colour = '2020', linetype = '2020')) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.3) + #fill = 'grey37'
  geom_line(aes(y = c(BD_19,NA), colour = '2019', linetype = '2019')) +
  geom_line(aes(y = c(BD_18, NA, NA), colour = '2018', linetype = '2018')) +
  geom_line(aes(y = c(BD_17, NA, NA, NA), colour = '2017', linetype = '2017')) +
  geom_line(aes(y = c(rep(1,36)), colour = 'Brms', linetype = '')) +
  
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'darkorange2', 'deepskyblue2', 'purple2'),
                     limits = c('2020','2019','2018','2017'),
                     breaks = c('2020','2019','2018','2017')) +
  scale_linetype_manual(name = '',
                        values = c('solid','solid','solid','solid'),
                        limits = c('2020','2019','2018','2017'),
                        breaks = c('2020','2019','2018','2017'))

p15 <- p15 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p15

ggsave(p15, filename = "../figures/LAN/figure_15.png", width=9, height=4.5, dpi=300)

 plot_B <- ggarrange(p14, p15, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "bottom")
 ggsave(plot_B, filename = "../figures/LAN/figure_16.png", width=8, height=6.5, dpi=300)

