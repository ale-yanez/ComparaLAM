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

out1 <- read.admb("../output/base")
std1 <- read.table("../output/base.std", header=T, sep="", na="NA", fill=T)


# Para graficar ... ####
 yrs <- out1$YRS
 M <- 0.3
 Brms <- out1$BDoLP*0.4
 Frms <- out1$Fpbr[3]

# #predichos y estimados 
 Rec_est1      <- subset(std1,name=='Restim')$value
 desvRec1      <- subset(std1,name=='log_dev_Ro')$value
 BT_est1       <- subset(std1,name=='BT')$value
 BD_est1       <- subset(std1,name=='BD')$value
 F_est1        <- exp(subset(std1,name=='log_Fh')$value)
 
# # std 
 stdRec1       <- subset(std1,name=='Restim')$std
 stddesvRec1   <- subset(std1,name=='log_dev_Ro')$std
 stdBT1        <- subset(std1,name=='BT')$std
 stdBD1        <- subset(std1,name=='BD')$std
 stdF1         <- subset(std1,name=='log_Fh')$std

# # Confidence Intervals
 rec1_lwr      <-Rec_est1-1.96*stdRec1
 rec1_upr      <-Rec_est1+1.96*stdRec1
 desvrec1_lwr  <- desvRec1-1.96*stddesvRec1
 desvrec1_upr  <- desvRec1+1.96*stddesvRec1
 BT1_lwr       <-BT_est1-1.96*stdBT1
 BT1_upr       <-BT_est1+1.96*stdBT1
 BD1_lwr       <-BD_est1-1.96*stdBD1
 BD1_upr       <-BD_est1+1.96*stdBD1
 F1_lwr        <-exp(log(F_est1)-1.96*stdF1)
 F1_upr        <-exp(log(F_est1)+1.96*stdF1)
 

#Var Pop LAM MODEL ###

# Reclutamiento ####

p8 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_est1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey60', alpha = 0.4) +
  scale_color_manual(name = '',
                     values = c('royalblue3'),
                     limits = c('actual'),
                     breaks = c('actual')) +
  scale_linetype_manual(name = '',
                        values = c('solid'),
                        limits = c('actual'),
                        breaks = c('actual'))
p8 <- p8 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p8

# Desvíos Reclutamiento ####

p9 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = desvRec1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=desvrec1_lwr, ymax=desvrec1_upr), fill = 'grey60', alpha = 0.4) + 
  geom_line(aes(y = c(rep(0,length(yrs))), colour = '', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'black'),
                     limits = c('actual', ''),
                     breaks = c('actual', '')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'dotted'),
                        limits = c('actual', ''),
                        breaks = c('actual', ''))
p9 <- p9 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Desvíos Reclutamientos') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p9

plot_rec <- ggarrange(p8, p9, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "none")

ggsave(plot_rec, filename = "../figures/figure_1.png", width=7, height=8, dpi=300)

# Biomasa Total ####

p10 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_est1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey60', alpha = 0.4) + 
  geom_line(aes(y = c(rep(1,length(yrs))), colour = 'Brms', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse4'),
                     limits = c('actual', 'Brms'),
                     breaks = c('actual', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash'),
                        limits = c('actual', 'Brms'),
                        breaks = c('actual', 'Brms'))
p10 <- p10 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'none') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p10


# Biomasa Desovante ####

p11 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1, colour = 'actual', linetype = 'actual')) + 
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.4) + 
  geom_line(aes(y = c(rep(Brms,length(yrs))), colour = 'Brms', linetype = 'Brms')) +
  annotate("text", x=1989, y=3500, label="Brms") +

  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse4'),
                     limits = c('actual',  'Brms'),
                     breaks = c('actual',  'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash'),
                        limits = c('actual', 'Brms'),
                        breaks = c('actual', 'Brms'))
p11 <- p11 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'none') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p11

plot_B <- ggarrange(p10, p11, ncol = 1, nrow = 2, align = "v", common.legend = T, legend = "none")
ggsave(plot_B, filename = "../figures/figure_2.png", width=8, height=6.5, dpi=300)


# Mortalidad por Pesca ####

p12 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.4) + 
  geom_line(aes(y = c(rep(M,length(yrs))), colour = 'M', linetype = 'M')) +
  geom_line(aes(y = c(rep(Frms,length(yrs))), colour = 'Frms', linetype = 'Frms')) +
  annotate("text", x=2014, y=0.42, label="M") +
  annotate("text", x=2014, y=0.28, label="Frms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'dodgerblue3','red'),
                     limits = c('actual',  'M', 'Frms'),
                     breaks = c('actual',  'M', 'Frms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash','dotted'),
                        limits = c('actual', 'M', 'Frms'),
                        breaks = c('actual', 'M', 'Frms'))
p12 <- p12 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = '') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p12

ggsave(p12, filename = "../figures/figure_3.png", width=8, height=6.5, dpi=300)
