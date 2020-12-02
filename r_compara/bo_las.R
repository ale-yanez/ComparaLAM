# Funciones y Directorios ####
rm(list = ls())
library(rstudioapi)
library(ggplot2)
library(reshape)
library(ggpubr)
library(devtools)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

out20 <- read.admb("../2020/stock_LAS/output/base")
std20 <- read.table("../2020/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)

out19 <- read.admb("../2019/stock_LAS/output/base")
std19 <- read.table("../2019/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)

out18 <- read.admb("../2018/stock_LAS/output/base")
std18 <- read.table("../2018/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)

out17 <- read.admb("../2017/stock_LAS/output/base")
std17 <- read.table("../2017/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)

Bo20 <- subset(std20,name=='SSBo')$value
Bo19 <- subset(std19,name=='SSBo')$value
Bo18 <- subset(std18,name=='SSBo')$value
Bo17 <- subset(std17,name=='SSBo')$value

sd_Bo20 <- subset(std20,name=='SSBo')$std
sd_Bo19 <- subset(std19,name=='SSBo')$std
sd_Bo18 <- subset(std18,name=='SSBo')$std
sd_Bo17 <- subset(std17,name=='SSBo')$std

rms20 <- out20$BDoLP*0.4
rms19 <- out19$BDoLP*0.4
rms18 <- out18$BDoLP*0.4
rms17 <- out17$BDoLP*0.4


x <- data.frame(ev20=rnorm(1000,Bo20,sd_Bo20),ev19=rnorm(1000,Bo19,sd_Bo19),ev18=rnorm(1000,Bo18,sd_Bo18),ev17=rnorm(1000,Bo17,sd_Bo17))
data<- melt(x)
head(data)
data$rms <- c(rep(rms20,1000),rep(rms19,1000),rep(rms18,1000),rep(rms17,1000))
colnames(data) <- c('Asesoria', 'SSBo', 'rms')

dens <- ggplot(data,aes(x=SSBo, fill=Asesoria)) + geom_density(alpha=0.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Biomasa Desovante Virginal')

dens    

ggsave(dens, filename = "../figures/LAS/figure_1.png", width=8, height=6.5, dpi=300)

box <- ggplot(data,aes(x=Asesoria, y=SSBo, fill=Asesoria)) + geom_boxplot() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Biomasa Deesovante Virginal') + xlab('Asesoría')

box
ggsave(box, filename = "../figures/LAS/figure_2.png", width=8, height=6.5, dpi=300)


dens_2 <- ggplot(data,aes(x=SSBo, fill=Asesoria)) + geom_density(alpha=0.25) +
  geom_vline(data= data,  aes(xintercept=rms, color=Asesoria), size=1) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Biomasa Desovante Virginal')

dens_2 <- dens_2 + facet_grid(Asesoria ~ .)
dens_2
ggsave(dens_2, filename = "../figures/LAS/figure_3.png", width=6.5, height=8.5, dpi=300)
