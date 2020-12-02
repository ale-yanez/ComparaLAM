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

out20 <- read.admb("../2020/stock_LAN/output/base")
std20 <- read.table("../2020/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out19 <- read.admb("../2019/stock_LAN/output/base")
std19 <- read.table("../2019/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out18 <- read.admb("../2018/stock_LAN/output/base")
std18 <- read.table("../2018/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

out17 <- read.admb("../2017/stock_LAN/output/base")
std17 <- read.table("../2017/stock_LAN/output/base.std", header=T, sep="", na="NA", fill=T)

Ro20 <- exp(subset(std20,name=='log_Ro')$value)
Ro19 <- exp(subset(std19,name=='log_Ro')$value)
Ro18 <- exp(subset(std18,name=='log_Ro')$value)
Ro17 <- exp(subset(std17,name=='log_Ro')$value)

sd_Ro20 <- exp(subset(std20,name=='log_Ro')$std)
sd_Ro19 <- exp(subset(std19,name=='log_Ro')$std)
sd_Ro18 <- exp(subset(std18,name=='log_Ro')$std)
sd_Ro17 <- exp(subset(std17,name=='log_Ro')$std)


x <- data.frame(ev20=rnorm(1000,Ro20,sd_Ro20),ev19=rnorm(1000,Ro19,sd_Ro19),ev18=rnorm(1000,Ro18,sd_Ro18),ev17=rnorm(1000,Ro17,sd_Ro17))
data<- melt(x)
head(data)
colnames(data) <- c('Asesoria', 'Ro')

dens <- ggplot(data,aes(x=Ro, fill=Asesoria)) + geom_density(alpha=0.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Ro')

dens    

ggsave(dens, filename = "../figures/LAS/figure_4.png", width=8, height=6.5, dpi=300)

box <- ggplot(data,aes(x=Asesoria, y=Ro, fill=Asesoria)) + geom_boxplot() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Ro') + xlab('Asesoría')

box
ggsave(box, filename = "../figures/LAS/figure_5.png", width=8, height=6.5, dpi=300)


dens_2 <- ggplot(data,aes(x=Ro, fill=Asesoria)) + geom_density(alpha=0.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Ro')

dens_2 <- dens_2 + facet_grid(Asesoria ~ .)
dens_2
ggsave(dens_2, filename = "../figures/LAS/figure_6.png", width=6.5, height=8.5, dpi=300)
