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

F20 <- exp(subset(std20,name=='log_Fh')$value[36])
F19 <- exp(subset(std19,name=='log_Fh')$value[35])
F18 <- exp(subset(std18,name=='log_Fh')$value[34])
F17 <- exp(subset(std17,name=='log_Fh')$value[33])

sd_F20 <- subset(std20,name=='log_Fh')$std[36]
sd_F19 <- subset(std19,name=='log_Fh')$std[35]
sd_F18 <- subset(std18,name=='log_Fh')$std[34]
sd_F17 <- subset(std17,name=='log_Fh')$std[33]


x <- data.frame(ev20=rnorm(1000,F20,sd_F20),ev19=rnorm(1000,F19,sd_F19),ev18=rnorm(1000,F18,sd_F18),ev17=rnorm(1000,F17,sd_F17))
data<- melt(x)
head(data)
colnames(data) <- c('Asesoria', 'Fcr')

dens <- ggplot(data,aes(x=Fcr, fill=Asesoria)) + geom_density(alpha=0.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Fcr')

dens    

ggsave(dens, filename = "../figures/LAS/figure_7.png", width=8, height=6.5, dpi=300)

box <- ggplot(data,aes(x=Asesoria, y=Fcr, fill=Asesoria)) + geom_boxplot() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Fcr') + xlab('Asesoría')

box
ggsave(box, filename = "../figures/LAS/figure_8.png", width=8, height=6.5, dpi=300)


dens_2 <- ggplot(data,aes(x=Fcr, fill=Asesoria)) + geom_density(alpha=0.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=9)) +
  ylab('Densidad') + xlab('Fcr')

dens_2 <- dens_2 + facet_grid(Asesoria ~ .)
dens_2
ggsave(dens_2, filename = "../figures/LAS/figure_9.png", width=6.5, height=8.5, dpi=300)
