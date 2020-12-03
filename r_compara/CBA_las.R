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

std20 <- read.table("../2020/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)
std19 <- read.table("../2019/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)
std18 <- read.table("../2018/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)
std17 <- read.table("../2017/stock_LAS/output/base.std", header=T, sep="", na="NA", fill=T)

# Estimación CBA #### 

r    <- seq(0.1,0.5,0.1) # niveles de riesgo (cuantiles)                                
nr   <- length(r)                                                                                   

CBA21  <- matrix(ncol=nr)
CBA20  <- matrix(ncol=nr)
CBA19  <- matrix(ncol=nr)
CBA18  <- matrix(ncol=nr)

# CBA del año siguiente al realizado la evaluacion
CBA21p    <-subset(std20,name=='CBA')$value[3]
CBA20p    <-subset(std19,name=='CBA')$value[3]
CBA19p    <-subset(std18,name=='CBA')$value[3]
CBA18p    <-subset(std17,name=='CBA')$value[3]

CBA21pstd <-subset(std20,name=='CBA')$std[3]
CBA20pstd <-subset(std19,name=='CBA')$std[3]
CBA19pstd <-subset(std18,name=='CBA')$std[3]
CBA18pstd <-subset(std17,name=='CBA')$std[3]


  for(j in 1:nr){	
    CBA21[j]<-qnorm(r[j],CBA21p,CBA21pstd)
    }
  
  for(j in 1:nr){	
  CBA20[j]<-qnorm(r[j],CBA20p,CBA20pstd)
  }

  for(j in 1:nr){	
  CBA19[j]<-qnorm(r[j],CBA19p,CBA19pstd)
  }

  for(j in 1:nr){	
  CBA18[j]<-qnorm(r[j],CBA18p,CBA18pstd)
  }

  tCBA <- round(rbind(CBA21,CBA20,CBA19,CBA18),0)
  colnames(tCBA) <- c(seq(10,50,10))
  eval <- c(2020,2019,2018,2017)
  
  tabCBA <- cbind(eval, tCBA)
  
  write.table(tCBA, '../tables/LAS/CBAtable_01.txt', append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)

  
  tabCBA
  df <- data.frame(asesoria=rep(c("2020", "2019", "2018", "2017"), each=5),
                    percent=rep(c("10%", "20%", "30%", "40%", "50%"),4),
                    value=(c(tabCBA[1,-1], tabCBA[2,-1], tabCBA[3,-1], tabCBA[4,-1])))
  head(df)
  
  p <- ggplot(df, aes(x=percent, y=value, group=asesoria)) +
    geom_line(aes(color=asesoria))+
    geom_point(aes(color=asesoria)) +
    theme_minimal() + theme(legend.position="right") +
    ylab('CBA (t)') + xlab('Porcentaje')

    p
    
  ggsave(p, filename = "../figures/LAS/figure_10.png", width=8.5, height=5.5, dpi=300)  
