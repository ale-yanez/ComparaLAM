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
out19 <- read.admb("../2019/stock_LAN/output/base")
out18 <- read.admb("../2018/stock_LAN/output/base")
out17 <- read.admb("../2017/stock_LAN/output/base")

length(out20$YRS)
Year <- c(out20$YRS[36],out20$YRS[35],out20$YRS[34],out20$YRS[33])
F45 <- round(c(out20$Fpbr[3], out19$Fpbr[3], out18$Fpbr[3], out17$Fpbr[3]),3)
RMS <- round(c(out20$BDoLP, out19$BDoLP, out18$BDoLP, out17$BDoLP)*0.4,0)

table <- cbind(Year=Year, F45=F45, RMS=RMS)
write.table(table, '../tables/LAN/pbrtable_1.txt', append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)
