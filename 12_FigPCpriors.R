

# Created 10.12.2021 


# PC prior plots

#-----------------------------------------------------------------


installpack <- FALSE


if(installpack){
  install.packages(c("ggplot2", "patchwork", "sf", "spdep"))
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
}

library(INLA)
library(ggplot2)
library(patchwork)
library(sf)
library(spdep)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/TutorialExcessOutput/")


# prior for standard deviation

xvals = seq(0.1, 10, length.out=1000)
lambda=-log(0.01)/1

ggplot() + geom_line(aes(x=xvals, y=lambda*exp(-lambda*xvals))) + 
  theme_light() + ylab(expression("Implied prior for "~sigma[b])) + 
  xlab(expression(sigma[b])) + 
  theme(text = element_text(size = 8)) -> p1



# prior for mixing parameter

shp = read_sf("ProvCM01012020_g_WGS84.shp")
W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 


# Create the precision matrix
pol <- spdep::poly2nb(shp)
W <- spdep::nb2mat(pol, style = "B", zero.policy = TRUE)
W <- -W
diag(W) <- abs(apply(W, 1, sum))
n <- nrow(W)
QQ <- W


?INLA:::inla.pc.bym.phi
log.prior = INLA:::inla.pc.bym.phi(Q = QQ, rankdef = 1, u = 0.5, alpha = 0.5)
phis = 1/(1+exp(-seq(-7, 7, len=10000)))

ggplot() + geom_line(aes(x=phis, y=exp(log.prior(phis)))) + 
  theme_light() + ylab(expression("PC prior for "~phi)) + 
  xlab(expression(phi)) + 
  theme(text = element_text(size = 8)) -> p2



png("PCpriors.png", width = 14, height = 5, units = "cm", res = 300)
(p1|p2) 
dev.off()



######################################################################################
######################################################################################
######################################################################################
######################################################################################


