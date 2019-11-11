#--- Modelo TERRA ----

#Instalar y Cargar librerias
#install.packages("R2OpenBUGS")
library(R2OpenBUGS)

#library(R2jags)

#-Directorio de trabajo-
#wdir<-"c:/lnieto/Proyectos/Datank/Datos/Terra/"
#setwd(wdir)

#--- Retail SUPERTIENDA---

retail<- read_csv("data_retail_semanal.csv")



#--- ACTIVIDAD ---

#-Lectura de datos-
#act<-read.csv("act_real(1).csv")
acti <- read.csv("training_data_d.csv")
act <- retail
act <- retail %>% slice(53:156)
test <- retail %>% slice(157:208)
act$cantidad<-as.numeric(act$cantidad)
act$fecha_del_pedido<-as.Date(act$fecha_del_pedido)

n<-dim(act)[1]
#act<-act[-n,]
#n<-dim(act)[1]
plot(act$fecha_del_pedido,log(act$cantidad),type="l")

#-Creacion de variables
sem<-rep(1:26,4)
tem<-rep(c(rep(1,26),rep(2,26)),2)
ano<-c(rep(1,52),rep(2,52))

#-Definicion de datos para OpenBugs
data<-list("n"=n,"y"=log(act$cantidad),"sem"=sem,"tem"=tem,"ano"=ano)

#-Definicion de inits-
inits1<-function(){list(alpha=0,beta=rep(0,2),gama=rep(0,26),epsilon=rep(0,2),
                       bega=matrix(0,nrow=2,ncol=26),beep=matrix(0,nrow=2,ncol=2),gaep=matrix(0,nrow=26,ncol=2),
                       tau=1,yf=rep(0,n),yp=rep(0,52))}
inits2<-function(){list(alpha=0,beta=rep(0,2),gama=rep(0,26),epsilon=rep(0,2),
                        tau=1,yf=rep(0,n),yp=rep(0,52))}
inits3<-function(){list(alpha=0,beta=rep(0,2),gama=rep(0,26),epsilon=rep(0,2),
                       bega=matrix(0,nrow=2,ncol=26),beep=matrix(0,nrow=2,ncol=2),gaep=matrix(0,nrow=26,ncol=2),
                       tau=1,yf=rep(0,n),yp=rep(0,52),lam=rep(0,n+52),tau.lam=1)}

#-Seleccion de parametros a monitorear-
pars1<-c("alpha.est","beta.est","gama.est","epsilon.est",
        "bega.est","beep.est","gaep.est",
        "tau","yf","yp")
pars2<-c("alpha.est","beta.est","gama.est","epsilon.est",
        "tau","yf","yp")
pars3<-c("alpha.est","beta.est","gama.est","epsilon.est",
        "bega.est","beep.est","gaep.est",
        "tau","yf","yp","lam.est")

#-Corrida de codigo-
#BUGS
act.sim1<-bugs(data,inits1,pars1,model.file="modact.txt",
              n.iter=8000,n.chains=2,n.burnin=900)
act.sim2<-bugs(data,inits2,pars2,model.file="modact2.txt",
              n.iter=5000,n.chains=2,n.burnin=500)
act.sim3<-bugs(data,inits3,pars3,model.file="modact3.txt",
              n.iter=5000,n.chains=2,n.burnin=500,n.thin=10,debug=TRUE)
#JAGS
act.sim1<-jags(data,inits1,pars1,model.file="modact.txt",
              n.iter=5000,n.chains=2,n.burnin=500)
act.sim2<-jags(data,inits2,pars2,model.file="modact2.txt",
              n.iter=5000,n.chains=2,n.burnin=500)
act.sim3<-jags(data,inits3,pars3,model.file="modact3.txt",
              n.iter=5000,n.chains=2,n.burnin=500,n.thin=10)

#-Seleccionar modelo
act.sim<-act.sim1

#-Monitoreo de la cadena-

#OpenBUGS
#out<-act.sim$sims.list
#
#z<-out$alpha
#par(mfrow=c(2,2))
#plot(z,type="l")
#plot(cumsum(z)/(1:length(z)),type="l")
#hist(z,freq=FALSE)
#acf(z)

#Resumen (estimadores)
out.sum<-act.sim$summary
#
#out.sum<-act.sim$BUGSoutput$summary
print(out.sum)
head(out.sum)

#Estimadores (beta)
#out.beta<-out.sum[grep("beta",rownames(out.sum)),]
#par(mfrow=c(1,1))
#plot(out.beta[,1]-mean(out.beta[,1]))

#Estimadores (gama)
#out.gama<-out.sum[grep("gama",rownames(out.sum)),]
#par(mfrow=c(1,1))
#plot(out.gama[,1]-mean(out.gama[,1]))

#Estimadores (epsilon)
#out.epsilon<-out.sum[grep("epsilon",rownames(out.sum)),]
#par(mfrow=c(1,1))
#plot(out.epsilon[,1]-mean(out.epsilon[,1]))

#Estimadores (lam)
#out.lam<-out.sum[grep("lam",rownames(out.sum)),]
#out.lam[,c(1,3,7)]
#par(mfrow=c(1,1))
#plot(out.lam[,1],type="l")


#Predicciones (dentro de muestra)
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
#
par(mfrow=c(1,1))
ymin<-min(data$y,out.yf[,c(1,3,7)])
ymax<-max(data$y,out.yf[,c(1,3,7)])
plot(data$y,ylim=c(ymin,ymax),lwd=2.4,type="l")
lines(out.yf[,1],lwd=1.1,col=19)
lines(out.yf[,3],lty=2,col=19)
lines(out.yf[,7],lty=2,col=19)
#
par(mfrow=c(1,1))
plot(as.vector(t(data$y)),out.yf[,1])
R2<-(cor(as.vector(t(data$y)),out.yf[,1]))^2
print(R2)

#Predicciones (individuo promedio) 
# Gráficas para plática
out.yp<-out.sum[grep("yp",rownames(out.sum)),]
#
par(mfrow=c(1,1))
ymin<-min(data$y,out.yp[,c(1,3,7)])
ymax<-max(data$y,out.yp[,c(1,3,7)])
plot(c(data$y,out.yp[,1]),ylim=c(ymin,ymax),type="n")
lines(1:104,data$y,lwd=1,col=1)
lines(105:156,out.yp[,1],lwd=1.5,col=4)
lines(105:156,out.yp[,3],lty=1,col=4)
lines(105:156,out.yp[,7],lty=1,col=4)
lines(105:156,log(test$cantidad),lwd=1.5,col=3)
