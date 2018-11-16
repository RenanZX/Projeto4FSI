library(RSNNS)
library(ggplot2)
library(R.matlab)

#código de processamento baseado neste aritgo
#http://www.di.fc.ul.pt/~jpn/r/rbf/rbf.html

randdata = function(data){
  dP <- cbind(data$P.train,data$P.test)
  dT <- cbind(data$T.train,data$T.test)
  kdat <- list()
  dPv <- sample(ncol(dP), 0.9*ncol(dP), replace = TRUE)
  kdat$P.train <- dP[,dPv]
  kdat$P.test <- dP[,-dPv]
  kdat$T.train <- t(as.matrix(dT[,dPv]))
  kdat$T.test <- t(as.matrix(dT[,-dPv]))
  kdat
}

leitor <- list(
  readfile = function(){
    data <- readMat(file.choose())
    data
  }
)

processor <- list(
  ProcessarFBR = function(dados,sz,maxtit){
    yt <- t(as.matrix(dados$T.train))
    xt <- t(as.matrix(dados$P.train))
    
    modelo <- rbf(x = xt, y = yt, size = sz, maxit = maxtit, linOut = TRUE)
    predteste <- sign(predict(modelo, t(as.matrix(dados$P.test))))
    
    mean(predteste == t(dados$T.test))
  },
  ProcessarMLP = function(dados,sz,maxtit){
    yt <- t(as.matrix(dados$T.train))
    xt <- t(as.matrix(dados$P.train))
    
    modelo <- mlp(x = xt, y = yt, size = sz, maxit = maxtit, linOut = TRUE)
    predteste <- sign(predict(modelo, t(as.matrix(dados$P.test))))
    
    mean(predteste == t(dados$T.test))
  }
)

data = leitor$readfile()

cvalues <- 0:10

for (i in 0:11){
  rdata <- randdata(data)
  cvalues[i] = processor$ProcessarFBR(rdata,40,1000)*100
}

cvaluesmlp <- 0:10

for (i in 0:11){
  rdata <- randdata(data)
  cvaluesmlp[i] = processor$ProcessarMLP(rdata,40,1000)*100
}

plot(cvalues, type="o", col="blue",xlab = "modelos",ylab = "Taxa de acerto %", main = "Radial Basis Function x Multi Layer Perceptron",ylim=c(0,80))

lines(cvaluesmlp, type="o", pch=22, lty=2, col="red")

legend(c(0,20), legend=c("RBF","MLP"),
       col=c("blue","red"), lty=1:2, cex=0.5)