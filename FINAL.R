########################111111111111111111111111111##########################
namuka = read.csv('10_industry_portfolios_FF3_1992_2017.csv', header=T)
pca.cov=prcomp(namuka)
summary(pca.cov)
names(pca.cov)
attributes(pca.cov)
eigen.fit=eigen(var(as.matrix(namuka)))
names(eigen.fit)
sqrt(eigen.fit$values)
eigen.fit$vectors
p1=p1/sum(p1)
p1
names(p1)
barplot(p1, horiz=F, cex.names=0.5)
f1 = as.matrix(rtn %*% p1)
attributes(f1)
n.obs=dim(rtn)[1]
X.mat=cbind(rep(1, n.obs), f1)
colnames(X.mat)=c("intercept", "Factor 1")
XX.mat = crosspond(X.mat)
G.hat=solve(XX.mat)%*%crossprod(X.mat, as.matrix(rtn))
beta.hat=G.hat[2,]
beta.hat
E.hat=as.matrix(rtn)-X.mat%*%G.hat
diagD.hat=diag(crossprod(E.hat)/(n.obs-2))
cov.pc1=as.numeric(var(f1))*beta.hat%*%t(beta.hat)+diag(diagD.hat)
cov.pc1
cov2cor(cov.pc1)
fama<-read.csv('FF3_1992_2017.csv')
fama<-fama[, -1]
fama<-fama/100
fama
namuka.ff<-as.data.frame(fama)
namuka1<-namuka.ff*NA
for (i in 1:4){
  namuka1[,i]<-namuka.ff[,i] - fama["RF"]
}
sigF<-as.matrix(var(fama[,-4]))
sigF
Mkt.RF<-fama[,1]
SMB<-fama[,2]
HML<-fama[,3]
namuka1=namuka1[,-4]
Stocks<-as.matrix(fama)
Fit<-lm(Stocks~Mkt.RF+SMB+HML)
Fit$coef
beta<-as.matrix(Fit$coef)
beta<-t(beta[-1,])
beta
resig2<-apply((Fit$resid)^2, 2, sum)/(11-3-1)
resig2<-diag(resig)
cov_fama<-beta%*%sigF%*%t(beta)+resig2
cov_fama
one<-matrix(rep(1,4), ncol=1)
up<-solve(cov_fama)%*%one
down<-t(one)%*%up
mvp.3f<-up/as.numeric(down)
mvp.3f
#############################22222222222222222222222222222222###########################
read.csv("mmrk.csv")
data = read.csv("mmrk.csv")
mrk=data[-1,]
head(mrk)
str(mrk)
str(mrk)
B.mat = cbind(mrk)
dim(B.mat)
dim(mrk)
f.to = solve(t(B.mat)%*%B.mat)%*%t(B.mat)%*%t(mrk)
t(f.to)
dim(f.to)
e.to = t(mrk) - B.mat%*%f.to
t(e.to)
dim(e.to)
diagD.hat = apply(e.to, 1, var)
Dinv.hat = diag(diagD.hat^(-1))
Dinv.hat
f.tg = solve(t(B.mat)%*%Dinv.hat%*%B.mat)%*%t(B.mat)%*%Dinv.hat%*%t(mrk)
f.tg
dim(f.tg)
t(f.tg)
e.tg = t(mrk) - B.mat%*%f.tg
t(e.tg)
dim(e.tg)
diagD.hat.g = apply(e.tg, 1, var)
Dinv.hat.g = diag(diagD.hat.g)
Dinv.hat.g
dim(Dinv.hat.g)
sigma.f = var(coredata(t(f.tg)))
cov.f = B.mat%*%sigma.f%*%t(B.mat) + Dinv.hat.g
cov.f
cor.f = cov2cor(cov.f)
cor.f
######################33333333333333333333333########################
qnorm(0.1)
industry.ret.m<-read.csv(('10_industries_portfolios_1992_2017.csv'), header=T)
industry.ret=industry.ret.m[,-1]
getSymbols(industry.ret.m, from='1992-01', to='2017-12')
apply(industry.ret, 2, sd)
mu<-apply(industry.ret, 2, mean)
mu
sigma<-apply(industry.ret, 2, sd)
sigma
q01<-mu+sigma*qnorm(0.01)
q01
es01<--(mu+sigma*dnorm(qnorm(0.01))/0.01)
es01
plot(industry.ret.m)
abline(h=c(q01[01]), col=c('red', 'blue'))
legend('topright', legend=c('q01'), col=('blue'), lty=1:2)
getSymbols(industry.ret.m, from='1992-01', to='2017-12')
library(PerformanceAnalytics)
VaR(industry.ret.m, p=0.95, method = "gaussian")
VaR(industry.ret.m, p=0.95, method = "historical")
ES(industry.ret.m, p=0.95, method = "gaussian")
ES(industry.ret.m, p=0.95, method = "historical")
industry.ret.6m=(industry.ret.m^0.17)/6









