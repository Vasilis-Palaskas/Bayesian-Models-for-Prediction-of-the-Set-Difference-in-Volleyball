# Load packages

library(latex2exp)
library(shape)




#### Data required for this type of analysis


lambda1<-params_final_ZDTS_paper.v1$lambda1
lambda2<-params_final_ZDTS_paper.v1$lambda2
# Calculate the posterior densities of
# both Esk (denoted as expec_value_used), Ezdts (denoted as expec_value_zdts) 

expec_value_zdts_nomin<-besselI(2*sqrt(lambda1*lambda2),1)+
				(2*besselI(2*sqrt(lambda1*lambda2),2)*(lambda1+lambda2))/
				(lambda1*lambda2)^(1/2)+(3*besselI(2*sqrt(lambda1*lambda2),3)*(lambda1^2+lambda1*lambda2+lambda2^2))/
				(lambda1*lambda2)

expec_value_zdts_denomin<-besselI(2*sqrt(lambda1*lambda2),1)*(lambda1+lambda2)+
				(besselI(2*sqrt(lambda1*lambda2),2)*(lambda1^2+lambda2^2))/
				(lambda1*lambda2)^(1/2)+(besselI(2*sqrt(lambda1*lambda2),3)*(lambda1^3+lambda2^3))/
				(lambda1*lambda2)

expec_value_used<-lambda1-lambda2
expec_value_zdts<-(lambda1-lambda2)*(expec_value_zdts_nomin/expec_value_zdts_denomin)

# Vectorize these quantities in terms of convenience
expec_value_used_new<-as.vector(expec_value_used)
expec_value_zdts_new<-as.vector(expec_value_zdts)
index<-cut(abs(expec_value_used_new),breaks=c(0,1,2,3,max(abs(expec_value_used_new))))
index<-cut(abs(expec_value_used_new),breaks=c(0,1,2,3,max(abs(expec_value_used_new))))



# Load the data
#ES <- dget(file.choose())#"E_sk.dat"

#EZ <- dget(file.choose())#"E_zdt.dat"


#index <- dget(file.choose())#"index.dat"


#l1 <- dget(file.choose())#lambda1.dat"


#l2 <- dget(file.choose())#lambda2.dat"

# Create a dataframe
data<-data.frame(ES=expec_value_used_new, EZ=expec_value_zdts_new, index=index, l1=as.vector(lambda1), l2=as.vector(lambda2))
head(data,20)

# 
data$loglam <- log(data$l1/data$l2)

data$logitEZ<- log( (3+data$EZ)/(3-data$EZ))# logit of shifted Ezdts, 
								#3 is used in terms of numerical convenience
attach(data)
dim(data)

# Keep as subsample 10000 random values in terms of convenience

sindx<-sample(1:length(ES),10000)


# For-loop so we find the proper cutpoint which will give us the 
# largest combined Rsquare (by the term combined we refer to something
# as Rsquare average of two models (two models fitted since
# between log of Ezdts (shifted) and logit of l1, l2 there are
# two different relationshis. For graphical representation of above mentioned,
# please check the below plot). 

ii <- seq(0,4,0.1)# For x=0, there is a change in relationship
				# between the two different quantities, so we observe
				# their relationship in these values	

N <- length(ii) 
res<-matrix( nrow=N, ncol=8 )


for (i in 1:N){ 
	plot(loglam[sindx],log(3+EZ)[sindx], col=index[sindx])

	# Model 1
	index0<-(loglam<ii[i])
	model <- lm( log(3+EZ)~loglam, data=data[index0,] ); model1<-model
	abline(model, lwd=3,col=2)
	res[i,1] <- ii[i] 
	res[i,2] <- summary(model)$r.sq
	res[i,5:6] <- coef(model)
	m1<-model 

	# Model 2
	model <- lm( log(3+EZ)~loglam, data=data[!index0,] ); model2<-model
	abline(model, lwd=3,col=2)
	res[i,3] <- summary(model)$r.sq
	res[i,7:8] <- coef(model)
	m2<-model 

	# Calculate R2 (combined) of Models 1, 2
	res[i,4]<- 1-(sum(m1$res^2)+sum(m2$res^2))/((length(EZ)-1)*var(log(3+EZ)))
	print(res[i,2:4])
}


# We chose the index which maximizes the R2
res[which.max( res[,4]),]

res[which.max( res[,4]),1]# cutpoint C
res[which.max( res[,4]),4]# combined R square
res[which.max( res[,4]),5:6]# coef of Model 1
res[which.max( res[,4]),7:8]# coef of Model 2


plot( res[,1], res[,2], type='l', col=2, lwd=2, lty=2,
		xlab="cutpoint (c)",ylim=c(0,1),main=TeX('Combined $R^{2}$ Selection'),
	ylab=TeX('$R^{2}$'),xaxt="n"		)

xlabel <- seq(0,6,by=0.8)
axis(1, at = xlabel)

ylabel <- seq(0, 1, by = 0.04)
lines(res[,1], res[,3], col=3, lwd=2 , lty=2)
lines(res[,1], res[,4], col=4, lwd=2 , lty=1)
lines(rep(res[which.max( res[,4]),1],49),seq(0,max( res[,4]),0.02 ),col=1,lwd=0.05,lty=4)
lines(seq(0,res[which.max( res[,4]),1],0.02 ),rep(max( res[,4]),41),col=1,lwd=0.05,lty=4)

legend( box.lty=0,"bottomleft", col=2:4,cex=1, lty=c(2,2,1), legend=c(TeX('$R^{2}$: $\\log(\\frac{\\lambda_{1}}{\\lambda_{2}}) \\leq c$'), TeX('$R^{2}$:  $\\log(\\frac{\\lambda_{1}}{\\lambda_{2}})>c$'),TeX('Combined $R^{2}$') ))#
Arrows(x0=0, y0=0.75, x1 = res[which.max( res[,4]),1]-0.10, y1 =max( res[,4])-0.03,lwd=2)


# Obtain the coefficients of the largest combined R-square
round(res[which.max( res[,4]),5:6],2)#log(l1/l2)<c
round(res[which.max( res[,4]),7:8],2)#log(l1/l2)>=c







# Details about models fitted for the cutpoint C=0.8 (selection of maximum combined R2)

plot(loglam[sindx],log(3+EZ)[sindx], col='black',main="Linear models Fitting",
		xlab=TeX("$\\log(\\frac{\\lambda_{1}}{\\lambda_{2}})$"),
		ylab=TeX("$\\log (3+E_{ZDTS})$"),cex.axis=1.2 ,cex.lab=1.3,xaxt="n"	)

model <- lm( log(3+EZ)~loglam, data=data )
#abline(model, lwd=2,lty=2,col=2)
Xlabel <- seq(-6, 6, by = 1)

axis(1, at = Xlabel, las = 1)

index0<-(loglam<0.8)
model <- lm( log(3+EZ)~loglam, data=data[index0,] ); model1<-model
abline(model, lwd=2,lty=1,col="blue")
summary(model)$r.sq
coef(model)
m1<-model 
model <- lm( log(3+EZ)~loglam, data=data[!index0,] ); model2<-model
abline(model, lwd=2,col="red",lty=1)
summary(model)$r.sq
coef(model) 
m2<-model

legend( lwd=c(1,1),lty=c(1,1),box.lty=0,"bottomright", col=c(4,2), legend=c("Model 1", "Model 2"),cex=1.2)#


