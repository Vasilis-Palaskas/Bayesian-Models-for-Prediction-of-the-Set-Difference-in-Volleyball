
#------------------Posterior summary statistics of-------------------------------------------------------------------------------------------
###---- the mu, home,exp(mu),exp(home), l1-l2, Ezdts (Table 4)--------###

##Extracting the parameter values 
##load(file.choose())#/Output/Models/Final_ZDTS_paper.v1
##final_ZDTS_paper.v1<-Final_ZDTS_paper.v1
##params_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1)

lambda1<-params_final_ZDTS_paper.v1$lambda1
lambda2<-params_final_ZDTS_paper.v1$lambda2






####MU

mean_mu<-mean(params_final_ZDTS_paper.v1$mu)
median_mu<-median(params_final_ZDTS_paper.v1$mu)
sd_mu<-sd(params_final_ZDTS_paper.v1$mu)
quantile(params_final_ZDTS_paper.v1$mu,probs=c(0.025,0.975))

lower_interval_mu<-mean_mu-sd(params_final_ZDTS_paper.v1$mu)
upper_interval_mu<-mean_mu+sd(params_final_ZDTS_paper.v1$mu)
mu_posterior<-c(mean_mu,median_mu,sd_mu,quantile(params_final_ZDTS_paper.v1$mu,probs=c(0.025,0.975)))





## mu: exponential scale
exp_mu<-exp(params_final_ZDTS_paper.v1$mu)
mean_exp_mu<-mean(exp_mu)
median_exp_mu<-median(exp_mu)
sd_exp_mu<-sd(exp_mu)
quantile(exp_mu,probs=c(0.025,0.975))

lower_interval_exp_mu<-mean_exp_mu-sd(exp_mu)
upper_interval_exp_mu<-mean_exp_mu+sd(exp_mu)

exp_mu_posterior<-c(mean_exp_mu,median_exp_mu,sd_exp_mu,quantile(exp_mu,probs=c(0.025,0.975))
)

##home

mean_home<-mean(params_final_ZDTS_paper.v1$home)
median_home<-median(params_final_ZDTS_paper.v1$home)
sd_home<-sd(params_final_ZDTS_paper.v1$home)
quantile(params_final_ZDTS_paper.v1$home,probs=c(0.025,0.975))

lower_interval_home<-mean_home-sd(params_final_ZDTS_paper.v1$home)
upper_interval_home<-mean_home+sd(params_final_ZDTS_paper.v1$home)

home_posterior<-c(mean_home,median_home,sd_home,quantile(params_final_ZDTS_paper.v1$home,probs=c(0.025,0.975))
)


##home: exponential scale
exp_home<-exp(params_final_ZDTS_paper.v1$home)
mean_exp_home<-mean(exp_home)
median_exp_home<-median(exp_home)
quantile(exp_home,probs=c(0.025,0.975))


lower_interval_exp_home<-mean_exp_home-sd(exp_home)
upper_interval_exp_home<-mean_exp_home+sd(exp_home)

exp_home_posterior<-c(mean_exp_home,median_exp_home,sd_home,quantile(exp_home,probs=c(0.025,0.975))
)



####--------- Computation of l1-l2, Ezdts between 
####---------   equal teams'strength
expec_value_zdts_equal<-expec_value_used_equal<-NULL
#### lambda1 - lambda2 between two equal strength teams

lambda1_equal<-exp(params_final_ZDTS_paper.v1$mu+params_final_ZDTS_paper.v1$home)
lambda2_equal<-exp(params_final_ZDTS_paper.v1$mu)

for (i in 1:dim(lambda1_equal)[1]){
				expec_value_zdts_nomin<-besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),1)+
				(2*besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),2)*(lambda1_equal[i]+lambda2_equal[i]))/
				(lambda1_equal[i]*lambda2_equal[i])^(1/2)+(3*besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),3)*(lambda1_equal[i]^2+lambda1_equal[i]*lambda2_equal[i]+lambda2_equal[i]^2))/
				(lambda1_equal[i]*lambda2_equal[i])

			expec_value_zdts_denomin<-besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),1)*(lambda1_equal[i]+lambda2_equal[i])+
				(besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),2)*(lambda1_equal[i]^2+lambda2_equal[i]^2))/
				(lambda1_equal[i]*lambda2_equal[i])^(1/2)+(besselI(2*sqrt(lambda1_equal[i]*lambda2_equal[i]),3)*(lambda1_equal[i]^3+lambda2_equal[i]^3))/
				(lambda1_equal[i]*lambda2_equal[i])


		expec_value_zdts_equal<-c(expec_value_zdts_equal,
						(lambda1_equal[i]-lambda2_equal[i])*(expec_value_zdts_nomin/expec_value_zdts_denomin))
		
		expec_value_used_equal<-c(expec_value_used_equal,lambda1_equal[i]-lambda2_equal[i])
	
}





mean(expec_value_zdts_equal)
mean(expec_value_used_equal)

####------- lambda1 - lambda2 between two equal strength teams--------####


lambda1_lambda2_equal<-lambda1_equal-lambda2_equal
mean_lambda1_lambda2_equal<-mean(lambda1_lambda2_equal)
median_lambda1_lambda2_equal<-median(lambda1_lambda2_equal)
sd_lambda1_lambda2_equal<-sd(lambda1_lambda2_equal)
quantile(lambda1_lambda2_equal,probs=c(0.025,0.975))


lambda1_lambda2_equal_posterior<-c( mean_lambda1_lambda2_equal,
	median_lambda1_lambda2_equal,sd_lambda1_lambda2_equal,
	quantile(lambda1_lambda2_equal,probs=c(0.025,0.975))		)

###### Ezdts between two equal strength teams


mean(expec_value_zdts_equal)
median(expec_value_zdts_equal)
sd(expec_value_zdts_equal)
quantile(expec_value_zdts_equal,probs=c(0.025,0.975))

mean(expec_value_zdts_equal)-sd(expec_value_zdts_equal)
mean(expec_value_zdts_equal)+sd(expec_value_zdts_equal)
#apply(as.matrix(expec_value_zdts_equal),1,quantile,probs=c(0.025,0.975))



expec_value_zdts_equal_posterior<-c(mean(expec_value_zdts_equal),median(expec_value_zdts_equal),
sd(expec_value_zdts_equal),quantile(expec_value_zdts_equal,probs=c(0.025,0.975))
)



# ------- Table 4--------#


mu_posterior
exp_mu_posterior
home_posterior
exp_home_posterior
lambda1_lambda2_equal_posterior
expec_value_zdts_equal_posterior



# ------Appendix Table with posterior summary statistics of ZDTS model parameters-------##
final_ZDTS_paper.v1_summary<-summary(final_ZDTS_paper.v1,
      pars=c("mu","home","attack","defense"), probs=c(0.025,0.5,0.975))

round(final_ZDTS_paper.v1_summary$summary[c(1:26),c(1,3)],2)

xtable(round(final_ZDTS_paper.v1_summary$summary[c(1:26),c(1,3)],2))



#----- Appendix Table with posterior summary statistics of the ordered model parameters-------##
final_ordered_logistic_summary<-summary(final_ordered_logistic,
      pars=c("c","team_abil"), probs=c(0.025,0.5,0.975))

round(final_ordered_logistic_summary$summary[c(1:17),c(1,3)],2)

xtable(round(final_ordered_logistic_summary$summary[c(1:17),c(1,3)],2))
