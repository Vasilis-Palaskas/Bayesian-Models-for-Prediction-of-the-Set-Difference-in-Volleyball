
######-----------------------ZDTS Model--------------------------------######
####---------------------MCMC CONVERGENCE DIAGNOSTICS------------------######
color_scheme_set("brightblue")
## ACF Plots

### Merging
mcmc_acf_bar(posterior_mu_home)
mcmc_acf_bar(posterior_attack)
mcmc_acf_bar(posterior_overall)
mcmc_acf_bar(posterior_defense)

#Without merging
mcmc_acf_bar(array_posterior_final_ZDTS_paper.v1, pars = c("mu", "home"))
mcmc_acf_bar(array_posterior_final_ZDTS_paper.v1[,,25:36])
mcmc_acf_bar(array_posterior_final_ZDTS_paper.v1[,,37:48])
mcmc_acf_bar(array_posterior_final_ZDTS_paper.v1[,,710:721])




## Trace Plots
color_scheme_set("mix-blue-red")


mcmc_trace(array_posterior_final_ZDTS_paper.v1, pars=c("mu","home"), 
           facet_args = list(ncol = 1, strip.position = "left"))
mcmc_trace(array_posterior_final_ZDTS_paper.v1[,,25:36])
mcmc_trace(array_posterior_final_ZDTS_paper.v1[,,37:48])
mcmc_trace(array_posterior_final_ZDTS_paper.v1[,,710:721])



## Ordered according to their ranking position
mcmc_trace(posterior_attack[,c(attack_order_final_ZDTS_paper.v1)])
mcmc_trace(posterior_defense[,c(defense_order_final_ZDTS_paper.v1)])
mcmc_trace(posterior_overall[,c(overall_order_final_ZDTS_paper.v1)])



## Ergodic plots

par(mfrow=c(1,2))
cum_mu <- cumsum(mu_final_ZDTS_paper.v1)/c(1:length(mu_final_ZDTS_paper.v1))
plot(cum_mu,type="l",col="blue",ylab="mu",xlab="Iterations")

cum_home<- cumsum(home_final_ZDTS_paper.v1)/c(1:length(home_final_ZDTS_paper.v1))
plot(cum_home,type="l",col="blue",ylab="home",xlab="Iterations")

par(mfrow=c(3,4))
# for attacks (Merged chains)
par(mfrow=c(3,4))



cum_att1<-cumsum(attack_final_ZDTS_paper.v1[,1])/c(1:length(attack_final_ZDTS_paper.v1[,1]))
plot(cum_att1,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[1])

cum_att2<-cumsum(attack_final_ZDTS_paper.v1[,2])/c(1:length(attack_final_ZDTS_paper.v1[,2]))
plot(cum_att2,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[2])

cum_att3<-cumsum(attack_final_ZDTS_paper.v1[,3])/c(1:length(attack_final_ZDTS_paper.v1[,3]))
plot(cum_att3,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[3])

cum_att4<-cumsum(attack_final_ZDTS_paper.v1[,4])/c(1:length(attack_final_ZDTS_paper.v1[,4]))
plot(cum_att4,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[4])

cum_att5<-cumsum(attack_final_ZDTS_paper.v1[,5])/c(1:length(attack_final_ZDTS_paper.v1[,5]))
plot(cum_att5,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[5])

cum_att6<-cumsum(attack_final_ZDTS_paper.v1[,6])/c(1:length(attack_final_ZDTS_paper.v1[,6]))
plot(cum_att6,type="l",col="blue",ylab=colnames(attack_final_ZDTS_paper.v1)[6])

cum_att7<-cumsum(attack_final_ZDTS_paper.v1[,7])/c(1:length(attack_final_ZDTS_paper.v1[,7]))
plot(cum_att7,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[7])

cum_att8<-cumsum(attack_final_ZDTS_paper.v1[,8])/c(1:length(attack_final_ZDTS_paper.v1[,8]))
plot(cum_att8,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[8])

cum_att9<-cumsum(attack_final_ZDTS_paper.v1[,9])/c(1:length(attack_final_ZDTS_paper.v1[,9]))
plot(cum_att9,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[9])

cum_att10<-cumsum(attack_final_ZDTS_paper.v1[,10])/c(1:length(attack_final_ZDTS_paper.v1[,10]))
plot(cum_att8,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[10])

cum_att11<-cumsum(attack_final_ZDTS_paper.v1[,11])/c(1:length(attack_final_ZDTS_paper.v1[,11]))
plot(cum_att11,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[11])

cum_att12<-cumsum(attack_final_ZDTS_paper.v1[,12])/c(1:length(attack_final_ZDTS_paper.v1[,12]))
plot(cum_att12,type="l",col="blue",xlab="Iterations",ylab=colnames(attack_final_ZDTS_paper.v1)[12])


# for defenses (Merge chains)
par(mfrow=c(3,4))


cum_def1<-cumsum(defense_final_ZDTS_paper.v1[,1])/c(1:length(defense_final_ZDTS_paper.v1[,1]))
plot(cum_def1,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[1])

cum_def2<-cumsum(defense_final_ZDTS_paper.v1[,2])/c(1:length(defense_final_ZDTS_paper.v1[,2]))
plot(cum_def2,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[2])

cum_def3<-cumsum(defense_final_ZDTS_paper.v1[,3])/c(1:length(defense_final_ZDTS_paper.v1[,3]))
plot(cum_def3,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[3])

cum_def4<-cumsum(defense_final_ZDTS_paper.v1[,4])/c(1:length(defense_final_ZDTS_paper.v1[,4]))
plot(cum_def4,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[4])

cum_def5<-cumsum(defense_final_ZDTS_paper.v1[,5])/c(1:length(defense_final_ZDTS_paper.v1[,5]))
plot(cum_def5,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[5])

cum_def6<-cumsum(defense_final_ZDTS_paper.v1[,6])/c(1:length(defense_final_ZDTS_paper.v1[,6]))
plot(cum_def6,type="l",col="blue",ylab=colnames(defense_final_ZDTS_paper.v1)[6])

cum_def7<-cumsum(defense_final_ZDTS_paper.v1[,7])/c(1:length(defense_final_ZDTS_paper.v1[,7]))
plot(cum_def7,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[7])

cum_def8<-cumsum(defense_final_ZDTS_paper.v1[,8])/c(1:length(defense_final_ZDTS_paper.v1[,8]))
plot(cum_def8,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[8])

cum_def9<-cumsum(defense_final_ZDTS_paper.v1[,9])/c(1:length(defense_final_ZDTS_paper.v1[,9]))
plot(cum_def9,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[9])

cum_def10<-cumsum(defense_final_ZDTS_paper.v1[,10])/c(1:length(defense_final_ZDTS_paper.v1[,10]))
plot(cum_def8,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[10])

cum_def11<-cumsum(defense_final_ZDTS_paper.v1[,11])/c(1:length(defense_final_ZDTS_paper.v1[,11]))
plot(cum_def11,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[11])

cum_def12<-cumsum(defense_final_ZDTS_paper.v1[,12])/c(1:length(defense_final_ZDTS_paper.v1[,12]))
plot(cum_def12,type="l",col="blue",xlab="Iterations",ylab=colnames(defense_final_ZDTS_paper.v1)[12])



# for overall abilities
par(mfrow=c(3,4))

cum_over1<-cumsum(overall_final_ZDTS_paper.v1[,1])/c(1:length(overall_final_ZDTS_paper.v1[,1]))
plot(cum_over1,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[1])

cum_over2<-cumsum(overall_final_ZDTS_paper.v1[,2])/c(1:length(overall_final_ZDTS_paper.v1[,2]))
plot(cum_over2,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[2])

cum_over3<-cumsum(overall_final_ZDTS_paper.v1[,3])/c(1:length(overall_final_ZDTS_paper.v1[,3]))
plot(cum_over3,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[3])

cum_over4<-cumsum(overall_final_ZDTS_paper.v1[,4])/c(1:length(overall_final_ZDTS_paper.v1[,4]))
plot(cum_over4,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[4])

cum_over5<-cumsum(overall_final_ZDTS_paper.v1[,5])/c(1:length(overall_final_ZDTS_paper.v1[,5]))
plot(cum_over5,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[5])

cum_over6<-cumsum(overall_final_ZDTS_paper.v1[,6])/c(1:length(overall_final_ZDTS_paper.v1[,6]))
plot(cum_over6,type="l",col="blue",ylab=colnames(overall_final_ZDTS_paper.v1)[6])

cum_over7<-cumsum(overall_final_ZDTS_paper.v1[,7])/c(1:length(overall_final_ZDTS_paper.v1[,7]))
plot(cum_over7,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[7])

cum_over8<-cumsum(overall_final_ZDTS_paper.v1[,8])/c(1:length(overall_final_ZDTS_paper.v1[,8]))
plot(cum_over8,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[8])

cum_over9<-cumsum(overall_final_ZDTS_paper.v1[,9])/c(1:length(overall_final_ZDTS_paper.v1[,9]))
plot(cum_over9,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[9])

cum_over10<-cumsum(overall_final_ZDTS_paper.v1[,10])/c(1:length(overall_final_ZDTS_paper.v1[,10]))
plot(cum_over8,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[10])

cum_over11<-cumsum(overall_final_ZDTS_paper.v1[,11])/c(1:length(overall_final_ZDTS_paper.v1[,11]))
plot(cum_over11,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[11])

cum_over12<-cumsum(overall_final_ZDTS_paper.v1[,12])/c(1:length(overall_final_ZDTS_paper.v1[,12]))
plot(cum_over12,type="l",col="blue",xlab="Iterations",ylab=colnames(overall_final_ZDTS_paper.v1)[12])




## Neff/N histograms
neff_ratio_mu_home<-neff_ratio(final_ZDTS_paper.v1,pars=c("mu","home"))
neff_ratio_attack<-neff_ratio(final_ZDTS_paper.v1,pars=c("attack"))
neff_ratio_defense<-neff_ratio(final_ZDTS_paper.v1,pars=c("defense"))
neff_ratio_overall<-neff_ratio(final_ZDTS_paper.v1,pars=c("overall"))




mcmc_neff(neff_ratio_mu_home)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_neff(neff_ratio_attack)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))
mcmc_neff(neff_ratio_defense)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_neff(neff_ratio_overall)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


## Rhat Plots
names(final_ZDTS_paper.v1)

rhat<-rhat(final_ZDTS_paper.v1)

rhat_mu_home<-rhat[c(1,2)]
rhat_attack<-rhat[c(25:36)]
rhat_defense<-rhat[c(37:48)]
rhat_overall<-rhat[c(37:48)]

mcmc_rhat(rhat_mu_home, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


mcmc_rhat(rhat_attack, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


mcmc_rhat(rhat_defense, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


mcmc_rhat(rhat_overall, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))



#-------------------------------------------------------------------------------------
###---------------Table with several convergence ----------------###
##----------diagnostics measures (neff, Rhat, Raftery-Lewis)-----####

## Merged Chains
params_final_ZDTS_paper.v1_summary<-summary(final_ZDTS_paper.v1,pars=c("mu","home","attack","defense"))

# All together model parameters

all_params_final_ZDTS_paper.v1<-cbind(mu_final_ZDTS_paper.v1,home_final_ZDTS_paper.v1,
	attack_final_ZDTS_paper.v1,defense_final_ZDTS_paper.v1)
# Convertion to mcmc objects

all_params_final_ZDTS_paper.v1_mcmc<-mcmc(all_params_final_ZDTS_paper.v1)

# Raftery Diagnostics
raftery.diag(all_params_final_ZDTS_paper.v1_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)

#Table with these measures
converg.diag_matrix_final_ZDTS_paper.v1<-cbind(round(params_final_ZDTS_paper.v1_summary$summary[,c(9)]),round(params_final_ZDTS_paper.v1_summary$summary[,c(10)],2),round(raftery.diag(all_params_final_ZDTS_paper.v1_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,2))
colnames(converg.diag_matrix_final_ZDTS_paper.v1)[c(1,2)]<-c("n_eff","Rhat")
# LateX table
xtable(converg.diag_matrix_final_ZDTS_paper.v1,digits=0,"MCMC Convergence diagnostics for the ZDTS")



## Separate Chains
# Extraction of parameters' values including warmup iterations
params_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,permuted=F)
params_final_ZDTS_paper.v1<-as.data.frame(params_final_ZDTS_paper.v1)

mu_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,permuted=F,pars="mu")
mu_final_ZDTS_paper.v1<-as.data.frame(mu_final_ZDTS_paper.v1)
mu_final_ZDTS_paper.v1_chain1<-mu_final_ZDTS_paper.v1[,1]

home_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,permuted=F,pars="home")
home_final_ZDTS_paper.v1<-as.data.frame(home_final_ZDTS_paper.v1)
home_final_ZDTS_paper.v1_chain1<-home_final_ZDTS_paper.v1[,1]

attack_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,permuted=F,pars="attack")
attack_final_ZDTS_paper.v1<-as.data.frame(attack_final_ZDTS_paper.v1)

defense_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,permuted=F,pars="defense")
defense_final_ZDTS_paper.v1<-as.data.frame(defense_final_ZDTS_paper.v1)

# All together model parameters

all_params_final_ZDTS_paper.v1<-cbind(mu_final_ZDTS_paper.v1,home_final_ZDTS_paper.v1,
	attack_final_ZDTS_paper.v1,defense_final_ZDTS_paper.v1)


all_params_final_ZDTS_paper.v1<-cbind(mu_final_ZDTS_paper.v1,home_final_ZDTS_paper.v1)
# Convertion to mcmc objects

all_params_final_ZDTS_paper.v1_mcmc<-mcmc(all_params_final_ZDTS_paper.v1)

# Raftery Diagnostics
raftery.diag(all_params_final_ZDTS_paper.v1_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)

#Table with these measures
converg.diag_matrix_final_ZDTS_paper.v1<-cbind(round(params_final_ZDTS_paper.v1_summary$summary[,c(9)]),round(params_final_ZDTS_paper.v1_summary$summary[,c(10)],2),round(raftery.diag(all_params_final_ZDTS_paper.v1_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,2))
colnames(converg.diag_matrix_final_ZDTS_paper.v1)[c(1,2)]<-c("n_eff","Rhat")
# LateX table
xtable(converg.diag_matrix_final_ZDTS_paper.v1,digits=0,"MCMC Convergence diagnostics for the ZDTS")



