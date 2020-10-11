
######--------------------- Ordered Logistic Model-------------------######
######---------------------MCMC CONVERGENCE DIAGNOSTICS------------------######
color_scheme_set("brightblue")
## ACF Plots
### Merging

mcmc_acf_bar(posterior_c)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_acf_bar(posterior_team_abil)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


### Without merging

mcmc_acf_bar(array_posterior_final_ordered_logistic, pars = c("c"))+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_acf_bar(array_posterior_final_ordered_logistic[,,17:28])+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

# Ordered according to their ranking position

mcmc_acf_bar(array_posterior_final_ordered_logistic[,,16+team_abil_order_final_ordered_logistic])

## Trace Plots
color_scheme_set("mix-blue-red")

mcmc_trace(array_posterior_final_ordered_logistic, pars=c("c"), 
           facet_args = list(ncol = 1, strip.position = "left"))+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_trace(array_posterior_final_ordered_logistic[,,17:28])+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))


# Ordered according to their ranking position

mcmc_trace(array_posterior_final_ordered_logistic[,,16+team_abil_order_final_ordered_logistic])



## Ergodic plots

par(mfrow=c(2,3))
cum_c1<- cumsum(c_final_ordered_logistic[,1])/c(1:length(c_final_ordered_logistic[,1]))
plot(cum_c1,type="l",col="blue",ylab="c1",xlab="Iterations")

cum_c2 <- cumsum(c_final_ordered_logistic[,2])/c(1:length(c_final_ordered_logistic[,2]))
plot(cum_c2,type="l",col="blue",ylab="c2",xlab="Iterations")

cum_c3 <- cumsum(c_final_ordered_logistic[,3])/c(1:length(c_final_ordered_logistic[,3]))
plot(cum_c3,type="l",col="blue",ylab="c3",xlab="Iterations")

cum_c4 <- cumsum(c_final_ordered_logistic[,4])/c(1:length(c_final_ordered_logistic[,4]))
plot(cum_c4,type="l",col="blue",ylab="c4",xlab="Iterations")

cum_c5 <- cumsum(c_final_ordered_logistic[,5])/c(1:length(c_final_ordered_logistic[,5]))
plot(cum_c5,type="l",col="blue",ylab="c5",xlab="Iterations")

# Ergodic plots for team ability parameters

par(mfrow=c(3,4))
cum_team_abil1<-cumsum(team_abil_final_ordered_logistic[,1])/c(1:length(team_abil_final_ordered_logistic[,1]))
plot(cum_team_abil1,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[1])

cum_team_abil2<-cumsum(team_abil_final_ordered_logistic[,2])/c(1:length(team_abil_final_ordered_logistic[,2]))
plot(cum_team_abil2,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[2])

cum_team_abil3<-cumsum(team_abil_final_ordered_logistic[,3])/c(1:length(team_abil_final_ordered_logistic[,3]))
plot(cum_team_abil3,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[3])

cum_team_abil4<-cumsum(team_abil_final_ordered_logistic[,4])/c(1:length(team_abil_final_ordered_logistic[,4]))
plot(cum_team_abil4,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[4])

cum_team_abil5<-cumsum(team_abil_final_ordered_logistic[,5])/c(1:length(team_abil_final_ordered_logistic[,5]))
plot(cum_team_abil4,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[5])

cum_team_abil6<-cumsum(team_abil_final_ordered_logistic[,6])/c(1:length(team_abil_final_ordered_logistic[,6]))
plot(cum_team_abil6,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[6])

cum_team_abil7<-cumsum(team_abil_final_ordered_logistic[,7])/c(1:length(team_abil_final_ordered_logistic[,7]))
plot(cum_team_abil7,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[7])

cum_team_abil8<-cumsum(team_abil_final_ordered_logistic[,3])/c(1:length(team_abil_final_ordered_logistic[,8]))
plot(cum_team_abil8,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[8])

cum_team_abil9<-cumsum(team_abil_final_ordered_logistic[,9])/c(1:length(team_abil_final_ordered_logistic[,9]))
plot(cum_team_abil9,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[9])

cum_team_abil10<-cumsum(team_abil_final_ordered_logistic[,10])/c(1:length(team_abil_final_ordered_logistic[,10]))
plot(cum_team_abil10,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[10])

cum_team_abil11<-cumsum(team_abil_final_ordered_logistic[,11])/c(1:length(team_abil_final_ordered_logistic[,11]))
plot(cum_team_abil11,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[11])

cum_team_abil12<-cumsum(team_abil_final_ordered_logistic[,12])/c(1:length(team_abil_final_ordered_logistic[,12]))
plot(cum_team_abil12,type="l",col="blue",xlab="Iterations",ylab=colnames(team_abil_final_ordered_logistic)[12])





## Neff/N histograms
neff_ratio_c<-neff_ratio(final_ordered_logistic,pars=c("c"))
neff_ratio_team_abil<-neff_ratio(final_ordered_logistic,pars=c("team_abil"))

mcmc_neff(neff_ratio_c)+ yaxis_text(hjust = 1)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_neff(neff_ratio_team_abil)+ yaxis_text(hjust = 3)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))



##Rhat Plots
names(final_ordered_logistic)

rhat<-rhat(final_ordered_logistic)

rhat_c<-rhat[c(12:16)]
rhat_team_abil<-rhat[c(17:28)]


mcmc_rhat(rhat_c, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 3)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

mcmc_rhat(rhat_team_abil, size = 5)+ yaxis_text(hjust = 1)+ yaxis_text(hjust = 3)+  theme(        axis.text.y = element_text( size = 15, angle = 0, hjust = 1, vjust = 0),  
               plot.title  =element_text( size = 23))

------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
###---------------Table with several convergence ----------------###
##----------diagnostics measures (neff, Rhat, Raftery-Lewis)-----####

## Merged Chains
params_final_ordered_logistic_summary<-summary(final_ordered_logistic,pars=c("c","team_abil"))

## Extraction of model parameters
params_final_ordered_logistic<-extract(final_ordered_logistic)

c_final_ordered_logistic<-params_final_ordered_logistic$c
team_abil_final_ordered_logistic<-params_final_ordered_logistic$team_abil
colnames(team_abil_final_ordered_logistic)[1:12]<-c(teams_names)[1:12]

# All together model parameters

all_params_final_ordered_logistic<-cbind(c_final_ordered_logistic,
	team_abil_final_ordered_logistic)

# Convertion to mcmc objects

all_params_final_ordered_logistic_mcmc<-mcmc(all_params_final_ordered_logistic)

# Raftery Diagnostics

raftery.diag(all_params_final_ordered_logistic_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)

#Table with these measures

converg.diag_matrix_final_ordered_logistic<-cbind(params_final_ordered_logistic_summary$summary[,c(9,10)],raftery.diag(all_params_final_ordered_logistic_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix)

converg.diag_matrix_final_ordered_logistic<-cbind(round(params_final_ordered_logistic_summary$summary[,c(9)]),round(params_final_ordered_logistic_summary$summary[,c(10)],2),round(raftery.diag(all_params_final_ordered_logistic_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,2))
colnames(converg.diag_matrix_final_ordered_logistic)[c(1,2)]<-c("n_eff","Rhat")

# LateX table

xtable(converg.diag_matrix_final_ordered_logistic,digits=0,"MCMC Convergence diagnostics for the Ordered Multinomial Logistic")



## Separate Chains

params_final_ordered_logistic_summary<-summary(final_ordered_logistic,pars=c("c","team_abil"))
# Extraction of model parameters
params_final_ordered_logistic<-extract(final_ordered_logistic,permuted=F)

c_final_ordered_logistic<-extract(final_ordered_logistic,permuted=F,pars="c")
c_final_ordered_logistic<-as.data.frame(c_final_ordered_logistic)


team_abil_final_ordered_logistic<-extract(final_ordered_logistic,permuted=F,pars="team_abil")
team_abil_final_ordered_logistic<-as.data.frame(team_abil_final_ordered_logistic)

# All together model parameters

all_params_final_ordered_logistic<-cbind(c_final_ordered_logistic,
	team_abil_final_ordered_logistic)

# Convertion to mcmc objects

all_params_final_ordered_logistic_mcmc<-mcmc(all_params_final_ordered_logistic)

# Raftery Diagnostics

raftery.diag(all_params_final_ordered_logistic_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)

#Table with these measures


converg.diag_matrix_final_ordered_logistic<-cbind(round(params_final_ordered_logistic_summary$summary[,c(9)]),round(params_final_ordered_logistic_summary$summary[,c(10)],2),round(raftery.diag(all_params_final_ordered_logistic_mcmc, q=0.025, r=0.005, s=0.95, converge.eps=0.001)$resmatrix,2))
colnames(converg.diag_matrix_final_ordered_logistic)[c(1,2)]<-c("n_eff","Rhat")

# LateX table

xtable(converg.diag_matrix_final_ordered_logistic,digits=0,"MCMC Convergence diagnostics for the Ordered Multinomial Logistic")



#######-----Nuts HMC diagnostics------####### Merging chains
np <- nuts_params(final_ordered_logistic)
lp <- log_posterior(final_ordered_logistic)

color_scheme_set("brightblue")

plot1<-mcmc_nuts_acceptance(np, lp)
plot2<-mcmc_nuts_divergence(np, lp)

grid.arrange(plot1,plot2,ncol=2)
mcmc_nuts_divergence(np, lp)
mcmc_nuts_stepsize(np, lp)
mcmc_nuts_treedepth(np, lp)

color_scheme_set("red")
mcmc_nuts_energy(np)
mcmc_nuts_energy(np, merge_chains = TRUE, binwidth = .15)
mcmc_nuts_energy(np) +
  facet_wrap(~ Chain, nrow = 1) +
  coord_fixed(ratio = 150) +
  ggtitle("NUTS Energy Diagnostic")
# }

