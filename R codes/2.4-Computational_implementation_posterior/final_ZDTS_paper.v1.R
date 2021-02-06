## Load some useful libraries
library(shinystan)
library(skellam)
library(rstan)
library(loo)
library(ggplot2)
library(MASS)
library(gridExtra)
library(xtable)
library(plyr)
library(bayesplot)
library(BayesianTools)##For DIC
library(coda)##For convergence diagnostics


##Load some proper datasets

#load(file.choose())#/Data/new_volley
#load(file.choose())#/Data/datafr_teams_scores_set_win

## Vector of teams names along with
## their ranking positions, points, abilities

observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")

teams_names_attack<-teams_names_over<-teams_names_defense<-teams_names_points<-teams_names_pos<-teams_names<-NULL
for (i in 1:length(levels(new_volley$home_team))) {
	teams_names_attack[i]<-paste(levels(new_volley$home_team)[i],"Attack",sep=" ")
	teams_names_defense[i]<-paste(levels(new_volley$home_team)[i],"Defense",sep=" ")
	teams_names_over[i]<-paste(levels(new_volley$home_team)[i],"Overall",sep=" ")

	teams_names_pos[i]<-paste(levels(new_volley$home_team)[i],observed_positions[i])
	teams_names_points[i]<-paste(levels(new_volley$home_team)[i],observed_points[i])
	teams_names[i]<-c(levels(new_volley$home_team)[i])
}
teams_names_attack
teams_names_defense
teams_names_over
teams_names_pos
teams_names_points
teams_names

## Datalist for Bayesian Models via RStan
dataList<-list(n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))


#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) ZDTS (Paper v1)--------------#################

Final_ZDTS_paper.v1<-stan(file.choose(),data=dataList,chains=3,thin=5,
iter=40000,warmup=10000,
cores=3,seed="12345",init_r=1)## Stan codes/ZDTS_paper.v1.stan

#Check the initial values
inits<-get_inits(Final_ZDTS_paper.v1)
inits1<-inits[[1]]
print(inits1)


# In terms of convenience, we save the results 
# of ZDTS model fitting in order to reproduce the result 
# by simply loading these results and proceed to further remaining analysis
#save(Final_ZDTS_paper.v1,file="Final_ZDTS_paper.v1 (init_r=1,seed=12345)")##call with data, attach, load

#load(file.choose())##Output/Stan Models/Final_ZDTS_paper.v1 (init_r=1,seed=12345)"

##Rename the output of the model in terms of our convienience
final_ZDTS_paper.v1<-Final_ZDTS_paper.v1
#divergent transitions
rstan::check_divergences(final_ZDTS_paper.v1)# no divergent transitions



#####-----------------------Information Criteria-----------------------######

####Extraction of the log-likelihood, deviance quantities
deviance_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1,pars="dev")

log_lik_final_ZDTS_paper.v1<- extract_log_lik(final_ZDTS_paper.v1)
r_eff_log_lik_final_ZDTS_paper.v1<- relative_eff(exp(log_lik_final_ZDTS_paper.v1),chain_id=rep(1:3,each=6000))


# Printing the mean deviance
 print(mean(deviance_final_ZDTS_paper.v1$dev))

#### Calculation of the DIC (Gelman,2004)
#### DIC_Gelman<-function(dev){
#### 	res<-mean(dev)+0.5*var(dev)
#### 	return(res)
#### }

#####----------------------Posterior summary----------------------------######

names(final_ZDTS_paper.v1)[25:36]<-c(teams_names_attack)[1:12]
names(final_ZDTS_paper.v1)[37:48]<-c(teams_names_defense)[1:12]
names(final_ZDTS_paper.v1)[710:721]<-c(teams_names_over)[1:12]

print(final_ZDTS_paper.v1, pars=c("mu","home","attack","defense","overall","dev"),probs = c(0.025,0.5,0.975), digits=2)

## Posterior summary diagnostics

## Stan interface for both summary and convergence diagnostics
launch_shinystan(final_ZDTS_paper.v1)


## Extraction of model parameters
params_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1)

mu_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
home_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
attack_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$attack
defense_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$defense
overall_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$overall

# All together model parameters

all_params_final_ZDTS_paper.v1<-cbind(mu_final_ZDTS_paper.v1,home_final_ZDTS_paper.v1,
	attack_final_ZDTS_paper.v1,defense_final_ZDTS_paper.v1)


lambda1_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda1
lambda2_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$lambda2

## Order of ability parameters (based on the posterior means)
attack_final_ZDTS_paper.v1_means<-apply(attack_final_ZDTS_paper.v1,2,mean) ##
defense_final_ZDTS_paper.v1_means<-apply(defense_final_ZDTS_paper.v1,2,mean) ##
overall_final_ZDTS_paper.v1_means<-apply(overall_final_ZDTS_paper.v1,2,mean) ##

attack_order_final_ZDTS_paper.v1<-order(attack_final_ZDTS_paper.v1_means,decreasing=T)
defense_order_final_ZDTS_paper.v1<-order(defense_final_ZDTS_paper.v1_means,decreasing=T)
overall_order_final_ZDTS_paper.v1<-order(overall_final_ZDTS_paper.v1_means,decreasing=T)



##Posterior 95% uncertainty intervals

## Renaming model parameters in terms of convenience in both tables and graphs

## Teams names
names(final_ZDTS_paper.v1)[25:36]<-c(teams_names)[1:12]
names(final_ZDTS_paper.v1)[37:48]<-c(teams_names)[1:12]
names(final_ZDTS_paper.v1)[710:721]<-c(teams_names)[1:12]

colnames(attack_final_ZDTS_paper.v1)<-c(teams_names)[1:12]
colnames(defense_final_ZDTS_paper.v1)<-c(teams_names)[1:12]
colnames(overall_final_ZDTS_paper.v1)<-c(teams_names)[1:12]

colnames(all_params_final_ZDTS_paper.v1)[1]<-c("mu")
colnames(all_params_final_ZDTS_paper.v1)[2]<-c("home")
colnames(all_params_final_ZDTS_paper.v1)[3:14]<-c(teams_names_attack)[1:12]
colnames(all_params_final_ZDTS_paper.v1)[15:26]<-c(teams_names_defense)[1:12]




## Teams names+points

names(final_ZDTS_paper.v1)[25:36]<-c(teams_names_points)[1:12]
names(final_ZDTS_paper.v1)[37:48]<-c(teams_names_points)[1:12]

## Or Teams names+positions
names(final_ZDTS_paper.v1)[25:36]<-c(teams_names_pos)[1:12]
names(final_ZDTS_paper.v1)[37:48]<-c(teams_names_pos)[1:12]

colnames(attack_final_ZDTS_paper.v1)<-c(teams_names_pos)[1:12]
colnames(defense_final_ZDTS_paper.v1)<-c(teams_names_pos)[1:12]
colnames(overall_final_ZDTS_paper.v1)<-c(teams_names_pos)[1:12]


## Data frame of parameters in terms of convenience in both tables and graphs

posterior_mu_home<-data.frame(mu_final_ZDTS_paper.v1,home_final_ZDTS_paper.v1)
colnames(posterior_mu_home)<-c("mu","home")

posterior_attack<-as.data.frame(attack_final_ZDTS_paper.v1)
posterior_defense<-as.data.frame(defense_final_ZDTS_paper.v1)
posterior_overall<-as.data.frame(overall_final_ZDTS_paper.v1)



## Convertion to array (necessary for the summaries)
array_posterior_final_ZDTS_paper.v1<-as.array(final_ZDTS_paper.v1)

