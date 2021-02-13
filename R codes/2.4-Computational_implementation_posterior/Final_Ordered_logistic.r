##Load some useful libraries
library(rstanarm)
library(shinystan)
library(skellam)
library(rstan)
library(loo)
library(ggplot2)
library(MASS)
library(xtable)
library(plyr)
library(bayesplot)
library(coda)##For convergence diagnostics


##Load some proper datasets

#load(file.choose())#Data/new_volley
#load(file.choose())#Data/datafr_teams_scores_set_win


## Vector of teams names along with
## their ranking positions, points, abilities

observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")

teams_names_abil<-teams_names_points<-teams_names_pos<-teams_names<-NULL
for (i in 1:12) {
  teams_names_abil[i]<-paste(levels(new_volley$home_team)[i],"Abil",sep=" ")
  teams_names_pos[i]<-paste(levels(new_volley$home_team)[i],observed_positions[i])
  teams_names_points[i]<-paste(levels(new_volley$home_team)[i],observed_points[i])
  teams_names[i]<-c(levels(new_volley$home_team)[i])
}
teams_names_abil
teams_names_pos
teams_names_points
teams_names


#### Creation of data list for running model through stan code

dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team),
dif_sets=datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score,
home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))


## Proper recoding of response variable (ordinality property)
for (i in 1:dataList_new$n_games) {
	if (dataList_new$dif_sets[i]==1) {
		dataList_new$dif_sets[i]=4
	} else if (dataList_new$dif_sets[i]==2){
		dataList_new$dif_sets[i]=5
	} else if (dataList_new$dif_sets[i]==3){
		dataList_new$dif_sets[i]=6
	} else if (dataList_new$dif_sets[i]==-1){
	  dataList_new$dif_sets[i]=3
	} else if (dataList_new$dif_sets[i]==-2){
	  dataList_new$dif_sets[i]=2
	} else if (dataList_new$dif_sets[i]==-3){
	  dataList_new$dif_sets[i]=1
	}
}

## Datalist for Bayesian Models via RStan

dataList_new_final<-list(n_teams=12,n_games=132,
dif_sets=dataList_new$dif_sets,
home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))

#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) Final Ordered Logistic--------------#################

final_ordered_logistic<-stan(file.choose(),data=dataList_new_final,chains=3,
		thin=2,cores=3,
          iter=16000,warmup=4000)#Stan codes/Ordered_paper.v1.stan


#save(final_ordered_logistic,file="Final Ordered Logistic")
#load(file.choose())#Output/Models/Final Ordered Logistic


#####---------Information Criteria-------######

####Extraction of the log-likelihood, deviance quantities
deviance_final_ordered_logistic<-extract(final_ordered_logistic,pars="dev")
log_lik_final_ordered_logistic<- extract_log_lik(final_ordered_logistic)
r_eff_log_lik_final_ordered_logistic<- relative_eff(exp(log_lik_final_ordered_logistic),chain_id=rep(1:3,each=6000))


#### Calculation of the DIC (Gelman,2004)
#### DIC_Gelman<-function(dev){
#### 	res<-mean(dev)+0.5*var(dev)
#### 	return(res)
#### }


#####----------------------Posterior summary----------------------------######
## Posterior summary diagnostics
names(final_ordered_logistic)[17:28]<-c(teams_names_abil)[1:12]
print(final_ordered_logistic, pars=c("c","team_abil"),probs = c(0.025,0.5,0.975), digits=2)


## Stan interface for both summary and convergence diagnostics
launch_shinystan(final_ordered_logistic)



## Extraction of model parameters
params_final_ordered_logistic<-extract(final_ordered_logistic)

c_final_ordered_logistic<-params_final_ordered_logistic$c
team_abil_final_ordered_logistic<-params_final_ordered_logistic$team_abil

# All together model parameters

all_params_final_ordered_logistic<-cbind(c_final_ordered_logistic,
	team_abil_final_ordered_logistic)

#Posterior means for ability parameters
team_abil_final_ordered_logistic_means<-apply(team_abil_final_ordered_logistic,2,mean) ##

# Order of ability parameters (based on the posterior means)
team_abil_order_final_ordered_logistic<-order(team_abil_final_ordered_logistic_means,decreasing=T)


##Posterior 95% uncertainty intervals

## Renaming model parameters in terms of convenience in both tables and graphs
## Teams names

names(final_ordered_logistic)[1:5]<-c("c1","c2","c3","c4","c5")
names(final_ordered_logistic)[17:28]<-c(teams_names)[1:12]

colnames(team_abil_final_ordered_logistic)[1:12]<-c(teams_names)[1:12]
colnames(c_final_ordered_logistic)<-c("c1","c2","c3","c4","c5")

## Teams names+points
names(final_ordered_logistic)[17:28]<-c(teams_names_points)[1:12]

colnames(team_abil_final_ordered_logistic)<-c(teams_names_points)[1:12]
## Teams names+positions
names(final_ordered_logistic)[17:28]<-c(teams_names_pos)[1:12]
colnames(team_abil_final_ordered_logistic)<-c(teams_names_pos)[1:12]

## Data frame of parameters (merged chains) in terms of convenience in both tables and graphs

posterior_c<-as.data.frame(c_final_ordered_logistic)
posterior_team_abil<-as.data.frame(team_abil_final_ordered_logistic)

## Convertion to array (necessary for the summaries)
array_posterior_final_ordered_logistic<-as.array(final_ordered_logistic)

