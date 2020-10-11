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
#-------------------------------------------------------------------------------------------------------------------------------
########-------------- Sensitivity Analysis--------------#################


## Different Datalists for Bayesian Models based on the value of c
## multiplied by the variance of the parameter's (home) prior distribution

dataList_c_2<-list(c=2,n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))


dataList_c_5<-list(c=5,n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))

dataList_c_10<-list(c=10,n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))

dataList_c_20<-list(c=20,n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))


# 
sensit_home.stan=
"
functions {
  
  real skellam_lpmf(int k, real mu1, real mu2) {
    real total;
    real log_prob;

    total = (- mu1 - mu2) + (log(mu1) - log(mu2)) * k / 2;
    log_prob = total + log(modified_bessel_first_kind(abs(k), 2 * sqrt(mu1*mu2)));

    return log_prob;
  }
  
  real skellam_without_lpmf(int k, real mu1, real mu2) {
    real log_prob_new;
    vector[6] lpmfs;
    real normalization;
    
    for(i in 1:3) {
      lpmfs[i] = skellam_lpmf(i - 4 | mu1, mu2);
      lpmfs[i + 3] = skellam_lpmf(i | mu1, mu2);
    }
    normalization = log_sum_exp(lpmfs);
    if (k > 3 )  {
        log_prob_new=-700;
        return log_prob_new;
    } else if (k <(-3)){
       log_prob_new=-700;
       return log_prob_new;
    } else if (k ==0){
       log_prob_new=-700;
       return log_prob_new;
    }  else {
      log_prob_new=skellam_lpmf(k|mu1,mu2)-normalization;
      return log_prob_new;
    }
    
  }
}

data {
  int c; //constant multiplied by the variance of mu parameter's prior
  int <lower=1> n_games; //number of games 132
  int <lower=1> n_teams; //number of teams 12
  int <lower=1,upper=n_teams> home_team[n_games];
  int <lower=1,upper=n_teams> away_team[n_games];
  int <lower=0,upper=3> home_sets[n_games];//0-3 sets can have each team
  int <lower=0,upper=3> away_sets[n_games];//0-3 sets can have each team
}

parameters {
  real mu;
  real home;
  real attack_raw[n_teams - 1];
  real defense_raw[n_teams - 1];
}

transformed parameters {
  // Enforce sum-to-zero constraints
  vector[n_teams]    attack;
  vector[n_teams]   defense;

  real lambda1[n_games];
  real  lambda2[n_games];  
  real lambda1_star[n_games];
  real lambda2_star[n_games];
  for (t in 1:(n_teams-1)) {
    attack[t] = attack_raw[t];
    defense[t] = defense_raw[t];
  }
  
  attack[n_teams] = -sum(attack_raw);
  defense[n_teams] = -sum(defense_raw);
  
  //likelihood-systematic component
  for (g in 1:n_games) {

    lambda1_star[g]=exp(mu+home+attack[home_team[g]]+defense[away_team[g]]);
    lambda2_star[g]=exp(mu+attack[away_team[g]]+defense[home_team[g]]);
    lambda1[g]=lambda1_star[g];
    lambda2[g]=lambda2_star[g];
  }
}


model {
  
  
  //Priors

  target+=normal_lpdf(mu|0,0.37);
  target+=normal_lpdf(home|0,0.37*sqrt(c));
  target+=normal_lpdf(attack|0,1);
  target+=normal_lpdf(defense|0,1);


  //likelihood-systematic component

  for (g in 1:n_games){
    target+=skellam_without_lpmf((home_sets[g]-away_sets[g])|lambda1[g],lambda2[g]);
    
  }
}

generated quantities{
  vector[n_games] log_lik;
  real dev;
  vector[n_teams]   overall;// overall ability

  //real DIC;

  dev=0;
  for (i in 1:n_games) {
    log_lik[i] = skellam_without_lpmf(home_sets[i]-away_sets[i] |lambda1[i],lambda2[i]);
    dev=dev-2*log_lik[i];
  }
  overall=attack-defense;
  //DIC=mean(dev)+0.5*variance(dev);
}
"

# c=2
ZDTS_paper_home_c_2<-stan(model_code=sensit_home.stan,data=dataList_c_2,thin=1,chains=1,
iter=2000,warmup=1000,seed="12345",init_r=1)


# c=5
ZDTS_paper_home_c_5<-stan(model_code=sensit_home.stan,data=dataList_c_5,thin=1,chains=1,
iter=2000,warmup=1000,seed="12345",init_r=1)

# c=10
ZDTS_paper_home_c_10<-stan(model_code=sensit_home.stan,data=dataList_c_10,thin=1,chains=1,
iter=2000,warmup=1000,seed="12345",init_r=1)

# c=20
ZDTS_paper_home_c_20<-stan(model_code=sensit_home.stan,data=dataList_c_20,thin=1,chains=1,
iter=2000,warmup=1000,seed="12345",init_r=1)

# Save the results of all fitted models

#save(ZDTS_paper_home_c_2,file="ZDTS_paper_home_c_2")
#save(ZDTS_paper_home_c_5,file="ZDTS_paper_home_c_5")
#save(ZDTS_paper_home_c_10,file="ZDTS_paper_home_c_10")
#save(ZDTS_paper_home_c_20,file="ZDTS_paper_home_c_20")




# Load the results of all fitted models for further analysis

#load(file.choose())#Output\Sensitivity analysis\home parameter\ZDTS_paper_home_c_5
#load(file.choose())#Output\Sensitivity analysis\home parameter\ZDTS_paper_home_c_10
#load(file.choose())#Output\Sensitivity analysis\home parameter\ZDTS_paper_home_c_20

 
#launch_shinystan(ZDTS_paper.v5)

#Print output of candidate models
print(ZDTS_paper_home_c_2,pars=c("home","home","attack","defense","dev"))
print(ZDTS_paper_home_c_5,pars=c("home","home","attack","defense","dev"))
print(ZDTS_paper_home_c_10,pars=c("home","home","attack","defense","dev"))
print(ZDTS_paper_home_c_20,pars=c("home","home","attack","defense","dev"))



# Extraction of the candidate models' deviances (Table 2)

#c=2
dev_sensit_home_c_2<-extract(ZDTS_paper_home_c_2,pars="dev")
dev_sensit_home_c_2$dev

mean(dev_sensit_home_c_2$dev)#358.6
sd(dev_sensit_home_c_2$dev)#6.63


#c=5
dev_sensit_home_c_5<-extract(ZDTS_paper_home_c_5,pars="dev")
dev_sensit_home_c_5$dev

mean(dev_sensit_home_c_5$dev)#359.5
sd(dev_sensit_home_c_5$dev)#6.7

#Model c=10
dev_sensit_home_c_10<-extract(ZDTS_paper_home_c_10,pars="dev")
dev_sensit_home_c_10$dev

mean(dev_sensit_home_c_10$dev)#359.2
sd(dev_sensit_home_c_10$dev)#6.4

#Model c=20
dev_sensit_home_c_20<-extract(ZDTS_paper_home_c_20,pars="dev")
dev_sensit_home_c_20$dev

mean(dev_sensit_home_c_20$dev)#358.9
sd(dev_sensit_home_c_20$dev)#6.7

