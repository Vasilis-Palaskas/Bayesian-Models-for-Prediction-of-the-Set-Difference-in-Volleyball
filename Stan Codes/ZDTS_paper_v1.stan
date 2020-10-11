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
    
      log_prob_new=skellam_lpmf(k|mu1,mu2)-normalization;
      return log_prob_new;
    
    
  }
}

data {
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
  target+=normal_lpdf(home|0,0.37);
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
