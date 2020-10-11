data {
  int <lower=1> n_games; //number of games 132
  int <lower=1> n_teams; //number of teams 12
  int <lower=1,upper=n_teams> home_team[n_games];
  int <lower=1,upper=n_teams> away_team[n_games];
  int <lower=1,upper=6> dif_sets[n_games];//negative dif_sets correspond to 4(-1),5(-2),6(-3)
  
}

parameters {
  vector[n_teams - 1] team_abil_raw;
  ordered[5] c;
}

transformed parameters {
  //sum to zero constraint
  vector[n_teams] team_abil;
 
  for (t in 1:(n_teams-1)) {
    team_abil[t] = team_abil_raw[t];
  }
  
  team_abil[n_teams] = -sum(team_abil_raw);
}

model {
  
  vector[n_games] mu =team_abil[home_team]-team_abil[away_team];
  // priors including all constants 
  target += normal_lpdf(c |0,10); 
  target += normal_lpdf(team_abil_raw |0,10); 

  
  
  
  //likelihood-systematic component
  
  for (g in 1:n_games) {
    target += ordered_logistic_lpmf(dif_sets[g] | mu[g], c);
  }
  
}

generated quantities{
  vector[n_games] log_lik;
  real dev;
  int y_pred[n_games];
  vector[n_games] mu =team_abil[home_team]-team_abil[away_team];
  dev=0;
  for (i in 1:n_games) {
    log_lik[i] = ordered_logistic_lpmf(dif_sets[i] | mu[i], c);
    dev=dev-2*log_lik[i];
    y_pred[i]=ordered_logistic_rng(mu[i], c);
  }

}
