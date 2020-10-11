data {
  int <lower=1> n_games_train; //number of games in train set
  int <lower=1> n_games_test; //number of games in test set
  int <lower=1> n_teams; //number of teams 12
  int <lower=1,upper=n_teams> home_team_train[n_games_train];
  int <lower=1,upper=n_teams> away_team_train[n_games_train];
  int <lower=1,upper=n_teams> home_team_test[n_games_test];
  int <lower=1,upper=n_teams> away_team_test[n_games_test];
  int <lower=1,upper=6> dif_sets[n_games_train];//negative dif_sets correspond to 4(-1),5(-2),6(-3)\
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
  
  vector[n_games_train] mu =team_abil[home_team_train]-team_abil[away_team_train];
  // priors including all constants 
  target += normal_lpdf(c |0,10); 
  target += normal_lpdf(team_abil_raw |0,10); 
  
  
  
  
  //likelihood-systematic component
  
  for (g in 1:n_games_train) {
    target += ordered_logistic_lpmf(dif_sets[g] | mu[g], c);
  }
  
}

generated quantities{
  int y_pred[n_games_test];
  vector[n_games_test] mu =team_abil[home_team_test]-team_abil[away_team_test];
  
  for (i in 1:n_games_test) {
    y_pred[i]=ordered_logistic_rng(mu[i], c);
  }

}
