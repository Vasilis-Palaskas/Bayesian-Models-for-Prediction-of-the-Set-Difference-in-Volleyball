


#Load the proper dataset

#load(file.choose())#/Data/new_volley
#load(file.choose())#/Data/datafr_teams_scores_set_win

####Here I keep in a vector the names of  the teams along with their positions and points
###to define more appropriate the names of parameters

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

dif_sets=datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score
#### Creation of data list for running model through stan code and brms

train_dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
dif_sets=datafr_teams_scores_set_win$home_score[1:(length(datafr_teams_scores_set_win$home_Team)/2)]-datafr_teams_scores_set_win$away_score[1:(length(datafr_teams_scores_set_win$home_Team)/2)],
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]))

test_dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
dif_sets=datafr_teams_scores_set_win$home_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]-datafr_teams_scores_set_win$away_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)],
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]))

#######---------- Proper recoding of response variable (ordinality) in both training and test sets
for (i in 1:train_dataList_new$n_games) {
	if (train_dataList_new$dif_sets[i]==1) {
		train_dataList_new$dif_sets[i]=4
	} else if (train_dataList_new$dif_sets[i]==2){
		train_dataList_new$dif_sets[i]=5
	} else if (train_dataList_new$dif_sets[i]==3){
		train_dataList_new$dif_sets[i]=6
	} else if (train_dataList_new$dif_sets[i]==-1){
	  train_dataList_new$dif_sets[i]=3
	} else if (train_dataList_new$dif_sets[i]==-2){
	  train_dataList_new$dif_sets[i]=2
	} else if (train_dataList_new$dif_sets[i]==-3){
	  train_dataList_new$dif_sets[i]=1
	}
}

for (i in 1:test_dataList_new$n_games) {
	if (test_dataList_new$dif_sets[i]==1) {
		test_dataList_new$dif_sets[i]=4
	} else if (test_dataList_new$dif_sets[i]==2){
		test_dataList_new$dif_sets[i]=5
	} else if (test_dataList_new$dif_sets[i]==3){
		test_dataList_new$dif_sets[i]=6
	} else if (test_dataList_new$dif_sets[i]==-1){
	  test_dataList_new$dif_sets[i]=3
	} else if (test_dataList_new$dif_sets[i]==-2){
	  test_dataList_new$dif_sets[i]=2
	} else if (test_dataList_new$dif_sets[i]==-3){
	  test_dataList_new$dif_sets[i]=1
	}
}

train_dataList_new_final<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
dif_sets=train_dataList_new$dif_sets,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]))

test_dataList_new_final<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
dif_sets=test_dataList_new$dif_sets,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]))


model_dataList_new_final<-list(n_teams=12,n_games_train=train_dataList_new_final$n_games,
n_games_test=test_dataList_new_final$n_games,
dif_sets=train_dataList_new$dif_sets,
home_team_train=train_dataList_new_final$home_team,home_team_test=test_dataList_new_final$home_team,
away_team_train=train_dataList_new_final$away_team,away_team_test=test_dataList_new_final$away_team)
##----- Team Abilities (rstan)-----## Baseline +3


out_of_sample_final_ordered_logistic<-stan(file.choose(),data=model_dataList_new_final,chains=3,thin=2,cores=3,
                                                        iter=16000,warmup=4000)###Run out_of_sample_final_ordered_logistic_team_abil.stan


#save(out_of_sample_final_ordered_logistic,file="Out of Sample Final Ordered Logistic")



#load(file.choose())#Output\Models\Out of Sample Final Ordered Logistic


#model<-out_of_sample_final_ordered_logistic
#train_dataset<-train_dataList_new_final
#test_dataset<-test_dataList_new_final

##By having the same data set for both training and test data set, we implement the in sample diagnostics
##By having different data sets for the training and test data sets, we implement the out-of-sample prediction


out_of_sample_prediction_Ordered<-function(train_dataset,test_dataset,model){
  
  ##### Extraction of model parameters
  params_out_of_sample_final_ordered_logistic<-rstan::extract(model)
  
  
  c_out_of_sample_final_ordered_logistic<-params_out_of_sample_final_ordered_logistic$c
  team_abil_out_of_sample_final_ordered_logistic<-params_out_of_sample_final_ordered_logistic$team_abil  

  
  

####Proper transformations so that observed dif-sets and pred_dif sets be compatitible
pred_differences_ord<-params_out_of_sample_final_ordered_logistic$y_pred

transform_pred_diff_ord<-matrix(nrow=dim(pred_differences_ord)[1],ncol=dim(pred_differences_ord)[2])## the y on original scale [-3,3]
for (i in 1:dim(pred_differences_ord)[1]) {
	for (j in 1:dim(pred_differences_ord)[2]){

  if (pred_differences_ord[i,j]==3) {
   transform_pred_diff_ord[i,j]=-1
  } else if (pred_differences_ord[i,j]==2){
    transform_pred_diff_ord[i,j]=-2
  } else if (pred_differences_ord[i,j]==1){
    transform_pred_diff_ord[i,j]=-3
  } else if  (pred_differences_ord[i,j]==4) {
   transform_pred_diff_ord[i,j]=1
  } else if (pred_differences_ord[i,j]==5) {
    transform_pred_diff_ord[i,j]=2
  } else if (pred_differences_ord[i,j]==6) {
    transform_pred_diff_ord[i,j]=3
  }	
 }
}
 
  #####------Regeneration of the league (by rmultinom differences)------########
  
  matrix_rows<-dim(transform_pred_diff_ord)[1]*test_dataset$n_games
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=test_dataset$n_teams)
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  
  for (j in 0:(dim(transform_pred_diff_ord)[1]-1)){
    
    for (i in 1:test_dataset$n_games) {
      
      if (transform_pred_diff_ord[j+1,i]>1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      } else if (transform_pred_diff_ord[j+1,i]==1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (transform_pred_diff_ord[j+1,i]<(-1))	{
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (transform_pred_diff_ord[j+1,i]==(-1)) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      }
    }
    
  }
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  ######-----Total points-----########
  post_distr_total_points<-matrix(nrow=dim(transform_pred_diff_ord)[1],ncol=test_dataset$n_teams)
  
  for (j in 0:(dim(transform_pred_diff_ord)[1]-1)){
    for (i in 1:test_dataset$n_teams) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$n_games+1):(j*test_dataset$n_games+test_dataset$n_games),i])
      
    }
  }
  
  
  
  # ------------------------------------------------------------------------------------------------------------------------------------------
  ##########--------Mean total points--------#########
  ###------- We extract the mean from the posterior of total points-------#######
  mean_pred_total_points<-NULL
  for (i in 1:12){
    mean_pred_total_points[i]<-mean(post_distr_total_points[,i])
  }
  teams_names
  ranking_datafr<-data.frame(teams_names,mean_pred_total_points)
  ord_ranking_datafr<-ranking_datafr[order(ranking_datafr$mean_pred_total_points,decreasing=T),]
  
  
  ##########-----------observed ranking (Test)-----------############
  #### Now we will see the proportion of success####
  obs_dif_test<-test_dataset$dif_sets
  matrix_obs_game_points<-matrix(nrow=test_dataset$n_games,ncol=test_dataset$n_teams)
  
  for (i in 1:test_dataset$n_games){
    if (obs_dif_test[i]>4) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-3
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-0
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]==4) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-2
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-1
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]<(3))	{
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-0
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-3
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]==(3)) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-1
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-2
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    }
  }
  
  
  
  
  
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  matrix_obs_game_points
  
  
  
  
  ######-----Total Points-----########
  obs_total_points_test<-NULL
  
  for (i in 1:test_dataset$n_teams) {
    obs_total_points_test[i]<-sum(matrix_obs_game_points[,i])
  }
  obs_ranking_datafr_test<-data.frame(teams_names,obs_total_points_test)
  final_obs_ranking_datafr_test<-obs_ranking_datafr_test[order(obs_ranking_datafr_test$obs_total_points_test,decreasing=T),]
  



  			return(list(pred_differences_out=transform_pred_diff_ord,
			multi_pred_differences_out=pred_differences_ord,
			pred_team_points=post_distr_total_points,
			pred_ranking_test=ord_ranking_datafr,
              obs_ranking_test=final_obs_ranking_datafr_test,
			obs_team_points_test=obs_total_points_test))
}




###########-------------------------In Sample prediction results--------------------###############
out_of_sample_results_ordered<-out_of_sample_prediction_Ordered(train_dataList_new_final,test_dataList_new_final,
                                         out_of_sample_final_ordered_logistic)
											
	pred_differences_out<-out_of_sample_results_ordered$pred_differences_out
	pred_team_points<-out_of_sample_results_ordered$pred_team_points
     pred_ranking<-out_of_sample_results_ordered$pred_ranking_test
	multi_pred_differences_out<-out_of_sample_results_ordered$multi_pred_differences_out
	obs_team_points_test<-out_of_sample_results_ordered$obs_team_points_test
	
	#save(pred_differences_out,file="Out of sample Predicted differences (ordered)")
	#save(multi_pred_differences_out,file="Out of sample Predicted differences (ordered) (Multinomial values)")
	#save(pred_ranking,file="Out of sample Predicted ranking (ordered)")
	#save(pred_team_points,file="Out of sample Predicted Points (ordered)")
	#save(obs_team_points_test,file="Out of sample Observed Points test(ordered)")


################--------------out_of_sample_deviance measures-------------###############
### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics

   #  load(file.choose())#Output\MAD Comparison (In and Out of Sample)\Out-of-Sample\Out of sample ordered\Out of sample Predicted differences (ordered)

	#load(file.choose())#Output\MAD Comparison (In and Out of Sample)\Out-of-Sample\Out of sample ordered\Out of sample Predicted differences (ordered)(Multinomial values)


	#load(file.choose())#Output\MAD Comparison (In and Out of Sample)\Out-of-Sample\Out of sample ordered\Out of sample Predicted ranking (ordered)

	#load(file.choose())#Output\MAD Comparison (In and Out of Sample)\Out-of-Sample\Out of sample ordered\Out of sample Predicted Points (ordered)

	#load(file.choose())#Output\MAD Comparison (In and Out of Sample)\Out-of-Sample\Out of sample ordered\Out-of-sample Observed Points Test set (ordered)



##########-------Expected Points----------########


observed_points_dev<-obs_team_points_test


out_of_sample_deviance_points_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:test_dataList_new_final$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_team_points[i,j]-observed_points_dev[j])
  }
  out_of_sample_deviance_points_ordered[i]<-(1/length(observed_points_dev))* sum_dev[i]
}
mean(out_of_sample_deviance_points_ordered)##3.35




##########-------Expected set differences----------########

observed_set_diff_dev<-datafr_teams_scores_set_win$home_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]-datafr_teams_scores_set_win$away_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]

out_of_sample_deviance_set_diff_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:length(observed_set_diff_dev)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_differences_out[i,j]-observed_set_diff_dev[j])
   
}
  out_of_sample_deviance_set_diff_ordered[i]<-(1/length(observed_set_diff_dev))* sum_dev[i]
}
mean(out_of_sample_deviance_set_diff_ordered)###1.58



##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out
pred_differences_out


length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))

pred_freq_ordered<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_freq_ordered_iter<-matrix(pred_freq_ordered,nrow=nrow(pred_differences_out),6,byrow=T)
pred_freq_ordered_iter[which(is.na(pred_freq_ordered_iter))]<-0
colnames(pred_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_ordered_iter_matrix<-matrix(pred_freq_ordered_iter,nrow=nrow(pred_freq_ordered_iter),6)

out_of_sample_deviance_freq_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_freq_ordered_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_freq_ordered_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_freq_ordered_iter_matrix[i,j]-observed_freq[j])
  }
  out_of_sample_deviance_freq_ordered[i]<-(1/ncol(pred_freq_ordered_iter_matrix))* sum_dev[i]
}
mean(out_of_sample_deviance_freq_ordered)##3,34


##########-------Relative Frequencies----------########
##########

###Predicted set differences###
multi_pred_differences_out
pred_differences_out

length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_ordered<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_rel_freq_ordered_iter<-matrix(pred_freq_ordered/test_dataList_new_final$n_games,nrow=nrow(pred_differences_out),6,byrow=T)
pred_rel_freq_ordered_iter[which(is.na(pred_rel_freq_ordered_iter))]<-0

colnames(pred_rel_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_ordered_iter_matrix<-matrix(pred_rel_freq_ordered_iter,nrow=nrow(pred_rel_freq_ordered_iter),6)



out_of_sample_deviance_rel_freq_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_rel_freq_ordered_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_rel_freq_ordered_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_rel_freq_ordered_iter_matrix[i,j]-(observed_freq[j]/test_dataList_new_final$n_games))
  }
  out_of_sample_deviance_rel_freq_ordered[i]<-(1/ncol(pred_freq_ordered_iter_matrix))* sum_dev[i]
}
mean(out_of_sample_deviance_rel_freq_ordered)##5.07
sd(out_of_sample_deviance_rel_freq_ordered)##1.55


#------------------------------------------------------------
#####----------Expected - Observed set difference (per team)------------###########
pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out),ncol=test_dataList_new$n_games)






test_dataList<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
home_sets=datafr_teams_scores_set_win$home_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)],
away_sets=datafr_teams_scores_set_win$away_score[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)],
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[(length(datafr_teams_scores_set_win$home_Team)/2+1):length(datafr_teams_scores_set_win$home_Team)]))


for (i in 1:nrow(pred_differences_out)){
	for (j in 1:test_dataList$n_games){

		if (pred_differences_out[i,j]==3){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-0
		} else if (pred_differences_out[i,j]==2){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-1
		}else if (pred_differences_out[i,j]==1){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-2
		}else if (pred_differences_out[i,j]==(-1)){
			pred_home_sets[i,j]<-2
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out[i,j]==(-2)){
			pred_home_sets[i,j]<-1
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out[i,j]==(-3)){
			pred_home_sets[i,j]<-0
			pred_away_sets[i,j]<-3
		}
	}
}


######------Observed set difference--------##########
observed_set_diff<-win_sets<-lose_sets<-NULL

for (i in 1:test_dataList$n_teams){
	win_sets[i]<-sum(test_dataList$home_sets[test_dataList$home_team==i])+
					sum(test_dataList$away_sets[test_dataList$away_team==i])
	lose_sets[i]<-sum(test_dataList$away_sets[test_dataList$home_team==i])+
					sum(test_dataList$home_sets[test_dataList$away_team==i])
	observed_set_diff[i]<-win_sets[i]-lose_sets[i]
}

######------Generated set difference--------##########

win_pred_sets<-lose_pred_sets<-pred_set_diff<-matrix(nrow=nrow(pred_differences_out),ncol=test_dataList$n_teams)

for (i in 1:nrow(pred_differences_out)){
	for (j in 1:test_dataList$n_teams){
		win_pred_sets[i,j]<-sum(pred_home_sets[i,which(test_dataList$home_team==j)])+
					sum(pred_away_sets[i,which(test_dataList$away_team==j)])
		lose_pred_sets[i,j]<-sum(pred_away_sets[i,which(test_dataList$home_team==j)])+
					sum(pred_home_sets[i,which(test_dataList$away_team==j)])

		pred_set_diff[i,j]<-win_pred_sets[i,j]-lose_pred_sets[i,j]
	}
}


##########-------Expected Total set differences----------########

out_of_sample_deviance_total_set_diff_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:test_dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_set_diff[i,j]-observed_set_diff[j])
  }
 out_of_sample_deviance_total_set_diff_ordered[i]<-(1/test_dataList$n_teams)* sum_dev[i]
}

mean(out_of_sample_deviance_total_set_diff_ordered)##6,14



### Save the results in order to use them in comparison with ones of ZDTS
#save(out_of_sample_deviance_total_set_diff_ordered,file="out_of_sample_deviance_total_set_diff_ordered")
#save(out_of_sample_deviance_points_ordered,file="out_of_sample_deviance_points_ordered")
#save(out_of_sample_deviance_set_diff_ordered,file="out_of_sample_deviance_set_diff_ordered")
#save(out_of_sample_deviance_rel_freq_ordered,file="out_of_sample_deviance_rel_freq_ordered")
#save(out_of_sample_deviance_freq_ordered,file="out_of_sample_deviance_freq_ordered")



