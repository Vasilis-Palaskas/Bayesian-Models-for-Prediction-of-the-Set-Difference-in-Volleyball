


#Load the proper dataset

#load(file.choose())#/Data/new_volley
#load(file.choose())#/Data/datafr_teams_scores_set_win

#### Here I keep in a vector the names of  the teams along with
#### the corresponding positions, points
#### to defenseine more appropriate the names of parameters
options(mc.cores = parallel::detectCores())
observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")

teams_names_attack<-teams_names_over<-teams_names_defense<-teams_names_points<-teams_names_pos<-teams_names<-NULL
for (i in 1:12) {
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

##---- Datalist for the out-of-sample prediction of the second halö of the mid-season---###

train_dataList<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
home_sets=datafr_teams_scores_set_win$home_score[1:(length(datafr_teams_scores_set_win$home_Team)/2)],
away_sets=datafr_teams_scores_set_win$away_score[1:(length(datafr_teams_scores_set_win$home_Team)/2)],
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[1:(length(datafr_teams_scores_set_win$home_Team)/2)]))

test_dataList<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)/2,
home_sets=datafr_teams_scores_set_win$home_score[((length(datafr_teams_scores_set_win$home_Team)/2)+1):length(datafr_teams_scores_set_win$home_Team)],
away_sets=datafr_teams_scores_set_win$away_score[((length(datafr_teams_scores_set_win$home_Team)/2)+1):length(datafr_teams_scores_set_win$home_Team)],
home_team=as.numeric(datafr_teams_scores_set_win$home_Team[((length(datafr_teams_scores_set_win$home_Team)/2)+1):length(datafr_teams_scores_set_win$home_Team)]),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team[((length(datafr_teams_scores_set_win$home_Team)/2)+1):length(datafr_teams_scores_set_win$home_Team)]))
#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) ZDTS (Paper v1)--------------#################

##Run this model with thin=1 in order to find the proper thinning to eliminate the autocorrelation
out_of_sample_final_ZDTS_paper.v1<-stan(file.choose(),data=train_dataList,chains=3,thin=5,
iter=40000,warmup=10000,cores=3,seed="12345",init_r=1)####Run Stan Models/ZDTS_paper.v1.stan




#Check the initial values
inits<-get_inits(out_of_sample_final_ZDTS_paper.v1)
inits1<-inits[[1]]
print(inits1)


#save(out_of_sample_final_ZDTS_paper.v1,file="out_of_sample_final_ZDTS_paper.v1 (init_r=1,seed=12345)")##call with data, attach, load

#load(file.choose())##Output/Stan Models/final_out_of_sample_final_ZDTS_paper.v1



#--------------------------------------------------------------------------------------------------------------------------
#################---------------------MAD Posterior Results by using the ZDTS----------------------------################################
#load(file.choose())##Output/Stan Models/ final_ZDTS_paper.v1

#load(file.choose())##dataList







#model<- out_of_sample_final_ZDTS_paper.v1
#train_dataset<-dataList
#test_dataset<-dataList

out_of_sample_prediction_ZDTS<-function(train_dataset,test_dataset,model){
  
  ##### Extraction of model parameters
  params_final_ZDTS_paper.v1<-rstan::extract(model)
  dim(params_final_ZDTS_paper.v1$lambda1)
  
  mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
  home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
  att_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$attack
  def_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$defense
  
  #######--------We use the test data
  lambda1_out_final_ZDTS_paper.v1<-lambda2_out_final_ZDTS_paper.v1<-matrix(nrow=dim(att_params_final_ZDTS_paper.v1)[1],ncol=test_dataset$n_games)
  for (i in 1:test_dataset$n_games){
    for (j in 1:dim(lambda1_out_final_ZDTS_paper.v1)[1]){
      lambda1_out_final_ZDTS_paper.v1[j,i]<-exp(mu_params_final_ZDTS_paper.v1[j]+home_params_final_ZDTS_paper.v1[j]+
                                          att_params_final_ZDTS_paper.v1[j,test_dataset$home_team[i]]+
                                          def_params_final_ZDTS_paper.v1[j,test_dataset$away_team[i]])
      lambda2_out_final_ZDTS_paper.v1[j,i]<-exp(mu_params_final_ZDTS_paper.v1[j]+
                                          att_params_final_ZDTS_paper.v1[j,test_dataset$away_team[i]]+
                                          def_params_final_ZDTS_paper.v1[j,test_dataset$home_team[i]])
    }
  }
  
  
  #### ----Out-of-sample Predicted differences from ZDS---#########
  #########---------Generation of values from ZDTS---------###########
  
  ####----Now we start to generate the predicted values------#####
  
  multi_pred_differences_out_final_ZDTS_paper.v1<-matrix(nrow=nrow(lambda2_out_final_ZDTS_paper.v1),ncol=ncol(lambda2_out_final_ZDTS_paper.v1))
  
  for (i in 1:ncol(lambda2_out_final_ZDTS_paper.v1)) {
    for (j in 1:nrow(lambda2_out_final_ZDTS_paper.v1)) {
      
	  #in order to avoid potential overflow
      if (lambda1_out_final_ZDTS_paper.v1[j,i]>700){
        lambda1_out_final_ZDTS_paper.v1[j,i]<-700
      }
      if (lambda2_out_final_ZDTS_paper.v1[j,i]>700){
        lambda2_out_final_ZDTS_paper.v1[j,i]<-700
      } 
      numer<-c(dskellam(-3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(-2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(-1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]),
               dskellam(3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i]))
      
      denom<-dskellam(-3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(-2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(-1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(1,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(2,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])+
        dskellam(3,lambda1_out_final_ZDTS_paper.v1[j,i],lambda2_out_final_ZDTS_paper.v1[j,i])
      if (denom==0){
        denom<-0.001
      }
      
      prob<-numer/denom
      x<-rmultinom(1,1,prob)
      multi_pred_differences_out_final_ZDTS_paper.v1[j,i]<-which(x[,1]==1)
    }
  }
  
  ###Transformation to the scale of negative and positive differences
  pred_differences_out_final_ZDTS_paper.v1<-matrix(nrow=nrow(multi_pred_differences_out_final_ZDTS_paper.v1
  ),ncol=ncol(multi_pred_differences_out_final_ZDTS_paper.v1))
  dim(multi_pred_differences_out_final_ZDTS_paper.v1)
  
  for (j in 1:nrow(multi_pred_differences_out_final_ZDTS_paper.v1
  )){
    for (i in 1:ncol(multi_pred_differences_out_final_ZDTS_paper.v1)) {
      
      if (multi_pred_differences_out_final_ZDTS_paper.v1
          [j,i]==4) {
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-1
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==5){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-2
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==6){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<-3
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==1){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--3
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==2){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--2
      } else if (multi_pred_differences_out_final_ZDTS_paper.v1
                 [j,i]==3){
        pred_differences_out_final_ZDTS_paper.v1[j,i]<--1
      }
    }
  }

  


  
  #####------Regeneration of the league (by rmultinom differences)------########
  
  matrix_rows<-dim(pred_differences_out_final_ZDTS_paper.v1)[1]*test_dataset$n_games
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=test_dataList$n_teams)
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  
  for (j in 0:(dim(pred_differences_out_final_ZDTS_paper.v1)[1]-1)){
    
    for (i in 1:test_dataset$n_games) {
      
      if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]>1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]==1) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]<(-1))	{
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-3
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } else if (pred_differences_out_final_ZDTS_paper.v1[j+1,i]==(-1)) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-2
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      }
    }
    
  }
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  ######-----Total points-----########
  post_distr_total_points<-matrix(nrow=dim(pred_differences_out_final_ZDTS_paper.v1)[1],ncol=test_dataList$n_teams)
  
  for (j in 0:(dim(pred_differences_out_final_ZDTS_paper.v1)[1]-1)){
    for (i in 1:test_dataList$n_teams) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$n_games+1):(j*test_dataset$n_games+test_dataset$n_games),i])
      
    }
  }
  
  
  
    ######------------------------------------------------------------------------------------------------------------------------------------------
  ##########--------Mean total points--------#########
  ###------- We extract the mean from the posterior of total points-------#######
  mean_pred_total_points<-NULL
  for (i in 1:test_dataList$n_teams){
    mean_pred_total_points[i]<-mean(post_distr_total_points[,i])
  }
  teams_names
  ranking_datafr<-data.frame(teams_names,mean_pred_total_points)
  ord_ranking_datafr<-ranking_datafr[order(ranking_datafr$mean_pred_total_points,decreasing=T),]
  
  



  ##########-----------observed ranking (Test)-----------############
  #### Now we will see the proportion of success####
  obs_dif_test<-test_dataset$home_sets-test_dataset$away_sets
  matrix_obs_game_points<-matrix(nrow=test_dataset$n_games,ncol=test_dataset$n_teams)
  
  for (i in 1:test_dataset$n_games){
    if (obs_dif_test[i]>1) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-3
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-0
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]==1) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-2
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-1
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]<(-1))	{
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-0
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-3
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    } else if (obs_dif_test[i]==(-1)) {
      matrix_obs_game_points[i,test_dataset$home_team[i]]<-1
      matrix_obs_game_points[i,test_dataset$away_team[i]]<-2
      matrix_obs_game_points[i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
    }
  }
  
  
  
  
  
  
  
  ######## This is the matrix with number of games in rows and number of teams in col####
  matrix_obs_game_points
  
  
  
  
  ######-----Total Points-----########
  obs_total_points_test<-NULL
  
  for (i in 1:test_dataList$n_teams) {
    obs_total_points_test[i]<-sum(matrix_obs_game_points[,i])
  }
  obs_ranking_datafr_test<-data.frame(teams_names,obs_total_points_test)
  final_obs_ranking_datafr_test<-obs_ranking_datafr_test[order(obs_ranking_datafr_test$obs_total_points_test,decreasing=T),]
  



  ######
  ######----Predicted differences : multi_pred_differences_out_final_ZDTS_paper.v1
  ######----Predicted points:pred_team_points

			return(list(pred_differences_out=pred_differences_out_final_ZDTS_paper.v1,
			multi_pred_differences_out=multi_pred_differences_out_final_ZDTS_paper.v1,
			pred_team_points=post_distr_total_points,
			pred_ranking_test=ord_ranking_datafr,
             obs_ranking_test=final_obs_ranking_datafr_test,
			 obs_team_points_test=obs_total_points_test))
}


###########-------------------------In Sample prediction results--------------------###############
out_of_sample_results_ZDTS<-out_of_sample_prediction_ZDTS(train_dataList,test_dataList,
                                        out_of_sample_final_ZDTS_paper.v1)
											
	pred_differences_out<-out_of_sample_results_ZDTS$pred_differences_out
	pred_team_points<-out_of_sample_results_ZDTS$pred_team_points
     pred_ranking <-out_of_sample_results_ZDTS$pred_ranking_test
	multi_pred_differences_out<-out_of_sample_results_ZDTS$multi_pred_differences_out
	obs_team_points_test<-out_of_sample_results_ZDTS$obs_team_points_test
	
	#save(pred_differences_out,file="Out-of-sample Predicted differences (ZDTS)")
	#save(multi_pred_differences_out,file="Out-of-sample Predicted differences (ZDTS)(Multinomial values)")
	#save(pred_ranking,file="Out-of-sample Predicted ranking (ZDTS)")
	#save(pred_team_points,file="Out-of-sample Predicted Points (ZDTS)")
	#save(obs_team_points_test,file="Out-of-sample Observed Points Test set (ZDTS)")

	
	
	
	###########----------Posterior summaries of MAD----------------###########################

################--------------out_of_sample_deviance measures-------------###############
### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics

    # load(file.choose())#/Output/MAD/Out of sample Predicted differences (ZDTS)

	#load(file.choose())#/Output/MAD/Out of sample Predicted differences (ZDTS)(Multinomial values)


	#load(file.choose())#/Output/MAD/Out of sample Predicted ranking (ZDTS)

	#load(file.choose())#/Output/MAD/Out of sample Predicted Points (ZDTS)

	#load(file.choose())#/Output/MAD/Out-of-sample Observed Points Test set (ZDTS)

##########-------Expected Points----------########


observed_points_dev<-obs_team_points_test


out_of_sample_deviance_points_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:test_dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_team_points[i,j]-observed_points_dev[j])
  }
  out_of_sample_deviance_points_zdts[i]<-(1/length(observed_points_dev))* sum_dev[i]
}
mean(out_of_sample_deviance_points_zdts)##3,57




##########-------Expected set differences----------########


observed_set_diff_dev<-test_dataList$home_sets-test_dataList$away_sets
out_of_sample_deviance_set_diff_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:length(observed_set_diff_dev)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_differences_out[i,j]-observed_set_diff_dev[j])
   
}
  out_of_sample_deviance_set_diff_zdts[i]<-(1/length(observed_set_diff_dev))* sum_dev[i]
}
mean(out_of_sample_deviance_set_diff_zdts)###1.61



##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out
pred_differences_out

observed_freq<-as.vector(table(observed_set_diff_dev))

table(pred_differences_out[2,])
pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_freq_zdts_iter<-matrix(pred_freq_zdts,nrow=nrow(pred_differences_out),6,byrow=T)
pred_freq_zdts_iter[which(is.na(pred_freq_zdts_iter))]<-0

colnames(pred_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_zdts_iter_matrix<-matrix(pred_freq_zdts_iter,nrow=nrow(pred_freq_zdts_iter),6)



out_of_sample_deviance_freq_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_freq_zdts_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_freq_zdts_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_freq_zdts_iter_matrix[i,j]-observed_freq[j])
  }
  out_of_sample_deviance_freq_zdts[i]<-(1/ncol(pred_freq_zdts_iter_matrix))* sum_dev[i]
}
mean(out_of_sample_deviance_freq_zdts)##3.14

sd(out_of_sample_deviance_freq_zdts)#0.90


##########-------Relative Frequencies----------########

###Predicted set differences###
multi_pred_differences_out
pred_differences_out

observed_freq<-as.vector(table(observed_set_diff_dev))

pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_rel_freq_zdts_iter<-matrix(pred_freq_zdts/test_dataList$n_games,nrow=nrow(pred_differences_out),6,byrow=T)
pred_rel_freq_zdts_iter[which(is.na(pred_rel_freq_zdts_iter))]<-0

colnames(pred_rel_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_zdts_iter_matrix<-matrix(pred_rel_freq_zdts_iter,nrow=nrow(pred_rel_freq_zdts_iter),6)



out_of_sample_deviance_rel_freq_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_rel_freq_zdts_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_rel_freq_zdts_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_rel_freq_zdts_iter_matrix[i,j]-(observed_freq[j]/test_dataList$n_games))
  }
  out_of_sample_deviance_rel_freq_zdts[i]<-(1/ncol(pred_freq_zdts_iter_matrix))* sum_dev[i]
}
mean(out_of_sample_deviance_rel_freq_zdts)##4,76%
sd(out_of_sample_deviance_rel_freq_zdts)##1,37%



######----------Expected - Observed set difference (per team)------------###########
pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out),ncol=test_dataList$n_games)


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

out_of_sample_deviance_total_set_diff_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:test_dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_set_diff[i,j]-observed_set_diff[j])
  }
 out_of_sample_deviance_total_set_diff_zdts[i]<-(1/test_dataList$n_teams)* sum_dev[i]
}

mean(out_of_sample_deviance_total_set_diff_zdts)##6.34


### Save the results in order to use them in comparison with ones of ordered logistic
#save(out_of_sample_deviance_total_set_diff_zdts,file="out_of_sample_deviance_total_set_diff_zdts")
#save(out_of_sample_deviance_points_zdts,file="out_of_sample_deviance_points_zdts")
#save(out_of_sample_deviance_set_diff_zdts,file="out_of_sample_deviance_set_diff_zdts")
#save(out_of_sample_deviance_rel_freq_zdts,file="out_of_sample_deviance_rel_freq_zdts")
#save(out_of_sample_deviance_freq_zdts,file="out_of_sample_deviance_freq_zdts")

