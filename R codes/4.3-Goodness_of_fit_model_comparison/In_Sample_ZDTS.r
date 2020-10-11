#--------------------------------------------------------------------------------------------------------------------------
#######---------MAD Posterior Results by using the ZDTS--------###############
#load(file.choose())##Output/Stan Models/ Final_ZDTS_paper.v1 (init_r=1,seed=12345)
#load(file.choose())##Data/dataList
#load(file.choose())##Data/new_volley

#######------- ZDTS Model with team abilities----#########

#Load proper libraries
library(tikzDevice)
library(bayesplot)
library(shinystan)
library(skellam)
library(rstan)
library(loo)


observed_positions<-c("(7)","(6)","(9)","(8)","(5)","(11)","(1)","(12)","(4)","(10)","(3)","(2)")
observed_points<-c("(36)","(37)","(16)","(28)","(38)","(14)","(62)","(7)","(39)","(16)","(50)","(53)")

teams_names_att<-teams_names_def<-teams_names_points<-teams_names_pos<-teams_names<-NULL
for (i in 1:12) {
  teams_names_att[i]<-paste(levels(new_volley$home_team)[i],"ATT",sep=" ")
  teams_names_def[i]<-paste(levels(new_volley$home_team)[i],"DEF",sep=" ")
  teams_names_pos[i]<-paste(levels(new_volley$home_team)[i],observed_positions[i])
  teams_names_points[i]<-paste(levels(new_volley$home_team)[i],observed_points[i])
  teams_names[i]<-c(levels(new_volley$home_team)[i])
}
teams_names_att
teams_names_def
teams_names_pos
teams_names_points
teams_names


## Datalist for Bayesian Models via RStan
dataList<-list(n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))



#model<- final_ZDTS_paper.v1
#train_dataset<-dataList
#test_dataset<-dataList

in_sample_prediction_ZDTS<-function(train_dataset,test_dataset,model){
  
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
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=12)
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
  post_distr_total_points<-matrix(nrow=dim(pred_differences_out_final_ZDTS_paper.v1)[1],ncol=12)
  
  for (j in 0:(dim(pred_differences_out_final_ZDTS_paper.v1)[1]-1)){
    for (i in 1:12) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$n_games+1):(j*test_dataset$n_games+test_dataset$n_games),i])
      
    }
  }
  
  
  
    ######------------------------------------------------------------------------------------------------------------------------------------------
  ##########--------Mean total points--------#########
  ###------- We extract the mean from the posterior of total points-------#######
  mean_pred_total_points<-NULL
  for (i in 1:12){
    mean_pred_total_points[i]<-mean(post_distr_total_points[,i])
  }
  teams_names
  ranking_datafr<-data.frame(teams_names,mean_pred_total_points)
  ord_ranking_datafr<-ranking_datafr[order(ranking_datafr$mean_pred_total_points,decreasing=T),]
  
  

  ######
  ######----Predicted differences : multi_pred_differences_out_final_ZDTS_paper.v1
  ######----Predicted points:pred_team_points

			return(list(pred_differences_out=pred_differences_out_final_ZDTS_paper.v1,
			multi_pred_differences_out=multi_pred_differences_out_final_ZDTS_paper.v1,
			pred_team_points=post_distr_total_points,
			pred_ranking_test=ord_ranking_datafr))
}


###########-------------------------In Sample prediction results--------------------###############
in_sample_results_ZDTS<-in_sample_prediction_ZDTS(dataList,dataList,
                                         final_ZDTS_paper.v1)
											
	pred_differences_out<-in_sample_results_ZDTS$pred_differences_out
	pred_team_points<-in_sample_results_ZDTS$pred_team_points
     pred_ranking <-in_sample_results_ZDTS$pred_ranking_test
	multi_pred_differences_out<-in_sample_results_ZDTS$multi_pred_differences_out
	
	#save(pred_differences_out,file="In sample Predicted differences (ZDTS)")
	#save(multi_pred_differences_out,file="In sample Predicted differences (ZDTS)(Multinomial values)")
	#save(pred_ranking,file="In sample Predicted ranking (ZDTS)")
	#save(pred_team_points,file="In sample Predicted Points (ZDTS)")
	
	
	
	
	###########----------Posterior summaries of MAD----------------###########################
	#load(file.choose())#/Output/MAD/In sample Predicted differences (ZDTS)

	#load(file.choose())#/Output/MAD/In sample Predicted differences (ZDTS)(Multinomial values)

	#load(file.choose())#/Output/MAD/In sample Predicted ranking (ZDTS)

	#load(file.choose())#/Output/MAD/In sample Predicted Points (ZDTS)



################--------------Deviance measures-------------###############
### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics


##########-------Expected Points----------########


observed_points_dev<-c(36,37,16,28,38,14,62,7,39,16,50,53)


deviance_points_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_team_points[i,j]-observed_points_dev[j])
  }
  deviance_points_zdts[i]<-(1/length(observed_points_dev))* sum_dev[i]
}
mean(deviance_points_zdts)##4.15




##########-------Expected set differences----------########
#load(file.choose())### Main data/ new_datalist
##########


observed_set_diff_dev<-dataList$home_sets-dataList$away_sets
deviance_set_diff_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:length(observed_set_diff_dev)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_differences_out[i,j]-observed_set_diff_dev[j])
   
}
  deviance_set_diff_zdts[i]<-(1/length(observed_set_diff_dev))* sum_dev[i]
}
mean(deviance_set_diff_zdts)###1.39



##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out
pred_differences_out


length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_freq_zdts_iter<-matrix(pred_freq_zdts,nrow=nrow(pred_differences_out),6,byrow=T)

colnames(pred_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_zdts_iter_matrix<-matrix(pred_freq_zdts_iter,nrow=nrow(pred_freq_zdts_iter),6)


deviance_freq_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_freq_zdts_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_freq_zdts_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_freq_zdts_iter_matrix[i,j]-observed_freq[j])
  }
  deviance_freq_zdts[i]<-(1/ncol(pred_freq_zdts_iter_matrix))* sum_dev[i]
}
mean(deviance_freq_zdts)##5.02



##########-------Relative Frequencies----------########
#load(file.choose())### Main data/ new_datalist
##########

###Predicted set differences###
multi_pred_differences_out
pred_differences_out

length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))

pred_freq_zdts<-NULL
for (i in 1:nrow((pred_differences_out))) {
	pred_freq_zdts<-c(pred_freq_zdts,as.vector(c(table(pred_differences_out[i,])["-3"],
	table(pred_differences_out[i,])["-2"],table(pred_differences_out[i,])["-1"],
	table(pred_differences_out[i,])["1"],table(pred_differences_out[i,])["2"],
	table(pred_differences_out[i,])["3"])))
}

pred_rel_freq_zdts_iter<-matrix(pred_freq_zdts/132,nrow=nrow(pred_differences_out),6,byrow=T)

colnames(pred_rel_freq_zdts_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_zdts_iter_matrix<-matrix(pred_rel_freq_zdts_iter,nrow=nrow(pred_rel_freq_zdts_iter),6)



deviance_rel_freq_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_rel_freq_zdts_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_rel_freq_zdts_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_rel_freq_zdts_iter_matrix[i,j]-(observed_freq[j]/dataList$n_games))
  }
  deviance_rel_freq_zdts[i]<-(1/ncol(pred_freq_zdts_iter_matrix))* sum_dev[i]
}
mean(deviance_rel_freq_zdts)##3.80%


#------------------------------------------------------------

######----------Expected - Observed set difference (per team)------------###########
pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out),ncol=dataList$n_games)


for (i in 1:nrow(pred_differences_out)){
	for (j in 1:dataList$n_games){

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

for (i in 1:dataList$n_teams){
	win_sets[i]<-sum(dataList$home_sets[dataList$home_team==i])+
					sum(dataList$away_sets[dataList$away_team==i])
	lose_sets[i]<-sum(dataList$away_sets[dataList$home_team==i])+
					sum(dataList$home_sets[dataList$away_team==i])
	observed_set_diff[i]<-win_sets[i]-lose_sets[i]
}

######------Generated set difference--------##########

win_pred_sets<-lose_pred_sets<-pred_set_diff<-matrix(nrow=nrow(pred_differences_out),ncol=dataList$n_teams)

for (i in 1:nrow(pred_differences_out)){
	for (j in 1:dataList$n_teams){
		win_pred_sets[i,j]<-sum(pred_home_sets[i,which(dataList$home_team==j)])+
					sum(pred_away_sets[i,which(dataList$away_team==j)])
		lose_pred_sets[i,j]<-sum(pred_away_sets[i,which(dataList$home_team==j)])+
					sum(pred_home_sets[i,which(dataList$away_team==j)])

		pred_set_diff[i,j]<-win_pred_sets[i,j]-lose_pred_sets[i,j]
	}
}


##########-------Expected Total set differences----------########

deviance_total_set_diff_zdts<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_set_diff[i,j]-observed_set_diff[j])
  }
 deviance_total_set_diff_zdts[i]<-(1/dataList$n_teams)* sum_dev[i]
}

mean(deviance_total_set_diff_zdts)##6.94


### Save the results in order to use them in comparison with ones of ordered logistic
#save(deviance_total_set_diff_zdts,file="deviance_total_set_diff_zdts")
#save(deviance_points_zdts,file="deviance_points_zdts")
#save(deviance_set_diff_zdts,file="deviance_set_diff_zdts")
#save(deviance_rel_freq_zdts,file="deviance_rel_freq_zdts")
#save(deviance_freq_zdts,file="deviance_freq_zdts")

