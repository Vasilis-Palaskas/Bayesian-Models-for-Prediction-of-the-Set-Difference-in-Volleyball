
#########---------PLAY-OFF PREDICTION BY USING ORDERED LOGISTIC----------#######

#########------------QUARTER FINALS--------------#######


library(shinystan)
library(skellam)
library(rstan)
library(loo)
library(ggplot2)
library(bayesplot)


#Load the proper dataset

#load(file.choose())#/Data/new_volley
#load(file.choose())#/Data/datafr_teams_scores_set_win


####Here I keep in a vector the teams'  names along with their ranking positions
####to define more appropriate the names of parameters
options(mc.cores = 4)
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



dif_sets=datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score
#### Creation of data list for running model through stan code and brms

train_dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team),
dif_sets=datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score,
home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))


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

# In both train and test datasets, both home_team and away teams variables are recorded as numeric codes (numeric variables)
# More specifically, the codes of teams are the indexes of corresponding teams names of the following vectors: 
# vector teams_names_att or teams_names_def.
# For example Olympiacos corresponds to the home team with numeric code 7 , Paok to code 12, etc..

train_dataList_new_final<-list(n_teams=12,n_games=train_dataList_new$n_games,
dif_sets=train_dataList_new$dif_sets,
home_team=train_dataList_new$home_team,
away_team=train_dataList_new$away_team)



test_dataList_new_final<-list(n_teams=12,n_games=length(c(12,1,12,7,7,4,11,2,11,9,5,9)),
home_team=c(12,1,12,7,7,4,11,2,11,9,5,9),
away_team=c(1,12,1,4,4,7,2,11,2,5,9,5))


## Construction of the final data needed for the model


model_dataList_new_final<-list(n_teams=12,n_games_train=train_dataList_new_final$n_games,
n_games_test=test_dataList_new_final$n_games,
dif_sets=train_dataList_new$dif_sets,
home_team_train=train_dataList_new_final$home_team,home_team_test=test_dataList_new_final$home_team,
away_team_train=train_dataList_new_final$away_team,away_team_test=test_dataList_new_final$away_team)



#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) Final Ordered Logistic (Quarter Finals)--------------#################


rstan_ordered_quarter<-stan(file.choose(),data=model_dataList_new_final,chains=3,thin=2,
                            iter=16000,warmup=4000,cores=3)####Run out_of_sample_Ordered_paper.v1.stan
#save(rstan_ordered_quarter,file="Quarter-final ordered")##call with data, attach, load


#load(file.choose())#Output\Models\Play-off\OrderedQuarter-final ordered



model<-rstan_ordered_quarter

observ_qualified_teams<-c(9,7,11,12)
dataset<-model_dataList_new_final

####-------------FUNCTION------#############

quarter_final_prediction_ordered<-function(dataset,model,
                                           observ_qualified_teams){
  
  
  ####----- 1ST STEP***
  
  ##### Extraction of model parameters
  param_ordered<-rstan::extract(model)
  
  y_pred_ordered<-param_ordered$y_pred
  
  T<-dim(y_pred_ordered)[1]
  
  
  
  ####----2ND STEP***
  
  ###Transformation to the scale of negative and positive differences
  transform_y_pred_ordered<-matrix(nrow=T,ncol=dataset$n_games_test)
  
  
  #####-----3RD STEP********
  for (j in 1:T){
    for (i in 1:dataset$n_games_test) {
      
      if (y_pred_ordered
          [j,i]==4) {
        transform_y_pred_ordered[j,i]<-1
      } else if (y_pred_ordered
                 [j,i]==5){
        transform_y_pred_ordered[j,i]<-2
      } else if (y_pred_ordered
                 [j,i]==6){
        transform_y_pred_ordered[j,i]<-3
      } else if (y_pred_ordered
                 [j,i]==3){
        transform_y_pred_ordered[j,i]<--1
      } else if (y_pred_ordered
                 [j,i]==2){
        transform_y_pred_ordered[j,i]<--2
      } else if (y_pred_ordered
                 [j,i]==1){
        transform_y_pred_ordered[j,i]<--3
      }
    }
  }
  
  
  
  matrix_rows<-dataset$n_games_test*T
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=dataset$n_teams)###columns are the whole number of the league teams
  ### In essence, we generate T (MCMC iterations) leagues
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  for (j in 0:(T-1)){
    for (i in 1:dataset$n_games_test) {   
      
      if (transform_y_pred_ordered[j+1,i]>0) {
        matrix_pred_game_points[j*dataset$n_games_test+i,dataset$home_team_test[i]]<-1
        matrix_pred_game_points[j*dataset$n_games_test+i,dataset$away_team_test[i]]<-0
        matrix_pred_game_points[j*dataset$n_games_test+i,-c(dataset$home_team_test[i],dataset$away_team_test[i])]<-0
      } else if (transform_y_pred_ordered[j+1,i]<0)	{
        matrix_pred_game_points[j*dataset$n_games_test+i,dataset$home_team_test[i]]<-0
        matrix_pred_game_points[j*dataset$n_games_test+i,dataset$away_team_test[i]]<-1
        matrix_pred_game_points[j*dataset$n_games_test+i,-c(dataset$home_team_test[i],dataset$away_team_test[i])]<-0	
      } 
      
    }
  }
  ######## This is the matrix with number of games in rows and number of teams in col####
  
  
  
  
  ######-----Posterior distribution of total points-----########
  post_distr_total_points<-matrix(nrow=T,ncol=dataset$n_teams)
  
  for (j in 0:(T-1)){
    for (i in 1:dataset$n_teams) {
      post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*dataset$n_games_test+1):(j*dataset$n_games_test+dataset$n_games_test),i])
      
    }
  }
  
  total_points<-NULL
  total_points<-post_distr_total_points
  #####-----4TH STEP*************
  
  qualif<-matrix(nrow=T,ncol=dataset$n_teams)
  
  total_points[1,]
  for (i in 1:T){
    for (j in 1:dataset$n_teams){
      
      if (total_points[i,j]>=2){
        qualif[i,j]<-1
      } else { 
        qualif[i,j]<-0
      }
    }
  }
  qualif[1,]
  #####5TH STEP***********
  
  
  
  #### A vector with how many times a team has qualified (from the observed qualified teams)
  
  number_correct_qualif<-NULL
  
  for (k in 1:dataset$n_teams){
    number_correct_qualif[k]<-sum(qualif[,k])
  }
  
  number_correct_qualif[observ_qualified_teams]
  ### Now we extract the percentage of qualifications
  
  percen_qualif<-number_correct_qualif[observ_qualified_teams]/T
  total_percen_correct_qualification<-sum(percen_qualif)/length(observ_qualified_teams)
  
  return(list(percentage_qualified_teams=percen_qualif,
              total_percentage_qualification=total_percen_correct_qualification,
	 post_distr_total_points=	 post_distr_total_points))
  
}


##-------- RESULTS------###
observ_qualified_teams<-c(9,7,11,12)


quarter_finals_results_ordered<-quarter_final_prediction_ordered(model_dataList_new_final,
                                                         rstan_ordered_quarter,observ_qualified_teams)
quarter_finals_results_ordered$post_distr_total_points
quarter_finals_results_ordered$total_percentage_qualification#82,32%
