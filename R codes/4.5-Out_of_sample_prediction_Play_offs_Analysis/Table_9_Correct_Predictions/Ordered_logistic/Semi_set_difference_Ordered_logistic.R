
#########---------PLAY-OFF PREDICTION BY USING ORDERED LOGISTIC----------#######

#########------------SEMI FINALS--------------#######


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

# In both train and test datasets, both home_team and away teams variables are recorded as numeric codes (numeric variables)
# More specifically, the codes of teams are the indexes of corresponding teams names of the following vectors: 
# vector teams_names_att or teams_names_def.
# For example Olympiacos corresponds to the home team with numeric code 7 , Paok to code 12, etc..


#### The observed quarter-final results
quarter_train_home_team<-c(12,7,11,9,1,5,2,7,11)
quarter_train_away_team<-c(1,4,2,5,12,9,11,4,2)
quarter_train_home_sets<-c(3,3,3,3,1,2,3,3,3)
quarter_train_away_sets<-c(0,1,0,1,3,3,2,0,2)

# Training data


train_dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team)+length(quarter_train_home_sets),
				dif_sets=c(datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score,
			quarter_train_home_sets-quarter_train_away_sets),
			home_team=c(as.numeric(factor(datafr_teams_scores_set_win$home_Team)),quarter_train_home_team),
			away_team=c(as.numeric(factor(datafr_teams_scores_set_win$away_Team)),quarter_train_away_team))

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


train_dataList_new_final<-list(n_teams=train_dataList_new$n_teams,
                               n_games=train_dataList_new$n_games,
dif_sets=train_dataList_new$dif_sets,
home_team=train_dataList_new$home_team,
away_team=train_dataList_new$away_team)

#### The observed semi-final results
semi_train_home_team<-c(7,7,9,12,12,11)
semi_train_away_team<-c(9,9,7,11,11,12)
semi_train_home_sets<-c(3,3,0,3,3,0)
semi_train_away_sets<-c(0,0,3,2,0,3)




test_dataList_new_final<-list(n_teams=12,n_games=length(semi_train_home_team),
                              home_team=semi_train_home_team,
                              away_team=semi_train_away_team,
                              home_sets=semi_train_home_sets,
                              away_sets=semi_train_away_sets)


## Construction of the final data needed for the model


model_dataList_new_final<-list(n_teams=12,n_games_train=train_dataList_new_final$n_games,
                               n_games_test=test_dataList_new_final$n_games,
                               dif_sets=train_dataList_new$dif_sets,
                               home_team_train=train_dataList_new_final$home_team,
                               home_team_test=test_dataList_new_final$home_team,
                               away_team_train=train_dataList_new_final$away_team,
                               away_team_test=test_dataList_new_final$away_team)





#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) Final Ordered Logistic (Semi Finals)--------------#################


rstan_ordered_semi_table9<-stan(file.choose(),data=model_dataList_new_final,chains=3,thin=2,
                            iter=16000,warmup=4000,cores=3)####Run out_of_sample_Ordered_paper.v1.stan
#save(rstan_ordered_semi,file="Semi-final ordered")##call with data, attach, load


#load(file.choose())# #Output/Stan Models/Semi-final ordered




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
  
  
  
  
  return(list(pred_differences_out=transform_pred_diff_ord,
              multi_pred_differences_out=pred_differences_ord))
}




###########-------------------------In Sample prediction results--------------------###############
out_of_sample_results_ordered<-out_of_sample_prediction_Ordered(train_dataList_new_final,test_dataList_new_final,
                                                                rstan_ordered_semi_table9)

pred_differences_out<-out_of_sample_results_ordered$pred_differences_out
multi_pred_differences_out<-out_of_sample_results_ordered$multi_pred_differences_out


##########-------Expected set differences----------########

observed_set_diff_dev<-test_dataList_new_final$home_sets-test_dataList_new_final$away_sets

out_of_sample_deviance_set_diff_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out)[1]){
  sum_dev[i]<-0
  for (j in 1:length(observed_set_diff_dev)) {
    sum_dev[i]<- sum_dev[i]+abs(pred_differences_out[i,j]-observed_set_diff_dev[j])
    
  }
  out_of_sample_deviance_set_diff_ordered[i]<-(1/length(observed_set_diff_dev))* sum_dev[i]
}
mean(out_of_sample_deviance_set_diff_ordered)###1.40
sd(out_of_sample_deviance_set_diff_ordered)###0.63
