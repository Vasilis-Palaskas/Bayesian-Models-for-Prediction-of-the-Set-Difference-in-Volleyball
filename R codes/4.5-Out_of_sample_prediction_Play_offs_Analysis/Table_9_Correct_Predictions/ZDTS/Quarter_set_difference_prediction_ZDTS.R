
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

# In both train and test datasets, both home_team and away teams variables 
# are recorded as numeric codes (numeric variables)
# More specifically, the codes of teams are the indexes of corresponding teams names 
# of the following vectors: vector teams_names_att or teams_names_def.
# For example Olympiacos corresponds to the home team with numeric code 7 , 
# Paok to code 12, etc...


#-Datalist for the Stan models

train_dataList<-list(n_teams=12,
n_games=length(datafr_teams_scores_set_win$home_Team),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))

#### The observed quarter-final results (according to
quarter_train_home_team<-c(12,7,11,9,1,5,2,7,11)
quarter_train_away_team<-c(1,4,2,5,12,9,11,4,2)
quarter_train_home_sets<-c(3,3,3,3,1,2,3,3,3)
quarter_train_away_sets<-c(0,1,0,1,3,3,2,0,2)

test_dataList<-list(n_teams=12,
n_games=length(quarter_train_home_team),
home_team=quarter_train_home_team,
away_team=quarter_train_away_team,
home_sets=quarter_train_home_sets,
away_sets=quarter_train_away_sets)


## Construction of the final data needed for the model

#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) Final ZDTS (Paper v1) (Quarter Finals)--------------#################


#Final_ZDTS_paper.v1<-stan(file.choose(),data=train_dataList,chains=3,thin=5,
#iter=40000,warmup=10000,cores=3,seed="12345",init_r=1)####Run Stan Models/final_ZDTS_paper.v1.stan


#save(Final_ZDTS_paper.v1,file="Quarter-final Final ZDTS (Paper v1)")##call with data, attach, load

#load(file.choose())#Output/Models/Final_ZDTS_paper.v1 (init_r=1,seed=12345)

#--

# observ_qualified_teams<-c(9,7,11,12)


#-------------------------------------------------------------
####-------------FUNCTION--------------#####

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
  
  
  
  
  return(list(pred_differences_out=pred_differences_out_final_ZDTS_paper.v1,
              multi_pred_differences_out=multi_pred_differences_out_final_ZDTS_paper.v1
              ))
}


###########-------------------------In Sample prediction results--------------------###############
out_of_sample_results_ZDTS<-out_of_sample_prediction_ZDTS(train_dataList,
                           test_dataList,
                           final_ZDTS_paper.v1)

pred_differences_out<-out_of_sample_results_ZDTS$pred_differences_out
multi_pred_differences_out<-out_of_sample_results_ZDTS$multi_pred_differences_out



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
mean(out_of_sample_deviance_set_diff_zdts)###1.29
sd(out_of_sample_deviance_set_diff_zdts)###0.39
