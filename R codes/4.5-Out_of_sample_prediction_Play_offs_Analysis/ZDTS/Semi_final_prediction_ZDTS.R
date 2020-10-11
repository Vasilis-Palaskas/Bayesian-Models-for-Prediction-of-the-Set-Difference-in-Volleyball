
#Load the proper dataset

##load(file.choose())#/Data/new_volley
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

# In both train and test datasets, both home_team and away teams variables are recorded as numeric codes (numeric variables)
# More specifically, the codes of teams are the indexes of corresponding teams names of the following vectors: 
# vector teams_names_att or teams_names_def.
# For example Olympiacos corresponds to the home team with numeric code 7 , Paok to code 12, etc..


#### The observed quarter-final results (according to
quarter_train_home_team<-c(12,7,11,9,1,5,2,7,11)
quarter_train_away_team<-c(1,4,2,5,12,9,11,4,2)
quarter_train_home_sets<-c(3,3,3,3,1,2,3,3,3)
quarter_train_away_sets<-c(0,1,0,1,3,3,2,0,2)

train_dataList<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_score)+length(quarter_train_away_sets),
                     home_sets=c(datafr_teams_scores_set_win$home_score,quarter_train_home_sets),
                     away_sets=c(datafr_teams_scores_set_win$away_score,quarter_train_away_sets),
                     home_team=c(as.numeric(datafr_teams_scores_set_win$home_Team),quarter_train_home_team),
                     away_team=c(as.numeric(datafr_teams_scores_set_win$away_Team),quarter_train_away_team))




test_dataList<-list(n_teams=12,n_games=length(c(7,7,9,9,7,12,12,11,11,12)),
				home_team=c(7,7,9,9,7,12,12,11,11,12),
				away_team=c(9,9,7,7,9,11,11,12,12,11))




#-------------------------------------------------------------------------------------------------------------------------------
########-------------- 1) Final ZDTS (Paper v1) (Semi Finals)--------------#################


Semi_Final_ZDTS_paper.v1<-stan(file.choose(),data=train_dataList,chains=3,thin=5,
iter=40000,warmup=10000,cores=3,seed="12345",init_r=1,verbose=T)####Run Stan Models/ZDTS_paper.v1.stan

#print(Semi_Final_ZDTS_paper.v1)

#save(Semi_Final_ZDTS_paper.v1,file="Semi-final Final ZDTS (Paper v1)")##call with data, attach, load

#load(file.choose())# #Output/Models/Semi-final Final ZDTS (Paper v1)


#launch_shinystan(Semi_Final_ZDTS_paper.v1)

model<-Semi_Final_ZDTS_paper.v1

train_dataset<-train_dataList
test_dataset<-test_dataList
observ_qualified_teams<-c(7,12)

#-------------------------------------------------------------
####-------------FUNCTION--------------#####
semi_final_prediction_ZDTS<-function(train_dataset,test_dataset,model,
observ_qualified_teams){
  

####----- 1ST STEP***-----####

  ##### Extraction of model parameters
  param_final_ZDTS_paper.v1<-rstan::extract(model)
  
  mu_param_final_ZDTS_paper.v1<-param_final_ZDTS_paper.v1$mu
  home_param_final_ZDTS_paper.v1<-param_final_ZDTS_paper.v1$home
  att_param_final_ZDTS_paper.v1<-param_final_ZDTS_paper.v1$attack ##dim(att_param_final_ZDTS_paper.v1)
  def_param_final_ZDTS_paper.v1<-param_final_ZDTS_paper.v1$defense
  
  T<-dim(att_param_final_ZDTS_paper.v1)[1]

#We use the test data
  lambda1_out_final_ZDTS_paper.v1<-lambda2_out_final_ZDTS_paper.v1<-matrix(nrow=T,ncol=test_dataset$n_games)
  for (i in 1:test_dataset$n_games){
    for (j in 1:T){
      lambda1_out_final_ZDTS_paper.v1[j,i]<-exp(mu_param_final_ZDTS_paper.v1[j]+home_param_final_ZDTS_paper.v1[j]+
                                          att_param_final_ZDTS_paper.v1[j,test_dataset$home_team[i]]+
                                          def_param_final_ZDTS_paper.v1[j,test_dataset$away_team[i]])
      lambda2_out_final_ZDTS_paper.v1[j,i]<-exp(mu_param_final_ZDTS_paper.v1[j]+
                                          att_param_final_ZDTS_paper.v1[j,test_dataset$away_team[i]]+
                                          def_param_final_ZDTS_paper.v1[j,test_dataset$home_team[i]])
    }
  }


#-----Generation of values from the predictive distribution based on the ZDTS
  
#Now we start to generate the predicted values

  pred_differences_skellam_out_final_ZDTS_paper.v1<-matrix(nrow=T,ncol=test_dataset$n_games)
  
  for (i in 1:test_dataset$n_games) {
    for (j in 1:T) {
      
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
      
      prob<-numer/denom## a vector of probabilities
      x<-rmultinom(1,1,prob)
      pred_differences_skellam_out_final_ZDTS_paper.v1[j,i]<-which(x[,1]==1)
    }
  }
  
####----- 2ND STEP***-----####

#Transformation to the scale of negative and positive differences
  transform_pred_diff_out_final_ZDTS_paper.v1<-matrix(nrow=T,ncol=test_dataset$n_games)
  dim(pred_differences_skellam_out_final_ZDTS_paper.v1)
  
####----- 3RD STEP***-----####
  for (j in 1:T){
    for (i in 1:test_dataset$n_games) {
      
      if (pred_differences_skellam_out_final_ZDTS_paper.v1
          [j,i]==4) {
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<-1
      } else if (pred_differences_skellam_out_final_ZDTS_paper.v1
                 [j,i]==5){
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<-2
      } else if (pred_differences_skellam_out_final_ZDTS_paper.v1
                 [j,i]==6){
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<-3
      } else if (pred_differences_skellam_out_final_ZDTS_paper.v1
                 [j,i]==3){
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<--1
      } else if (pred_differences_skellam_out_final_ZDTS_paper.v1
                 [j,i]==2){
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<--2
      } else if (pred_differences_skellam_out_final_ZDTS_paper.v1
                 [j,i]==1){
        transform_pred_diff_out_final_ZDTS_paper.v1[j,i]<--3
      }
    }
  }
  


  matrix_rows<-test_dataset$n_games*T
#columns are the whole number of the league teams

  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=train_dataset$n_teams)###columns are the whole number of the league teams
#In essence, we generate T (MCMC iterations) leagues
  dim(matrix_pred_game_points)
  points_team<-NULL
  
  for (j in 0:(T-1)){
    for (i in 1:test_dataset$n_games) {   

     if (transform_pred_diff_out_final_ZDTS_paper.v1[j+1,i]>0) {
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0
      } else if (transform_pred_diff_out_final_ZDTS_paper.v1[j+1,i]<0)	{
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$home_team[i]]<-0
        matrix_pred_game_points[j*test_dataset$n_games+i,test_dataset$away_team[i]]<-1
        matrix_pred_game_points[j*test_dataset$n_games+i,-c(test_dataset$home_team[i],test_dataset$away_team[i])]<-0	
      } 

    }
  }
##Posterior distribution of total points

post_distr_total_points<-matrix(nrow=T,ncol=train_dataset$n_teams)

for (j in 0:(T-1)){
  for (i in 1:train_dataset$n_teams) {
    post_distr_total_points[j+1,i]<-sum(matrix_pred_game_points[(j*test_dataset$n_games+1):(j*test_dataset$n_games+test_dataset$n_games),i])
    
  }
}

total_points<-NULL
total_points<-post_distr_total_points
####----- 4TH STEP***-----####

qualif<-matrix(nrow=T,ncol=train_dataset$n_teams)

total_points[1,]
for (i in 1:T){
	for (j in 1:train_dataset$n_teams){

		if (total_points[i,j]>=3){
			qualif[i,j]<-1
		} else { 
			qualif[i,j]<-0
		}
	}
}
qualif[1,]
####----- 5TH STEP***-----####



# A vector with how many times a team has qualified (from the observed qualified teams)

number_correct_qualif<-NULL

for (k in 1:train_dataset$n_teams){
		number_correct_qualif[k]<-sum(qualif[,k])
}

number_correct_qualif[observ_qualified_teams]
# Now we extract the percentage of qualifications

percen_qualif<-number_correct_qualif[observ_qualified_teams]/T
total_percen_correct_qualification<-sum(percen_qualif)/length(observ_qualified_teams)

 return(list(percentage_qualified_teams=percen_qualif,
			total_percentage_qualification=total_percen_correct_qualification))

}


#-----------------------------------------------------------------------
###------------------------- Semi-Finals Results--------------------------###observ_qualified_teams<-c(7,12)
observ_qualified_teams<-c(7,12)

semi_finals_results_ZDTS<-semi_final_prediction_ZDTS(train_dataset,test_dataset,
			model,observ_qualified_teams)
		
	semi_finals_results_ZDTS$total_percentage_qualification  ###86,28%
