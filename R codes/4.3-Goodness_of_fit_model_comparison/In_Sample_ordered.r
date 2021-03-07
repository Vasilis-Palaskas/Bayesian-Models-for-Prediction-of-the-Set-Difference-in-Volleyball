#--------------------------------------------------------------------------------------------------------------------------
######------MAD Posterior Results by using the ordered multinomial------############################
#load(file.choose())##Output/Stan Models/Final Ordered Logistic
#load(file.choose())##Data/dataList
#load(file.choose())##Data/new_volley

#######------- Ordered-Multinomial Model with team abilities----#########


#Load proper libraries


library(tikzDevice)
library(bayesplot)
library(shinystan)
library(skellam)
library(rstan)
library(loo)

#######------- ZDTS Model with team abilities----#########

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




dataList_new<-list(n_teams=12,n_games=length(datafr_teams_scores_set_win$home_Team),
                   dif_sets=datafr_teams_scores_set_win$home_score-datafr_teams_scores_set_win$away_score,
                   home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
                   away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))


## Proper recoding of response variable (ordinality property)
for (i in 1:dataList_new$n_games) {
  if (dataList_new$dif_sets[i]==1) {
    dataList_new$dif_sets[i]=4
  } else if (dataList_new$dif_sets[i]==2){
    dataList_new$dif_sets[i]=5
  } else if (dataList_new$dif_sets[i]==3){
    dataList_new$dif_sets[i]=6
  } else if (dataList_new$dif_sets[i]==-1){
    dataList_new$dif_sets[i]=3
  } else if (dataList_new$dif_sets[i]==-2){
    dataList_new$dif_sets[i]=2
  } else if (dataList_new$dif_sets[i]==-3){
    dataList_new$dif_sets[i]=1
  }
}

## Datalist for Bayesian Models via RStan

dataList_new_final<-list(n_teams=12,n_games=132,
                         dif_sets=dataList_new$dif_sets,
                         home_team=as.numeric(factor(datafr_teams_scores_set_win$home_Team)),
                         away_team=as.numeric(factor(datafr_teams_scores_set_win$away_Team)))


#model<-final_ordered_logistic
#train_dataset<-dataList_new_final
#test_dataset<-dataList_new_final

##By having the same data set for both training and test data set, we implement the in sample diagnostics
##By having different data sets for the training and test data sets, we implement the out-of-sample prediction


in_sample_prediction_Ordered<-function(train_dataset,test_dataset,model){
  
  ##### Extraction of model parameters
  params_final_ordered_logistic<-rstan::extract(model)
  
  
  c_final_ordered_logistic<-params_final_ordered_logistic$c
  team_abil_final_ordered_logistic<-params_final_ordered_logistic$team_abil  

  
  

####Proper transformations so that observed dif-sets and pred_dif sets be compatitible
pred_differences_ord<-params_final_ordered_logistic$y_pred

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
  matrix_pred_game_points<-matrix(nrow=matrix_rows,ncol=12)
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
  post_distr_total_points<-matrix(nrow=dim(transform_pred_diff_ord)[1],ncol=12)
  
  for (j in 0:(dim(transform_pred_diff_ord)[1]-1)){
    for (i in 1:12) {
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
  
  
  			return(list(pred_differences_out=transform_pred_diff_ord,
			multi_pred_differences_out=pred_differences_ord,
			pred_team_points=post_distr_total_points,
			pred_ranking_test=ord_ranking_datafr))
}




###########-------------------------In Sample prediction results--------------------###############
in_sample_results_ordered<-in_sample_prediction_Ordered(dataList_new_final,dataList_new_final,
                                          final_ordered_logistic)
		

				
	pred_differences_out_ordered<-in_sample_results_ordered$pred_differences_out
	pred_team_points_ordered<-in_sample_results_ordered$pred_team_points
     pred_ranking_ordered<-in_sample_results_ordered$pred_ranking_test
	multi_pred_differences_out_ordered<-in_sample_results_ordered$multi_pred_differences_out
							
	#save(pred_differences_out_ordered,file="In sample Predicted differences (ordered)")
	#save(multi_pred_differences_out_ordered,file="In sample Predicted differences (ordered) (Multinomial values)")
	##save(pred_ranking_ordered,file="In sample Predicted ranking (ordered)")
	#save(pred_team_points_ordered,file="In sample Predicted Points (ordered)")
	
	
	
	###########----------Posterior summaries of MAD----------------###########################
	#load(file.choose())#/Output/MAD/In sample Predicted differences (ordered)

	#load(file.choose())#/Output/MAD/In sample Predicted differences (ordered)(Multinomial values)

	#load(file.choose())#/Output/MAD/In sample Predicted ranking (ordered)

	#load(file.choose())#/Output/MAD/In sample Predicted Points (ordered)



	
	###########----------Posterior summaries of MAD----------------###########################

## Datalist needed for the calculation of post. distr. of MAD quantities.
dataList<-list(n_teams=length(teams_names),n_games=length(datafr_teams_scores_set_win$home_score),
home_sets=datafr_teams_scores_set_win$home_score,
away_sets=datafr_teams_scores_set_win$away_score,
home_team=as.numeric(datafr_teams_scores_set_win$home_Team),
away_team=as.numeric(datafr_teams_scores_set_win$away_Team))

################--------------Deviance measures-------------###############
### Since we obtained some predicted quantities (set differences, points, rankings), we are ready to 
### obtain the posterior distribution of Mean Absolute Difference (MAD) diagnostics


##########-------Expected Points----------########


observed_points_dev<-c(36,37,16,28,38,14,62,7,39,16,50,53)


deviance_points_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out_ordered)[1]){
  sum_dev[i]<-0
  for (j in 1:dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_team_points_ordered[i,j]-observed_points_dev[j])
  }
  deviance_points_ordered[i]<-(1/length(observed_points_dev))* sum_dev[i]
}
mean(deviance_points_ordered)##4.93




##########-------Expected set differences----------########



observed_set_diff_dev<-dataList$home_sets-dataList$away_sets
deviance_set_diff_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out_ordered)[1]){
  sum_dev[i]<-0
  for (j in 1:length(observed_set_diff_dev)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_differences_out_ordered[i,j]-observed_set_diff_dev[j])
   
}
  deviance_set_diff_ordered[i]<-(1/length(observed_set_diff_dev))* sum_dev[i]
}
mean(deviance_set_diff_ordered)###1.45



##########-------Frequencies----------########


###Predicted set differences###
multi_pred_differences_out_ordered
pred_differences_out_ordered


length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_ordered<-NULL
for (i in 1:nrow((pred_differences_out_ordered))) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out_ordered[i,])["-3"],
	table(pred_differences_out_ordered[i,])["-2"],table(pred_differences_out_ordered[i,])["-1"],
	table(pred_differences_out_ordered[i,])["1"],table(pred_differences_out_ordered[i,])["2"],
	table(pred_differences_out_ordered[i,])["3"])))
}

pred_freq_ordered_iter<-matrix(pred_freq_ordered,nrow=nrow(pred_differences_out_ordered),6,byrow=T)
pred_freq_ordered_iter[which(is.na(pred_freq_ordered_iter))]<-0
colnames(pred_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_freq_ordered_iter_matrix<-matrix(pred_freq_ordered_iter,nrow=nrow(pred_freq_ordered_iter),6)

deviance_freq_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_freq_ordered_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_freq_ordered_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_freq_ordered_iter_matrix[i,j]-observed_freq[j])
  }
  deviance_freq_ordered[i]<-(1/ncol(pred_freq_ordered_iter_matrix))* sum_dev[i]
}
mean(deviance_freq_ordered)##4.12



##########-------Relative Frequencies----------########
##########

###Predicted set differences###
multi_pred_differences_out_ordered
pred_differences_out_ordered

length(observed_set_diff_dev)###132, i.e number of regular season matches
observed_freq<-as.vector(table(observed_set_diff_dev))


pred_freq_ordered<-NULL
for (i in 1:nrow(pred_differences_out_ordered)) {
	pred_freq_ordered<-c(pred_freq_ordered,as.vector(c(table(pred_differences_out_ordered[i,])["-3"],
	table(pred_differences_out_ordered[i,])["-2"],table(pred_differences_out_ordered[i,])["-1"],
	table(pred_differences_out_ordered[i,])["1"],table(pred_differences_out_ordered[i,])["2"],
	table(pred_differences_out_ordered[i,])["3"])))
}

pred_rel_freq_ordered_iter<-matrix(pred_freq_ordered/132,nrow=nrow(pred_differences_out_ordered),6,byrow=T)
pred_rel_freq_ordered_iter[which(is.na(pred_rel_freq_ordered_iter))]<-0

colnames(pred_rel_freq_ordered_iter)<-c("-3","-2","-1","1","2","3")

pred_rel_freq_ordered_iter_matrix<-matrix(pred_rel_freq_ordered_iter,nrow=nrow(pred_rel_freq_ordered_iter),6)



deviance_rel_freq_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_rel_freq_ordered_iter_matrix)[1]){
  sum_dev[i]<-0
  for (j in 1:ncol(pred_rel_freq_ordered_iter_matrix)) {
  sum_dev[i]<- sum_dev[i]+abs(pred_rel_freq_ordered_iter_matrix[i,j]-(observed_freq[j]/dataList_new_final$n_games))
  }
  deviance_rel_freq_ordered[i]<-(1/ncol(pred_freq_ordered_iter_matrix))* sum_dev[i]
}
mean(deviance_rel_freq_ordered)##3.12


#------------------------------------------------------------

######----------Expected - Observed set difference (per team)------------###########
pred_home_sets<-pred_away_sets<-pred_home_team<-pred_away_team<-matrix(nrow=nrow(pred_differences_out_ordered),ncol=dataList$n_games)


for (i in 1:nrow(pred_differences_out_ordered)){
	for (j in 1:dataList$n_games){

		if (pred_differences_out_ordered[i,j]==3){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-0
		} else if (pred_differences_out_ordered[i,j]==2){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-1
		}else if (pred_differences_out_ordered[i,j]==1){
			pred_home_sets[i,j]<-3
			pred_away_sets[i,j]<-2
		}else if (pred_differences_out_ordered[i,j]==(-1)){
			pred_home_sets[i,j]<-2
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out_ordered[i,j]==(-2)){
			pred_home_sets[i,j]<-1
			pred_away_sets[i,j]<-3
		}else if (pred_differences_out_ordered[i,j]==(-3)){
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

win_pred_sets<-lose_pred_sets<-pred_set_diff<-matrix(nrow=nrow(pred_differences_out_ordered),ncol=dataList$n_teams)

for (i in 1:nrow(pred_differences_out_ordered)){
	for (j in 1:dataList$n_teams){
		win_pred_sets[i,j]<-sum(pred_home_sets[i,which(dataList$home_team==j)])+
					sum(pred_away_sets[i,which(dataList$away_team==j)])
		lose_pred_sets[i,j]<-sum(pred_away_sets[i,which(dataList$home_team==j)])+
					sum(pred_home_sets[i,which(dataList$away_team==j)])

		pred_set_diff[i,j]<-win_pred_sets[i,j]-lose_pred_sets[i,j]
	}
}


##########-------Expected Total set differences----------########

deviance_total_set_diff_ordered<-sum_dev<-NULL
for (i in 1:dim(pred_differences_out_ordered)[1]){
  sum_dev[i]<-0
  for (j in 1:dataList$n_teams) {
  sum_dev[i]<- sum_dev[i]+abs(pred_set_diff[i,j]-observed_set_diff[j])
  }
 deviance_total_set_diff_ordered[i]<-(1/dataList$n_teams)* sum_dev[i]
}

mean(deviance_total_set_diff_ordered)##7.83



### Save the results in order to use them in comparison with ones of ZDTS
#save(deviance_total_set_diff_ordered,file="deviance_total_set_diff_ordered")
#save(deviance_points_ordered,file="deviance_points_ordered")
#save(deviance_set_diff_ordered,file="deviance_set_diff_ordered")
#save(deviance_rel_freq_ordered,file="deviance_rel_freq_ordered")
#save(deviance_freq_ordered,file="deviance_freq_ordered")


