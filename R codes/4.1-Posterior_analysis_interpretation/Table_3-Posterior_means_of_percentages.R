##------------- Table 3-------------##

##Load the proper libraries

library(skellam)
library(rstan)
library(ggplot2)
library(bayesplot)
library(gridExtra)

##Load the output of the ZDTS model
#load(file.choose())#Output\Models\Final_ZDTS_paper.v1 (init_r=1,seed=12345)
#final_ZDTS_paper.v1<-Final_ZDTS_paper.v1


### Observed percentages of set difference outcomes
round(table(dataList$home_sets-dataList$away_sets)/length(dataList$home_sets-dataList$away_sets),
2)

##Extraction of model parameters
params_final_ZDTS_paper.v1<-extract(final_ZDTS_paper.v1)
  
  mu_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$mu
  home_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$home
  att_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$attack
  def_params_final_ZDTS_paper.v1<-params_final_ZDTS_paper.v1$defense
  


#####---------ZDTS: Probabilities of outcomes between equal strength teams based on the posterior distribution-----------#####

lambda1<-exp(mu_params_final_ZDTS_paper.v1+home_params_final_ZDTS_paper.v1)
lambda2<-exp(mu_params_final_ZDTS_paper.v1)


distr_zdts<-matrix(nrow=length(lambda1),ncol=6)
outcomes<-c(1:6)
for (i in 1:length(lambda1)){
	for (j in outcomes){
	if (j<4){
distr_zdts[i,j]<-dskellam(j-4,lambda1[i],lambda2[i])/(sum(dskellam(-3:-1,lambda1[i],lambda2[i]))+sum(dskellam(1:3,lambda1[i],lambda2[i])))
             }
		else {
		distr_zdts[i,j]<-dskellam(j-3,lambda1[i],lambda2[i])/(sum(dskellam(-3:-1,lambda1[i],lambda2[i]))+sum(dskellam(1:3,lambda1[i],lambda2[i])))
	}
  }
}


distr_zdts<-as.data.frame(distr_zdts*100)
colnames(distr_zdts)<-c("-3","-2","-1","1","2","3")
plot1<-mcmc_areas(distr_zdts,adjust=1/2,
    prob = 0.95,point_est = c( "mean"))+ggtitle("ZDTS")+scale_x_continuous(limits = c(0,50))+
  		theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+
	 xlab("Percentages (%)") + ylab("Set Differences")


## Load the output of ordered logistic model
#load(file.choose())##\Output\Models\Final Ordered Logistic

## Extraction of model parameters
params_final_ordered_logistic<-extract(final_ordered_logistic)
c_final_ordered_logistic<-params_final_ordered_logistic$c
  
 

#####---------Ordered: Probabilities of outcomes between equal strength teams based on the posterior distribution-----------#####

  prob_outcome_altern<-matrix(nrow=dim(c_final_ordered_logistic)[1],ncol=6)
  prob_outcome<-matrix(nrow=dim(c_final_ordered_logistic)[1],ncol=6)
  for (i in 1:dim(prob_outcome)[1]){
	for (j in 1:6){
	if (j==1){
	prob_outcome_altern[i,j]<-(exp(c_final_ordered_logistic[i,j]))/(1+exp(c_final_ordered_logistic[i,j]))

	prob_outcome[i,j]<-(exp(c_final_ordered_logistic[i,j]))/(1+exp(c_final_ordered_logistic[i,j]))
	}else if (j>1 & j<6){
		prob_outcome[i,j]<-(exp(c_final_ordered_logistic[i,j])-exp(c_final_ordered_logistic[i,j])* sum(prob_outcome[i,1:j-1])- sum(prob_outcome[i,1:j-1]))/
		(1+exp(c_final_ordered_logistic[i,j]))
		prob_outcome_altern[i,j]<-(exp(c_final_ordered_logistic[i,j])/(1+exp(c_final_ordered_logistic[i,j])))-sum(prob_outcome_altern[i,1:j-1])
		} else {
		prob_outcome[i,j]<-1-sum(prob_outcome[i,1:5])
		prob_outcome_altern[i,j]<-1-sum(prob_outcome_altern[i,1:5])

		}
	}
}	


prob_outcome<-as.data.frame(prob_outcome*100)
colnames(prob_outcome)<-c("-3","-2","-1","1","2","3")



plot2<-mcmc_areas(prob_outcome,adjust=1/2,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Ordered")+scale_x_continuous(limits = c(0,50))+
 theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_blank(),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        plot.title  =element_text( size = 23),
  axis.ticks.y = element_blank(),axis.line.y=element_blank())+
	 xlab("Percentages (%)")

# Merge in a common plot
plot<-grid.arrange(plot1,plot2,ncol=2)
plot<-arrangeGrob(plot1,plot2,ncol=2)

ggsave(file="set_difference_probabilities.png",plot,width=276, height=195, units="mm")


###-----Table 3------###
# Posterior mean and percentages of ZDTS
round(apply(distr_zdts,2,mean)*100,2)
round(apply(distr_zdts,2,sd),2)

# Posterior mean and percentages of the ordered-multinomial logistic
round(apply(prob_outcome_altern,2,mean)*100,2)
round(apply(prob_outcome,2,sd),2)



