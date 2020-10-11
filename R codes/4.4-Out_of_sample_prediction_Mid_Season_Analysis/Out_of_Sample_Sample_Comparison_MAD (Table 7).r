---------------------------------------------------------------------------------------------------------------------------
########### --------------------Bayesian Comparison between MAD of two fitted models --------------------#################

library(gridExtra)
library(ggpubr)
library(bayesplot)


# In order to obtain the ppc bar plots, it is essential
# to run the codes related with the mid-season prediction for
# both models

source(file.choose())#\R Codes\4.4-Out-of-sample prediction Mid-Season Analysis\Out_of_Sample_Ordered.R
source(file.choose())#\R Codes\4.4-Out-of-sample prediction Mid-Season Analysis\Out_of_Sample_ZDTS.R


###--Load the proper quantities
##Ordered
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_points_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_set_diff_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_freq_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_rel_freq_ordered

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_total_set_diff_ordered

##ZDTS
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_points_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_set_diff_zdts
#
	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_freq_zdts

#	load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_rel_freq_zdts

	#load(file.choose())#/Output/MAD Comparison (In and Out of Sample)/Out-of-Sample/out_of_sample_deviance_total_set_diff_zdts

##### Conclusion of the MAD measures of the Ordered

out_of_sample_deviance_points_ordered
out_of_sample_deviance_set_diff_ordered
out_of_sample_deviance_freq_ordered
out_of_sample_deviance_rel_freq_ordered
out_of_sample_deviance_total_set_diff_ordered

##### Conclusion of the MAD measures of the ZDTS

out_of_sample_deviance_points_zdts
out_of_sample_deviance_set_diff_zdts
out_of_sample_deviance_freq_zdts
out_of_sample_deviance_rel_freq_zdts*100
out_of_sample_deviance_total_set_diff_zdts



#### out_of_sample_deviance points
out_of_sample_deviance_points<-data.frame(out_of_sample_deviance_points_zdts,out_of_sample_deviance_points_ordered)
out_of_sample_deviance_set_diff<-data.frame(out_of_sample_deviance_set_diff_zdts,out_of_sample_deviance_set_diff_ordered)
out_of_sample_deviance_freq<-data.frame(out_of_sample_deviance_freq_zdts,out_of_sample_deviance_freq_ordered)
out_of_sample_deviance_rel_freq<-data.frame(out_of_sample_deviance_rel_freq_zdts*100,out_of_sample_deviance_rel_freq_ordered*100)
out_of_sample_deviance_total_set_diff<-data.frame(out_of_sample_deviance_total_set_diff_zdts,out_of_sample_deviance_total_set_diff_ordered)

colnames(out_of_sample_deviance_points)<-c("ZDTS","Ordered-Multinomial")
colnames(out_of_sample_deviance_set_diff)<-c("ZDTS","Ordered-Multinomial")
colnames(out_of_sample_deviance_freq)<-c("ZDTS","Ordered-Multinomial")
colnames(out_of_sample_deviance_rel_freq)<-c("ZDTS","Ordered-Multinomial")
colnames(out_of_sample_deviance_total_set_diff)<-c("ZDTS","Ordered-Multinomial")

#Table with posterior means of each measure (Table 7)
apply(out_of_sample_deviance_freq,2,mean)
apply(out_of_sample_deviance_rel_freq,2,mean)
apply(out_of_sample_deviance_set_diff,2,mean)
apply(out_of_sample_deviance_points,2,mean)
apply(out_of_sample_deviance_total_set_diff,2,mean)

apply(out_of_sample_deviance_freq,2,sd)
apply(out_of_sample_deviance_rel_freq,2,sd)
apply(out_of_sample_deviance_set_diff,2,sd)
apply(out_of_sample_deviance_points,2,sd)
apply(out_of_sample_deviance_total_set_diff,2,sd)



# Figures for Table 7 quantities (Electronic Appendix)
plot1<-mcmc_areas(out_of_sample_deviance_points,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Expected total points of each team")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
	
plot2<-mcmc_areas(out_of_sample_deviance_set_diff,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Expected set difference")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
		
plot3<-mcmc_areas(out_of_sample_deviance_freq,adjust=10,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Frequencies of set differences")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
		
	plot4<-	mcmc_areas(out_of_sample_deviance_rel_freq,adjust=12,area_method="equal area",
    			prob = 0.95,point_est = c( "mean"))+ggtitle("  Q= Relative Frequencies of set differences (%)")+
  			theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        		axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        		axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        		plot.title  =element_text( size = 23),axis.text.y = element_blank(),
  			axis.ticks.y = element_blank(),axis.line.y=element_blank())

		
		plot5<-mcmc_areas(out_of_sample_deviance_total_set_diff,
    prob = 0.95,point_est = c( "mean"))+ggtitle("Q=Expected total set differences of each team")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))



ggarrange( plot1,plot2,plot5, nrow=2,ncol=2)
grid.arrange(plot3,plot4,ncol=2)
plot<-arrangeGrob(plot3,plot4,ncol=2)


