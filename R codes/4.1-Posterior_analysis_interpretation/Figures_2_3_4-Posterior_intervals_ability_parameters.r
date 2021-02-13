
#----------------ordered Logistic model----------------#
### 95% Posterior Intervals for ability parameters

# Figure 2
plot1<-mcmc_intervals(posterior_team_abil[,c(team_abil_order_final_ordered_logistic)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Overall abilities")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 27, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 26))


##or by using another way

plot2<-mcmc_intervals(array_posterior_final_ordered_logistic[,,c(16+team_abil_order_final_ordered_logistic)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Abilities")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))




#----------------ZDTS Model----------------#


### 95% Posterior Intervals for ability parameters
color_scheme_set("brightblue")

plot1<-mcmc_intervals(posterior_attack[,c(attack_order_final_ZDTS_paper.v1)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Attack")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 25, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 24))

plot2<-mcmc_intervals(posterior_defense[,c(defense_order_final_ZDTS_paper.v1)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Defense")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 25, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 24))



plot3<-mcmc_intervals(posterior_overall[,c(overall_order_final_ZDTS_paper.v1)],
                      prob = 0.95,prob_outer=0.95,
                      point_est = c( "mean"))+ggtitle("Overall abilities")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 27, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 20, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 20, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 26))

#------- Figure 3

ggsave(file="overall_abilities.png",units="mm",plot3)
# ---------------Figure 4

grid.arrange(plot1,plot2,ncol=2)

