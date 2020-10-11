# Load useful libraries

library(skellam)
library(ggplot2)



# Figure 1


expec_value_zdts<-expec_value_used<-NULL
for (lambda1 in seq(0.1, 3, by = 0.1)){
	for (lambda2 in seq(0.1, 3, by = 0.1)){

			expec_value_zdts_nomin<-besselI(2*sqrt(lambda1*lambda2),1)+
				(2*besselI(2*sqrt(lambda1*lambda2),2)*(lambda1+lambda2))/
				(lambda1*lambda2)^(1/2)+(3*besselI(2*sqrt(lambda1*lambda2),3)*(lambda1^2+lambda1*lambda2+lambda2^2))/
				(lambda1*lambda2)

			expec_value_zdts_denomin<-besselI(2*sqrt(lambda1*lambda2),1)*(lambda1+lambda2)+
				(besselI(2*sqrt(lambda1*lambda2),2)*(lambda1^2+lambda2^2))/
				(lambda1*lambda2)^(1/2)+(besselI(2*sqrt(lambda1*lambda2),3)*(lambda1^3+lambda2^3))/
				(lambda1*lambda2)

		expec_value_used<-c(expec_value_used,(lambda1-lambda2))
		expec_value_zdts<-c(expec_value_zdts,
						(lambda1-lambda2)*(expec_value_zdts_nomin/expec_value_zdts_denomin))
	}
}

expec_value_zdts[-(which(is.nan(expec_value_zdts)))]
expec_value_used[-(which(is.nan(expec_value_used)))]
summary
summary(lm(expec_value_zdts ~ expec_value_used-1))
lm_eqn <- function(df){
    m <- lm(expec_value_zdts ~ expec_value_used-1, df);
    eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(R)^2~"="~r2, 
         list(b = format(unname(coef(m)[1]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 2)))
    as.character(as.expression(eq));
}

data<-data.frame(expec_value_used=expec_value_used,expec_value_zdts=expec_value_zdts)

p <- ggplot(data = data, aes(x = expec_value_used, y = expec_value_zdts)) +
            geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
            geom_point(color="blue")+scale_x_continuous(name = "E(Æ_Sk)") +
      scale_y_continuous(name = "E(Z_ZDTS)") 
p1 <- p + geom_text(x = -1.5, y = 2, label = lm_eqn(data), parse = TRUE,size=6)+
  theme(axis.text.x = element_text( size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 13, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size =rel(2), angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = rel(2), angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 10))

ggsave(file="new2_Exp_zdts_3.png",p1,width=195,units="mm")
p1
