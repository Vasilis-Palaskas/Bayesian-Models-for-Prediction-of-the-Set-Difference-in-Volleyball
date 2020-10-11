##-----------Prior Sensitivity analysis results (Table 2)-----------##

# Run the below mentioned files in order to obtain
# the required quantities regarding with specific model deviances
source(file.choose())#\R Codes\3.2-Prior Sensitivity analysis\Sensitivity_Analysis_home.R
source(file.choose())#\R Codes\3.2-Prior Sensitivity analysis\Sensitivity_Analysis_mu.R
source(file.choose())#\R Codes\3.2-Prior Sensitivity analysis\Sensitivity_Analysis_att-def.R

#####---------------Table 2----------------#################


# Home effect parameter
mean(dev_sensit_home_c_2$dev)#358.6
sd(dev_sensit_home_c_2$dev)#6.63


mean(dev_sensit_home_c_5$dev)#359.5
sd(dev_sensit_home_c_5$dev)#6.7


mean(dev_sensit_home_c_10$dev)#359.2
sd(dev_sensit_home_c_10$dev)#6.4

mean(dev_sensit_home_c_20$dev)#358.9
sd(dev_sensit_home_c_20$dev)#6.7


# mu parameter

mean(dev_sensit_mu_c_2$dev)#356.9
sd(dev_sensit_mu_c_2$dev)#6.0

mean(dev_sensit_mu_c_5$dev)#356.9
sd(dev_sensit_mu_c_5$dev)#6.2

mean(dev_sensit_mu_c_10$dev)#356.8
sd(dev_sensit_mu_c_10$dev)#6.0


mean(dev_sensit_mu_c_20$dev)#357.9
sd(dev_sensit_mu_c_20$dev)#5.91



# Both attacking and defensive parameters
mean(dev_sensit_att_def_c_2$dev)#357.4
sd(dev_sensit_att_def_c_2$dev)#6.5


mean(dev_sensit_att_def_c_5$dev)#357.9
sd(dev_sensit_att_def_c_5$dev)#7.0


mean(dev_sensit_att_def_c_10$dev)#357.7
sd(dev_sensit_att_def_c_10$dev)#7.1


mean(dev_sensit_att_def_c_20$dev)#358.6
sd(dev_sensit_att_def_c_20$dev)#7.3


