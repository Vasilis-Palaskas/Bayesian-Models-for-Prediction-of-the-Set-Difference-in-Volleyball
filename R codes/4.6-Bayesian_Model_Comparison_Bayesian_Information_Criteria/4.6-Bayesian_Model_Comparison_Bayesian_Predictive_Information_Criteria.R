

# Model comparison through the Bayesian information criteria of WAIC, LOOIC

###---------Table 9--------###
# ZDTS Model
waic(log_lik_final_ZDTS_paper.v1)####379,7
loo(log_lik_final_ZDTS_paper.v1,r_eff=r_eff_log_lik_final_ZDTS_paper.v1)#for model with proper thinning 380,3

# Ordered Logistic Model
waic(log_lik_final_ordered_logistic)####379,7
loo(log_lik_final_ordered_logistic,r_eff=r_eff_log_lik_final_ordered_logistic)#for model with proper thinning 379,9
