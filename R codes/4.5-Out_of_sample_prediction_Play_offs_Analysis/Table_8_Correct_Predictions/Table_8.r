

# In order to obtain the play-off prediction results, it is essential
# to run the codes related with play-off prediction in each play-offs phase for
# both models

# Ordered logistic model (one code for each play-off phase)

source(file.choose())#\Ordered_logistic\Quarter_Final_Ordered_logistic
source(file.choose())#\Ordered_logistic\Semi_Final_Ordered_logistic
source(file.choose())#\Ordered_logistic\Final_Ordered_logistic





#ZDTS model (one code for each play-off phase)


source(file.choose())#\ZDTS\Quarter_final_prediction_ZDTS
source(file.choose())#\ZDTS\Semi_final_prediction_ZDTS
source(file.choose())#\ZDTS\Final_prediction_ZDTS

# Table 8 results

quarter_finals_results_ordered$total_percentage_qualification#82,32%
semi_finals_results_ordered$total_percentage_qualification#84,75
final_finals_results_ordered$total_percentage_qualification#24,74

quarter_finals_results_ZDTS$total_percentage_qualification#	86,53%
semi_finals_results_ZDTS$total_percentage_qualification  ###86,00%
Finals_results_ZDTS$total_percentage_qualification  ###11,13%

