##### Summary Figure for DataSet distributions of characteristics #########
#function of `data_summary` component of list output from `summarize_data` function

vis_data_summary <- function(data_summary){
  A = ggplot(data_summary)+
    geom_histogram(aes(number_of_features))+
    scale_y_continuous(labels = scales::label_number(accuracy = 1))+
    labs(title = "Number of Predictors",
         x = "Predictors", y = "Frequency") +
    theme_bw()
  
  #Histogram for number of observations
  B = ggplot(data_summary) +
    geom_histogram(aes(number_of_instances)) +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(title = "Number of Observations",
         x = "Observations", y = "Frequency") +
    theme_bw()
  data_summary
  #Histogram of minority class proportion
  C = ggplot(data_summary)+
    geom_histogram(aes(imbalance))+
    scale_y_continuous(labels = scales::label_number(accuracy = 1))+
    labs(title = "Imbalance",
         x = "Imbalance", y = "Frequency") +
    theme_bw()
  
  #place histogram plots in one pane
  ggarrange(A, B, C)
  
}
