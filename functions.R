google_graph = function(data, date, observation, graph_title) {
  
  data %>% 
    ggplot() +
    geom_line(aes(x = date, y = observation), 
              color = "#09557f",
              alpha = 0.6,
              size = 0.6) +
    labs(x = "Date (Start of Week)", 
         y = "Relative Proportion",
         title = graph_title) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(angle = 45))
  
}

bcp_analysis = function(observation, data) {
  
  set.seed(100)
  
  bcp = bcp(observation)
  
  prob = bcp$posterior.prob
  prob = as.data.frame(prob) 
  
  bcp_dataframe = cbind(data, prob) %>% 
    select(date, prob)
  
}

bcp_plot = function(dataframe){
  
  dataframe %>% 
    ggplot() +
    geom_line(aes(x = date, y = prob),
              color = "#09557f",
              alpha = 0.6,
              size = 0.6) +
    labs(x = "",
         y = "Posterior Probability",
         title = "Changepoint Probabilities") +
    theme_minimal() +
    ylim(0, 1) +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(angle = 45))
  
}