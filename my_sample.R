my_sample <- function(my_data, train_proportion = 0.5){
  data_length       = nrow(my_data)
  train_data_length  = floor(data_length * train_proportion) 
  my_sample_output  = sample(x = data_length,
                             size = train_data_length)
  return(my_sample_output)
}