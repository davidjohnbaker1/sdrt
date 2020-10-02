#--------------------------------------------------
# Shame IDYOM Code
#--------------------------------------------------


calculate_idyom_cum_ics <- function(dataset, helpful_list){
  
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 9",]$average_ic <- as.numeric(find_2_grams(4,9, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 2",]$average_ic <- as.numeric(find_2_grams(4,2, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 10",]$average_ic <- as.numeric(find_2_grams(2, 10, dataset) 
                                                                              %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 1",]$average_ic <- as.numeric(find_2_grams(7,1 , dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 5",]$average_ic <- as.numeric(find_2_grams(4,5, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="5 4",]$average_ic <- as.numeric(find_2_grams(5,4, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="11 4",]$average_ic <- as.numeric(find_2_grams(11,4, dataset) 
                                                                              %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 3",]$average_ic <- as.numeric(find_2_grams(0,3, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 0",]$average_ic <- as.numeric(find_2_grams(2,0, dataset) 
                                                                             %>% get_string_idyom_ic())
  #--------------------------------------------------
  # START THREE
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 9 8",]$average_ic <- as.numeric(find_3_grams(0,9,8, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 10 11",]$average_ic <- as.numeric(find_3_grams(0,10,11, dataset) 
                                                                                 %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 1 2",]$average_ic <- as.numeric(find_3_grams(0,1,2 , dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="9 0 7",]$average_ic <- as.numeric(find_3_grams(9,0,7, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 7 4",]$average_ic <- as.numeric(find_3_grams(2,7,4, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="5 4 2",]$average_ic <- as.numeric(find_3_grams(5,4,2 , dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 2 0",]$average_ic <- as.numeric(find_3_grams(4,2,0, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 5 4",]$average_ic <- as.numeric(find_3_grams(7,5,4, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 10 10",]$average_ic <- as.numeric(find_3_grams(0,10,10, dataset) 
                                                                                 %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 1 2",]$average_ic <- as.numeric(find_3_grams(0,1,2, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="9 0 7",]$average_ic <- as.numeric(find_3_grams(9,0,7, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 7 4",]$average_ic <- as.numeric(find_3_grams(2,7,4, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="5 4 2",]$average_ic <- as.numeric(find_3_grams(5,4,2, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 2 0",]$average_ic <- as.numeric(find_3_grams(4,2,0, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 5 4",]$average_ic <- as.numeric(find_3_grams(7,5,4, dataset) 
                                                                               %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 10 10",]$average_ic <- as.numeric(find_3_grams(0,10,10, dataset) 
                                                                                 %>% get_string_idyom_ic())
  #--------------------------------------------------
  # START FIVE GRAMS 
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="6 4 4 6 4",]$average_ic <- as.numeric(find_5_grams(6,4,4,6,4, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 5 4 2 0",]$average_ic <- as.numeric(find_5_grams(7,5,4,2,0, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 9 7 5 4",]$average_ic <- as.numeric(find_5_grams(7,9,7,5,4, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 2 4 5 2",]$average_ic <- as.numeric(find_5_grams(2,2,4,5,2, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="9 5 4 5 7",]$average_ic <- as.numeric(find_5_grams(9,5,4,5,7, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="9 7 5 4 2",]$average_ic <- as.numeric(find_5_grams(9,7,5,4,2, dataset) 
                                                                                   %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 9 11 0 7",]$average_ic <- as.numeric(find_5_grams(7,9,11,0,7, dataset) 
                                                                                    %>% get_string_idyom_ic())
  #--------------------------------------------------
  # START SEVEN 
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="9 8 5 8 4 0 10",]$average_ic <- as.numeric(find_7_grams(9,8,5,8,4,0,10, dataset) 
                                                                                        %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="5 7 9 7 5 4 2",]$average_ic <- as.numeric(find_7_grams(5, 7, 9, 7, 5, 4, 2, dataset) 
                                                                                       %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 11 9 7 5 4 2",]$average_ic <- as.numeric(find_7_grams(0, 11, 9, 7, 5, 4, 2, dataset) 
                                                                                        %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="5 8 6 8 5 1 1",]$average_ic <- as.numeric(find_7_grams(5, 8, 6, 8, 5, 1, 1, dataset) 
                                                                                       %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 4 0 7 4 2 4",]$average_ic <- as.numeric(find_7_grams(0, 4, 0, 7, 4, 2, 4, dataset) 
                                                                                       %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 9 7 5 4 2 0",]$average_ic <- as.numeric(find_7_grams(7, 9, 7, 5, 4, 2, 0, dataset) 
                                                                                       %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="0 2 0 11 0 4 2",]$average_ic <- as.numeric(find_7_grams(0, 2, 0, 11, 0, 4, 2, dataset) 
                                                                                        %>% get_string_idyom_ic())
  #--------------------------------------------------
  # START 9 GRAMS
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="11 9 7 9 7 5 7 9 7",]$average_ic <- as.numeric(find_9_grams(11, 9, 7, 9, 7, 5, 7, 9, 7, dataset) 
                                                                                            %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 5 2 11 0 2 4 4 5",]$average_ic <- as.numeric(find_9_grams(4, 5, 2, 11, 0, 2, 4, 4, 5, dataset) 
                                                                                            %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 5 9 11 4 3 2 7 5",]$average_ic <- as.numeric(find_9_grams(7, 5, 9, 11, 4, 3, 2, 7, 5, dataset) 
                                                                                            %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 5 4 2 0 4 3 4 8",]$average_ic <- as.numeric(find_9_grams(2, 5, 4, 2, 0, 4, 3, 4, 8, dataset) 
                                                                                           %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="7 9 7 5 4 2 0 2 4",]$average_ic <- as.numeric(find_9_grams(7, 9, 7, 5, 4, 2, 0, 2, 4, dataset) 
                                                                                           %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="2 4 5 7 9 7 5 4 2",]$average_ic <- as.numeric(find_9_grams(2, 4, 5, 7, 9, 7, 5, 4, 2, dataset) 
                                                                                           %>% get_string_idyom_ic())
  #--------------------------------------------------
  helpful_list[helpful_list$idyom_notation=="4 5 7 9 7 5 4 2 0",]$average_ic <- as.numeric(find_9_grams(4, 5, 7, 9, 7, 5, 4, 2, 0, dataset) 
                                                                                           %>% get_string_idyom_ic())
  
  
}
