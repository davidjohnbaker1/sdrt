#======================================================================================================
# Find IDyOM n-gram Functions
#--------------------------------------------------
# Hard Code Example 

std_e_m <- function(x) sd(x)/sqrt(length(x))


get_string_idyom_ic <- function(chopdown){
  chopdown %>%
    select(melody.name, note.id, scale_degree, information.content, new) %>%
    group_by(new) %>% # Group every instance together 
    mutate(sum_ic = sum(information.content)) %>% # cumulative add all notes in pattern
    select(melody.name, sum_ic, new) %>%
    distinct() %>% 
    ungroup(new) %>% # 
    summarise(mean_total_ic = mean(sum_ic))
}


find_2_grams <- function(first, second, dataset){
  dataset %>% 
    mutate(new = cumsum(ifelse(scale_degree == first & 
                                 lead(scale_degree, n = 1) == second , 1, 0))) %>% 
    filter(new != 0) %>% 
    group_by(new) %>%
    slice(1L:2L)
}

find_3_grams <- function(first, second, third, dataset){
  dataset %>% 
    mutate(new = cumsum(ifelse(scale_degree == first & 
                                 lead(scale_degree, n = 1) == second & 
                                 lead(scale_degree, n = 2) == third, 1, 0))) %>% 
    filter(new != 0) %>% 
    group_by(new) %>%
    slice(1L:3L)
}

find_5_grams <- function(first, second, third, fourth, fifth, dataset){
  dataset %>% 
    mutate(new = cumsum(ifelse(scale_degree == first & 
                                 lead(scale_degree, n = 1) == second & 
                                 lead(scale_degree, n = 2) == third &
                                 lead(scale_degree, n = 3) == fourth &
                                 lead(scale_degree, n = 4) == fifth 
                               , 1, 0))) %>% 
    filter(new != 0) %>% 
    group_by(new) %>%
    slice(1L:5L)
}


find_7_grams <- function(first, second, third, fourth, fifth, sixth, seventh, dataset){
  dataset %>% 
    mutate(new = cumsum(ifelse(scale_degree == first & 
                                 lead(scale_degree, n = 1) == second & 
                                 lead(scale_degree, n = 2) == third & 
                                 lead(scale_degree, n = 3) == fourth & 
                                 lead(scale_degree, n = 4) == fifth & 
                                 lead(scale_degree, n = 5) == sixth & 
                                 lead(scale_degree, n = 6) == seventh, 1, 0))) %>% 
    filter(new != 0) %>% 
    group_by(new) %>%
    slice(1L:7L)
}

find_9_grams <- function(first, second, third, fourth, fifth, 
                         sixth, seventh, eighth, ninth, dataset){
  dataset %>% 
    mutate(new = cumsum(ifelse(scale_degree == first & 
                                 lead(scale_degree, n = 1) == second & 
                                 lead(scale_degree, n = 2) == third & 
                                 lead(scale_degree, n = 3) == fourth & 
                                 lead(scale_degree, n = 4) == fifth & 
                                 lead(scale_degree, n = 5) == sixth & 
                                 lead(scale_degree, n = 6) == seventh &
                                 lead(scale_degree, n = 7) == eighth &
                                 lead(scale_degree, n = 8) == ninth, 
                               1, 0))) %>% 
    filter(new != 0) %>% 
    group_by(new) %>%
    slice(1L:9L)
}


gram_finder <- function(gram, dataset){
  gram_c <- as.character(gram)
  cap <- nchar(gram)
  dataset %>%
    mutate(flag = row_number() %in% grepRaw(gram_c, 
                                            paste0(scale_degree, collapse = ""), 
                                            all = TRUE,
                                            fixed = TRUE)) %>%
    group_by(flag = cumsum(flag)) %>%
    filter(flag != 0) %>% 
    slice(1:cap)
}


#======================================================================================================

