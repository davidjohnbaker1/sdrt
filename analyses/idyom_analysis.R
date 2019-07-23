#--------------------------------------------------
# IDyOM Analysis
#--------------------------------------------------
library(tidyverse)

idyom_data <- read.delim("corpus/idyom_data.dat")

idyom_data <- as_tibble(idyom_data)

idyom_data %>%
  select(melody.id, note.id, melody.name, phrase:pulses,
         mode:cpitch.entropy,probability, information.content, entropy) -> idyom_data


View(idyom_data)

# Calculate Scale Degree 
# Find occurances of Patterns in Corpus
# Get IC for those patterns 

#--------------------------------------------------
# Make Scale Degree Calcualtions
# Use Key Sig and Mode 
# Cpitch - referent mod 12 

create_referent <- function(keysig, modal){
  x <- ifelse(test = keysig >= 0, 
              yes = ((keysig * 7) + modal ) %% 12, 
              no = ((keysig * -5) + modal) %% 12  )
  x
}

idyom_data$referent <- create_referent(keysig = idyom_data$keysig, modal = idyom_data$mode)

# Cpint - Referent 
idyom_data$scale_degree <- (idyom_data$cpitch - idyom_data$referent) %% 12

#--------------------------------------------------
# Make Do-Re-Mi 

idyom_data %>%
  select(melody.name, scale_degree, information.content, entropy, probability) -> matcha
