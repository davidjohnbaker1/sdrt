#--------------------------------------------------
# Get Incon stuff for modling 
#--------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("pmcharrison/incon")
#--------------------------------------------------
# Try his examples
library(incon)
chord <- c(60, 64, 67) # major triad, MIDI note numbers
incon(chord,model = "all")
