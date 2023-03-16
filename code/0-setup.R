# ==============================================================================
# SETUP
# ==============================================================================

# set path
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))

# data processing
library(tidyverse)
library(magrittr)
library(reshape2)

# plotting
library(cowplot)
library(png)

# machine learning
library(caret)
library(ranger)
library(party)

# load functions
functions <- list.files(path = 'functions/', pattern = "[.]R$", 
                        full.names = TRUE, recursive = TRUE)
for (i in (1:length(functions))) {source(functions[i])}
