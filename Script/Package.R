# Load packages ####
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(writexl)

library(ggprism)
library(patchwork)
library(magrittr)

library(ggpubr)
library(patchwork)
library(pheatmap)

library(mice)
library(VIM)

library(gtsummary)





# github configuration ####
#library(usethis)
#library(gitcreds)
#usethis::edit_git_config() #Veryfy username and mail
#usethis::use_git()
#usethis::create_github_token()

#This will take you to the appropriate page on the GitHub website, 
#where you’ll give your token a name and copy it (don’t lose it because 
#it will never appear again!).
# Token_name : ANEMF_Enquete_Preca 
# Token : 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
#gitcreds::gitcreds_set()

#usethis::use_git()
#usethis::use_github()

pacman::p_load(rio,       # to import data
               here,      # to locate files
               tidyverse, # to clean, handle, and plot the data (includes ggplot2 package)
               apyramid,  # a package dedicated to creating age pyramids
               janitor,   # tables and cleaning data
               stringr)   # working with strings for titles, captions, etc.

