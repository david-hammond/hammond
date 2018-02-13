###################################################################################
# This id a modified version of ProjectTemplate
# created by David Hammond 14 Feb 2018
#
# install pacman package if not already installed
if (!require("pacman")) install.packages("pacman")
#
# p_load is the same as library() but it installs package
# and dependencies if they are not already installed
#
library(pacman)
p_load(tidyverse, ProjectTemplate, rio)
#
# reload.project loads everything in your project folders.
# Change parameters as you need
reload.project(override.config = list(data_loading = F, munging = T))
#
# Now you are ready to write your analysis scipt
##################################################################################


