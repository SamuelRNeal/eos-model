# Set up environment

# renv::init()
# renv::snapshot()
# renv::restore()

# Load required libraries ----------

# Core
source("Scripts/functions.R")
library(tidyverse)
library(lubridate)
library(progress) # progress bars

# Data wrangling
library(rjson)
library(fastLink)
library(hablar)
library(naniar) # missing data
library(visdat) # visualising missingness
library(VIM) # missing data
library(rms) # missing data
library(mice) # impute missing data
library(finalfit) # visualise missing data
library(gtools)

# Reports
library(knitr)
library(kableExtra)
library(cowplot)
library(magick)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(stevetemplates)

# Modelling
library(pROC) # ROC curves
library(ROCR)
library(mitools)
library(splines)
library(caret)
library(interactions) # interaction plots
library(psfmi) # to pool performance measures
library(gtsummary)
library(gt)
library(boot)

# Presentation of results
library(CalibrationCurves) # from github
