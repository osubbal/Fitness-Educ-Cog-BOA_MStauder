# Fitness-Educ-Cog-BOA_MStauder
A repository containing the code used for "Examining cross-sectional and longitudinal relationships between multidomain physical fitness metrics, education, and cognition in Black older adults" (Stauder et al. 2024)

# About

- Coding Language: [R]
- Version: [version 4.1.1]
- Required Packages: 
      library('foreign') 
      library('readxl')
      library('dplyr')
      library("sas7bdat")
      library('ggplot2')
      library('ggsignif')
      library("plotrix")
      library(psych)
      library(tidyverse)
      library(Hmisc)
      library(caret)
      library(lmtest)
      library(corrplot)
      library(car)
      library(relaimpo)
      library(ggpubr)
      library(DescTools)
      library(epiDisplay)
  
# Usage

Goal: three main objectives: 1) To explore the relative strength of cross-sectional associations between modifiable variables (gait speed, grip strength, peak expiratory flow, body mass index, education) and global cognition at baseline, 2) To assess whether baseline modifiable variables predict maintenance or decline in cognitive performance over 4 years, and 3) To examine whether rate of change in physical fitness variables was associated with rate of change in global cognition.
Original Usage: [all data obtained from HRS] Predictor variables = demographic variables (including years of educational attainment), physical fitness/performance variables (gaitspeed, peak expiratory flow, grip strength, body mass index); Outcome variables = TICS Composite Score, Rate of change in TICS Composite over 4 years, Cognitive maintainer/decliner (based on binary divide; decliner = <0 rate of change)

How to Use: 
- To run this code, you will use the attached R code and RAND HRS Longitudinal File 2018 (V1) Waves 10–13 data (available at https://hrs.isr.umich.edu/about).

# BBAL File Paths
 - Dataset: "\Projects\HRS_onedrive_orig\RawData"
 - Dataset Documentation: "\Projects\BADS_2021E0199\HRS\BOA\Stauder_FYP_BOA_Long-Fit-Cog\randhrs1992_2018v1_File-Documentation.pdf"
 - Script: "\Projects\BADS_2021E0199\HRS\BOA\Stauder_FYP_BOA_Long-Fit-Cog\Scripts\2025-01-03_HRS_BOA_Manuscript_GitHub-Code.R"      
