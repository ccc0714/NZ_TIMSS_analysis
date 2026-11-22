# NZ_TIMSS_analysis
## Model Results
Due to github having a file size limit of 25mb, Model results will be stored in a google drive. Click [here](https://drive.google.com/drive/folders/1b58Ag5RefUQHeevk4CQUmR__kWL6_UFY?usp=sharing) to access it. 

## Code scripts and description 

* 2019Data - This folder contains the 2019 TIMSS data for New Zealand and Ireland 

* 2023Data - This folder contain the 2023 TIMSS data for New Zealand and Ireland 

* BCF_model - This folder contain the MVBCF model from Nathan McJames and another modified model (NZ_MVBCF) that consist of 5 tau trees

* analysis_code - This folder contain the code used for running the MVBCF model on homework related variables such as duration and frequency
  - TIMSS2019/2023_NZ_analysis is for analyzing the effect of student reported homework duration and frequency on student achievement
  - TIMSS2019/2023_NZ_teacher_analysis is for analyzing the effect of teacher reported homework duration and frequency on student achievement

* ATE_calculation - This file is for calculating the Average Treatment Effect (ATE) for all analysis in the folder analysis_code

* NZL_IRL_proportion - This file contain the code for comparing response proportion to treatment heterogeneity variables, including design-based chi-square tests between New Zealand and Ireland

* NZL_IRL_freq - This file contain the code for testing the difference between student and teacher reported homework frequency in New Zealand and Ireland

* Teacher_Student_Frequency - This file contain the code for finding the difference in responses for Frequency and Duration by Student and Teacher, including the project-based classroom comparison

* jackknife_helpers - Helper functions for TIMSS jackknife repeated replication standard errors, used by the three files above

All scripts are run from the repository root (e.g. data is read from 2019Data/ and 2023Data/). 





