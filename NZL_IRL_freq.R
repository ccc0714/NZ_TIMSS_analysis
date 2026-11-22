library(tidyverse)
library(haven)
library(plyr)
library(missRanger)
library(mltools)
library(data.table)
library(dbarts)
library(Rcpp)
library(foreign)
library(BART)
library(survey)
source("jackknife_helpers.R")

df.clean <- function(BCG, BSG, BST, BTM, BTS){
  
  BST_BTM<-merge(BST, BTM, by="IDTEALIN")
  
  order1<-as.numeric(as.character(BST_BTM$IDSTUD))
  order2<-as.numeric(as.character(BST_BTM$BTBM14))
  order2<-ifelse(is.na(order2), 0, order2)*-1
  
  BST_BTM<-BST_BTM[order(order1, order2),] 
  
  support_teachers<-c()
  
  for(i in 2:(length(BST_BTM$IDSTUD)))
  {
    if(BST_BTM$IDSTUD[i] == BST_BTM$IDSTUD[i-1])
    {
      support_teachers<-c(support_teachers, i)
    }
  }
  
  #Reducing student with multiple teachers to only 1 teacher 
  BST_BTM<-BST_BTM[-support_teachers,] 
  
  #Each row now has the context data added to it.
  BSG_BST_BTM<-merge(BSG, BST_BTM, by="IDSTUD", all.x=T)
  
  #Each row now has school data too.
  BSG_BST_BTM_BCG<-merge(BSG_BST_BTM, BCG, by="IDSCHOOL")
  
  #Science data
  BST_BTS<-merge(BST, BTS, by="IDTEALIN")
  
  order1<-as.numeric(as.character(BST_BTS$IDSTUD))
  order2<-as.numeric(as.character(BST_BTS$BTBS14))
  order2<-ifelse(is.na(order2), 0, order2)*-1
  
  BST_BTS<-BST_BTS[order(order1, order2),]
  
  support_teachers<-c()
  
  for(i in 2:(length(BST_BTS$IDSTUD)))
  {
    if(BST_BTS$IDSTUD[i] == BST_BTS$IDSTUD[i-1])
    {
      support_teachers<-c(support_teachers, i)
    }
  }
  
  #Reducing student with multiple teachers to only 1 teacher 
  BST_BTS<-BST_BTS[-support_teachers,]
  
  BSG_BST_BTS<-merge(BSG, BST_BTS, by="IDSTUD", all.x=T)
  
  BSG_BST_BTS_BCG<-merge(BSG_BST_BTS, BCG, by="IDSCHOOL")
  
  #Select Columns
  maths_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                "BSBG08A", "BSBG08B",
                "BSBG09A", "BSDGEDUP", "BSBGHER",
                "BSBGSSB", "BSBGSB", "BSBGICM",
                "BSBGSCM", "BSBGSVM",
                
                "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
                "BSBG11A", "BSBG11B", "BSBG10",
                
                "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDMMME",
                
                "BCBGDAS", "BCBGEAS", "BCBGMRS",
                "BCDGSBC",
                
                "BTBM19CA", "BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE", "BTBM14")
  
  
  maths_other<-c("BSMMAT01.x", "BSMMAT02.x", "BSMMAT03.x", 
                 "BSMMAT04.x", "BSMMAT05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")
  
  
  maths_treatment<-c("BSBM26AA", "BSBM26BA")
  
  
  science_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                  "BSBG08A", "BSBG08B",
                  "BSBG09A", "BSDGEDUP", "BSBGHER",
                  "BSBGSSB", "BSBGSB", "BSBGICS",
                  "BSBGSCS", "BSBGSVS",
                  
                  "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
                  "BSBG11A", "BSBG11B", "BSBG10",
                  
                  "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                  "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDSMSE",
                  
                  "BCBGDAS", "BCBGEAS", "BCBGSRS",
                  "BCDGSBC",
                  
                  "BTBS18CA", "BTBS18CB", "BTBS18CC", "BTBS18CD", "BTBS18CE", "BTBS14")
  
  science_other<-c("BSSSCI01.x", "BSSSCI02.x", "BSSSCI03.x",
                   "BSSSCI04.x", "BSSSCI05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")
  
  science_treatment<-c("BSBS26AB", "BSBS26BB")
  
  math_teacher <- c("BTBM19A", "BTBM19B")
  
  science_teacher <- c("BTBS18A", "BTBS18B")
  
  XYM<-BSG_BST_BTM_BCG[,c(maths_vars, maths_other, maths_treatment, math_teacher, "IDSCHOOL", "BTBM20E", "BTBM15A")]
  XYS<-BSG_BST_BTS_BCG[,c(science_vars, science_other, science_treatment, science_teacher, "IDSCHOOL", "BTBS19E", "BTBS15A")]
  
  #Function for converting to numeric
  mymap<-function(x)
  {
    x<-as.numeric(as.character(x))
    
    return(x)
  }
  
  #Categories to be mapped to numeric values
  from<-c("Girl","Boy","Omitted or invalid",
          "Always","Almost always","Sometimes","Never",
          "None or very few (0–10 books)",                         
          "Enough to fill one shelf (11–25 books)",                
          "Enough to fill one bookcase (26–100 books)",            
          "Enough to fill two bookcases (101–200 books)",          
          "Enough to fill three or more bookcases (more than 200)",
          "Finish <Lower secondary education—ISCED Level 2>",                             
          "Finish <Upper secondary education—ISCED Level 3>",                             
          "Finish <Post-secondary, non-tertiary education—ISCED Level 4>",                
          "Finish <Short-cycle tertiary education—ISCED Level 5>",                        
          "Finish <Bachelor’s or equivalent level—ISCED Level 6>",                        
          "Finish <Postgraduate degree: Master’s—ISCED Level 7 or Doctor —ISCED Level 8>",
          "University or Higher",                      
          "Post-secondary but not University",         
          "Upper Secondary",                           
          "Lower Secondary",                           
          "Some Primary, Lower Secondary or No School",
          "Don't Know",
          "Yes","No","I don't know", "Not applicable",
          "Every day","Almost every day","Sometimes","Never",
          "Female","Male",
          "Under 25","25–29","30–39","40–49","50–59","60 or more",
          "Major in Mathematics and Mathematics Education",       
          "Major in Mathematics but not in Mathematics Education",
          "Major in Mathematics Education but not in Mathematics",
          "All Other Majors",                                     
          "No Formal Education Beyond Upper Secondary",
          "More Affluent",                               
          "Neither More Affluent nor More Disadvantaged",
          "More Disadvantaged",
          "Major in Science and Science Education",       
          "Major in Science but not in Science Education",
          "Major in Science Education but not in Science",
          "Once a week","Once every two weeks","Once a month","Once every two month","Never or almost never",
          "Every day", "3 or 4 times a week", "1 or 2 times a week", "Less than once a week", "Never",
          "My teacher never gives me homework in…", "1–15 minutes", "16–30 minutes", "31–60 minutes", "61–90 minutes", "More than 90 minutes",
          "Always or almost always", "Sometimes", "Never or almost never", "Logically not applicable",
          "I do not assign science homework", "I do not assign mathematics homework", "15 minutes or less",
          "A lot", "Some", "None",
          "Every or almost every lesson", "About half the lessons", "Some lessons")
  
  
  to=c(0, 1, NA,
       3, 2, 1, 0,
       5, 20, 50, 150, 200,
       2, 3, 4, 5, 6, 8,
       5, 4, 3, 2, 1, 0,
       1, 0, NA, NA,
       5, 2, 1, 0,
       0, 1,
       25, 29, 39, 49, 59, 66,
       2, 1, 1, 0, 0,
       3, 2, 1,
       2, 1, 1,
       4,3,2,1,0,
       5, 3.5, 1.5, 0.5, 0,
       0, 10, 20, 45, 75, 100,
       2, 1, 0, 0,
       0, 0, 10,
       2, 1, 0,
       3, 2, 1)
  
  mymap2<-function(x)
  {
    x<-mapvalues(x,
                 from=from,
                 to=to)
    
    x<-as.numeric(as.character(x))
    
    return(x)
  }
  
  XYM<-mutate_at(XYM, c(2:9, 16:25, 27:28, 34, 38:43, 53:56, 58:59), mymap2)
  XYS<-mutate_at(XYS, c(2:9, 16:25, 27:28, 34, 38:43, 53:56, 58:59), mymap2)
  
  XYM<-mutate_at(XYM, c(1, 10:15, 26, 29:33, 35:37, 44:52, 57), mymap)
  XYS<-mutate_at(XYS, c(1, 10:15, 26, 29:33, 35:37, 44:52, 57), mymap)
  
  XY<-merge(XYM, XYS, by="IDSTUD")
  return(XY)
}

math_paired <- function(XY){
  math_class_id_var <- "IDCLASS.x.x"
  math_freq_var <- list(student = "BSBM26AA", teacher = "BTBM19A")
  math_duration_var <- list(student = "BSBM26BA", teacher = "BTBM19B")
  math_weight_var <- "TOTWGT.x"
  
  math_student_summary <- XY %>%
    group_by(!!sym(math_class_id_var)) %>%
    dplyr::summarize(
      mean_hw_freq_M = weighted.mean(!!sym(math_freq_var$student), !!sym(math_weight_var), na.rm = TRUE),
      mean_hw_time_M = weighted.mean(!!sym(math_duration_var$student), !!sym(math_weight_var), na.rm = TRUE),
      #Class weight = students the class represents; JK codes are constant within a school
      class_wt = sum(!!sym(math_weight_var), na.rm = TRUE),
      JKZONE = dplyr::first(JKZONE),
      JKREP = dplyr::first(JKREP),
      .groups = 'drop'
    )

  # Teacher Summary (Math)
  math_teacher_summary <- XY %>%
    select(!!sym(math_class_id_var), Teacher_Freq_M = !!sym(math_freq_var$teacher), Teacher_Time_M = !!sym(math_duration_var$teacher)) %>%
    group_by(!!sym(math_class_id_var)) %>%
    dplyr::summarize(
      Teacher_Freq_Mean_M = mean(Teacher_Freq_M, na.rm = TRUE),
      Teacher_Time_Mean_M = mean(Teacher_Time_M, na.rm = TRUE),
      N_Students_M = n(),
      .groups = 'drop'
    )
  
  # Paired Comparison (Math)
  math_paired_data <- math_student_summary %>%
    inner_join(math_teacher_summary, by = math_class_id_var) %>%
    mutate(
      diff_freq_M = Teacher_Freq_Mean_M - mean_hw_freq_M,
      diff_time_M = Teacher_Time_Mean_M - mean_hw_time_M,
      Subject = "Math"
    ) 
  return(math_paired_data)
}

sci_paired <- function(XY){
  science_class_id_var <- "IDCLASS.x.y"
  sci_freq_var <- list(student = "BSBS26AB", teacher = "BTBS18A")
  sci_duration_var <- list(student = "BSBS26BB", teacher = "BTBS18B")
  sci_weight_var <- "TOTWGT.y"
  
  sci_student_summary <- XY %>%
    group_by(!!sym(science_class_id_var)) %>%
    dplyr::summarize(
      mean_hw_freq_S = weighted.mean(!!sym(sci_freq_var$student), !!sym(sci_weight_var), na.rm = TRUE),
      mean_hw_time_S = weighted.mean(!!sym(sci_duration_var$student), !!sym(sci_weight_var), na.rm = TRUE),
      class_wt = sum(!!sym(sci_weight_var), na.rm = TRUE),
      JKZONE = dplyr::first(JKZONE),
      JKREP = dplyr::first(JKREP),
      .groups = 'drop'
    )
  
  # Teacher Summary (Science)
  sci_teacher_summary <- XY %>%
    select(!!sym(science_class_id_var), Teacher_Freq_S = !!sym(sci_freq_var$teacher), Teacher_Time_S = !!sym(sci_duration_var$teacher)) %>%
    group_by(!!sym(science_class_id_var)) %>%
    dplyr::summarize(
      Teacher_Freq_Mean_S = mean(Teacher_Freq_S, na.rm = TRUE),
      Teacher_Time_Mean_S = mean(Teacher_Time_S, na.rm = TRUE),
      N_Students_S = n(),
      .groups = 'drop'
    )
  
  # Paired Comparison (Science)
  sci_paired_data <- sci_student_summary %>%
    inner_join(sci_teacher_summary, by = science_class_id_var) %>%
    mutate(
      diff_freq_S = Teacher_Freq_Mean_S - mean_hw_freq_S,
      diff_time_S = Teacher_Time_Mean_S - mean_hw_time_S,
      Subject = "Science"
    ) 
  return(sci_paired_data)
}

NZ_BCG<-as.data.frame(read.spss("2019Data/bcgnzlm7.sav"))
NZ_BSG<-as.data.frame(read.spss("2019Data/bsgnzlm7.sav"))
NZ_BST<-as.data.frame(read.spss("2019Data/bstnzlm7.sav"))
NZ_BTM<-as.data.frame(read.spss("2019Data/btmnzlm7.sav"))
NZ_BTS<-as.data.frame(read.spss("2019Data/btsnzlm7.sav"))

IR_BCG<-as.data.frame(read.spss("2019Data/bcgirlm7.sav"))
IR_BSG<-as.data.frame(read.spss("2019Data/bsgirlm7.sav"))
IR_BST<-as.data.frame(read.spss("2019Data/bstirlm7.sav"))
IR_BTM<-as.data.frame(read.spss("2019Data/btmirlm7.sav"))
IR_BTS<-as.data.frame(read.spss("2019Data/btsirlm7.sav"))

NZ_XY <- df.clean(NZ_BCG, NZ_BSG, NZ_BST, NZ_BTM, NZ_BTS)
IR_XY <- df.clean(IR_BCG, IR_BSG, IR_BST, IR_BTM, IR_BTS)

#Jackknife codes for design-based standard errors (joined within country, IDSTUD is not unique across countries)
NZ_XY <- add_jk(NZ_XY, NZ_BSG)
IR_XY <- add_jk(IR_XY, IR_BSG)

math_paired_data_NZ <- math_paired(NZ_XY)
math_paired_data_IR <- math_paired(IR_XY)
sci_paired_data_NZ <- sci_paired(NZ_XY)
sci_paired_data_IR <- sci_paired(IR_XY)

math_country <- bind_rows(
  math_paired_data_NZ %>% mutate(country = "NZ"),
  math_paired_data_IR %>% mutate(country = "IR")
)

sci_country <- bind_rows(
  sci_paired_data_NZ %>% mutate(country = "NZ"),
  sci_paired_data_IR %>% mutate(country = "IR")
)

#Original unweighted test (classes weighted equally, SRS standard errors), kept for comparison
summary(lm(diff_freq_M ~ country + N_Students_M, data = math_country))

summary(lm(diff_freq_S ~ country + N_Students_S, data = sci_country))

#-------------------Design-based test (TIMSS jackknife)---------------------------
#Classes weighted by the students they represent; zones made unique per country
math_country <- math_country %>% filter(!is.na(diff_freq_M), !is.na(JKZONE))
sci_country <- sci_country %>% filter(!is.na(diff_freq_S), !is.na(JKZONE))

math_design <- jk_design(math_country,
                         paste(math_country$country, math_country$JKZONE),
                         math_country$JKREP,
                         math_country$class_wt)

sci_design <- jk_design(sci_country,
                        paste(sci_country$country, sci_country$JKZONE),
                        sci_country$JKREP,
                        sci_country$class_wt)

#Weighted mean student-teacher gap per country (replaces -0.374 / -0.018 and -0.447 / -0.112)
svyby(~diff_freq_M, ~country, math_design, svymean)
svyby(~diff_freq_S, ~country, sci_design, svymean)

#countryNZ coefficient and its p-value test the NZ vs Ireland difference in the gap
summary(svyglm(diff_freq_M ~ country + N_Students_M, design = math_design))
summary(svyglm(diff_freq_S ~ country + N_Students_S, design = sci_design))
