library(tidyverse)
library(haven)
library(foreign)
library(rcompanion)
library(countrycode)
library(scales)
source("jackknife_helpers.R")

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
 
wtable <- function(rowvar, colvar, weights) {
  xtabs(weights ~ rowvar + colvar)
}

df.clean <- function(BCG, BSG, BST, BTM, BTS){
  
  ## ------------------------------- MATH ----------------------------------
  BST_BTM <- merge(BST, BTM, by="IDTEALIN")
  
  order1 <- as.numeric(as.character(BST_BTM$IDSTUD))
  order2 <- ifelse(is.na(as.numeric(as.character(BST_BTM$BTBM14))), 0,
                   as.numeric(as.character(BST_BTM$BTBM14))) * -1
  
  BST_BTM <- BST_BTM[order(order1, order2),]
  
  dup <- which(BST_BTM$IDSTUD[-1] == BST_BTM$IDSTUD[-nrow(BST_BTM)]) + 1
  BST_BTM <- BST_BTM[-dup,]
  
  BSG_BST_BTM <- merge(BSG, BST_BTM, by="IDSTUD", all.x=TRUE)
  BSG_BST_BTM_BCG <- merge(BSG_BST_BTM, BCG, by="IDSCHOOL")
  
  ## ------------------------------ SCIENCE --------------------------------
  BST_BTS <- merge(BST, BTS, by="IDTEALIN")
  
  order1 <- as.numeric(as.character(BST_BTS$IDSTUD))
  order2 <- ifelse(is.na(as.numeric(as.character(BST_BTS$BTBS14))), 0,
                   as.numeric(as.character(BST_BTS$BTBS14))) * -1
  
  BST_BTS <- BST_BTS[order(order1, order2),]
  
  dup <- which(BST_BTS$IDSTUD[-1] == BST_BTS$IDSTUD[-nrow(BST_BTS)]) + 1
  BST_BTS <- BST_BTS[-dup,]
  
  BSG_BST_BTS <- merge(BSG, BST_BTS, by="IDSTUD", all.x=TRUE)
  BSG_BST_BTS_BCG <- merge(BSG_BST_BTS, BCG, by="IDSCHOOL")
  #(1:10,17:26,28:29,35,39:44,53:61)
  ## ------------------------------ SELECT VARS -----------------------------
  maths_vars <- c("IDCNTRY.x", "BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                  "BSBG08A", "BSBG08B", "BSBG09A", "BSDGEDUP", "BSBGHER",
                  "BSBGSSB", "BSBGSB", "BSBGICM", "BSBGSCM", "BSBGSVM",
                  "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E",
                  "BSBG05F", "BSBG05G", "BSBG11A", "BSBG11B", "BSBG10",
                  "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                  "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDMMME",
                  "BCBGDAS", "BCBGEAS", "BCBGMRS", "BCDGSBC",
                  "BTBM19CA", "BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE", "BTBM14")
  
  maths_other <- c("BSMMAT01.x", "BSMMAT02.x", "BSMMAT03.x", "BSMMAT04.x",
                   "BSMMAT05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")
  
  maths_treat <- c("BSBM26AA", "BSBM26BA", "BTBM20A", "BTBM20B", "BTBM20C", "BTBM20D", "BTBM20E")
  
  science_vars <- c("IDCNTRY.x", "BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                    "BSBG08A", "BSBG08B", "BSBG09A", "BSDGEDUP", "BSBGHER",
                    "BSBGSSB", "BSBGSB", "BSBGICS", "BSBGSCS", "BSBGSVM",
                    "BSBG05A","BSBG05B","BSBG05C","BSBG05D","BSBG05E",
                    "BSBG05F","BSBG05G", "BSBG11A","BSBG11B","BSBG10",
                    "BTBG01","BTBG02","BTBG03","BTBG10",
                    "BTBGTJS","BTBGSOS","BTBGLSN","BTBGEAS","BTDSMSE",
                    "BCBGDAS","BCBGEAS","BCBGSRS","BCDGSBC",
                    "BTBS18CA","BTBS18CB","BTBS18CC","BTBS18CD","BTBS18CE","BTBS14")
  
  science_other <- c("BSSSCI01.x","BSSSCI02.x","BSSSCI03.x","BSSSCI04.x",
                     "BSSSCI05.x","IDCLASS.y","TOTWGT","IDSTUD")
  
  science_treat <- c("BSBS26AB", "BSBS26BB", "BTBS19A", "BTBS19B", "BTBS19C", "BTBS19D", "BTBS19E")
  
  school_factor <- c("BCBG05B","BTBG04","BTDGEAS","BTDGLSN","BTDGSOS","BTDGTJS")
  
  XYM <- BSG_BST_BTM_BCG[, c(maths_vars, maths_other, maths_treat, school_factor, "BTBM15A", "BTBM19A")]
  XYS <- BSG_BST_BTS_BCG[, c(science_vars, science_other, science_treat, school_factor, "BTBS15A", "BTBS18A")]
  
  factor_XYM <- XYM[, c(1:10,17:26,28:29,35,39:44,51:68)]
  factor_XYS <- XYS[, c(1:10,17:26,28:29,35,39:44,51:68)]
  
  XY <- merge(factor_XYM, factor_XYS, by="IDSTUD")
  return(XY)
}

view_prop <- function(index){
  var <- prop_df$variable[index]
  print(var)
  
  tab <- wtable(XY$IDCNTRY.x.x, XY[[var]], XY$TOTWGT)
  print(prop.table(tab, 1))
}

plot_prop <- function(index){
  target <- prop_df$variable[index]
  
  plot_data <- XY %>%
    group_by(IDCNTRY.x.x, .data[[target]]) %>%
    summarise(n = sum(TOTWGT), .groups = "drop") %>%
    group_by(IDCNTRY.x.x) %>%
    mutate(prop = n / sum(n))
  
  ggplot(plot_data, aes(x = .data[[target]], y = prop, fill = IDCNTRY.x.x)) +
    geom_col(position = position_dodge()) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
}


classify_cramers_v <- function(v, df_v) {
  
  if (df_v == 1) { 
    if (v < 0.30) return("Weak")
    else if (v < 0.50) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 2) { 
    if (v < 0.21) return("Weak")
    else if (v < 0.35) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 3) { 
    if (v < 0.17) return("Weak")
    else if (v < 0.29) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 4) { 
    if (v < 0.15) return("Weak")
    else if (v < 0.25) return("Moderate")
    else return("Strong")
    
  } else { 
    if (v < 0.13) return("Weak")
    else if (v < 0.22) return("Moderate")
    else return("Strong")
  }
}

NZ_XY <- df.clean(NZ_BCG, NZ_BSG, NZ_BST, NZ_BTM, NZ_BTS)
IR_XY <- df.clean(IR_BCG, IR_BSG, IR_BST, IR_BTM, IR_BTS)

XY <- rbind(NZ_XY, IR_XY)
XY$IDCNTRY.x.x <- droplevels(XY$IDCNTRY.x.x)
XY$IDCNTRY.x.x <- countrycode(XY$IDCNTRY.x.x, "iso3n", "country.name")
XY <- XY %>% select(-ends_with(".y"))
XY$TOTWGT <- as.numeric(as.character(XY$TOTWGT.x))
XY <- XY %>% select(-TOTWGT.x) %>% droplevels()

#Jackknife codes kept outside XY so the positional Cramér's V loop below is unaffected
jk_lookup <- bind_rows(
  add_jk(data.frame(IDSTUD = NZ_BSG$IDSTUD), NZ_BSG) %>%
    mutate(country = countrycode(NZ_BSG$IDCNTRY, "iso3n", "country.name")),
  add_jk(data.frame(IDSTUD = IR_BSG$IDSTUD), IR_BSG) %>%
    mutate(country = countrycode(IR_BSG$IDCNTRY, "iso3n", "country.name"))
)

#Design-based (Rao-Scott) chi-square of country vs one variable, with weighted Cramér's V.
#chisq.test on weighted counts treats the population total as the sample size, so p is always ~0.
design_chisq <- function(var){
  valid <- XY[!is.na(XY[[var]]) & !(XY[[var]] %in% c("Omitted or invalid", "Logically not applicable")), ]
  valid$IDSTUD <- as.numeric(as.character(valid$IDSTUD))
  valid <- left_join(valid, jk_lookup, by = c("IDSTUD", "IDCNTRY.x.x" = "country"))
  valid[[var]] <- droplevels(factor(valid[[var]]))
  valid$country <- factor(valid$IDCNTRY.x.x)

  des <- jk_design(valid, paste(valid$country, valid$JKZONE), valid$JKREP, valid$TOTWGT)
  test <- svychisq(as.formula(paste0("~", var, " + country")), des, statistic = "Chisq")

  data <- wtable(valid$IDCNTRY.x.x, valid[[var]], valid$TOTWGT)
  data <- data[, colSums(data) > 0, drop = FALSE]

  #svychisq(statistic = "Chisq") prints the unadjusted Pearson X^2 but takes p from
  #X^2 / mean design effect, so report the adjusted value that matches p
  df <- unname(test$parameter)

  data.frame(variable = var,
             pearson_chisq = unname(test$statistic),
             rao_scott_chisq = qchisq(test$p.value, df, lower.tail = FALSE),
             df = df,
             p_value = test$p.value,
             cramers_v = cramerV(data))
}
#-----------------------------------------------------------------------------------------------------
#Identifying V value for categorical variables against countries 

n <- ncol(XY) - 4
prop_df <- data.frame(variable = character(n), v_df = numeric(n),
                      v_value = numeric(n), strength = character(n))
index <- 1

for (i in c(4:(ncol(XY)-1))) {
  
  variable <- colnames(XY)[i]
  
  # weighted contingency table
  data <- wtable(XY$IDCNTRY.x.x, XY[[i]], XY$TOTWGT)
  
  v_value <- cramerV(data)
  v_df <- min(nrow(data) - 1, ncol(data) - 1)
  level <- classify_cramers_v(v_value, v_df)
  
  prop_df$variable[index] <- variable
  prop_df$v_value[index] <- v_value
  prop_df$v_df[index] <- v_df
  prop_df$strength[index] <- level
  
  index <- index + 1
}

#Index of Treatment variables are 28(BSBM26AA), 29(BSBM26BA), 47(BSBS26AB), 48(BSBS26BB)
view_prop(25)

#Design-based chi-square for the variables reported in Tables 1-4
table_vars <- c("BSBM26AA", "BSBS26AB", "BSBM26BA", "BSBS26BB",
                "BTBM20E", "BTBS19E", "BTBM15A", "BTBS15A")
table_tests <- bind_rows(lapply(table_vars, design_chisq))
table_tests

#------------------------Omit Logically not applicable---------------------------------------------------------
temp <- XY
target <- c("BTBM19CD", "BTBS18CA", "BTBM19CA", "BTBS18CD", "BTBM19CB", "BTBM19CE", "BTBS18CB", "BTBS18CE", "BTBS18CC", "BTBM19CC")
for (i in target){
  temp <- temp %>% 
    filter(.data[[i]] != "Logically not applicable") %>% 
    droplevels()
}

n <- 10
new_prop <- data.frame(variable = character(n), v_df = numeric(n), v_value = numeric(n), strength = character(n))
index <- 1
for (i in target){
  variable <- i
  #Exclude "Logically not applicable" for this item only (temp drops a student if any item is not applicable)
  valid <- XY[!is.na(XY[[i]]) & !(XY[[i]] %in% c("Omitted or invalid", "Logically not applicable")), ]
  valid[[i]] <- droplevels(valid[[i]])
  data <- wtable(valid$IDCNTRY.x.x, valid[[i]], valid$TOTWGT)
  data <- data[, colSums(data) > 0, drop = FALSE]
  v_value <- cramerV(data)
  v_df <- min(nrow(data)-1, ncol(data)-1)
  level <- classify_cramers_v(v_value, v_df)
  new_prop$variable[index] <- variable
  new_prop$v_value[index] <- v_value
  new_prop$v_df[index] <- v_df
  new_prop$strength[index] <- level
  index <- index + 1
}

#Design-based chi-square for the same items
new_prop_tests <- bind_rows(lapply(target, design_chisq))
new_prop_tests

#BTBM19CC = Math Teacher discuss homework in class, #BTBS18CC = Science Teacher discuss homework in class
prop.table(wtable(temp$IDCNTRY.x.x, temp[["BTBM19CB"]], temp$TOTWGT), 1)

#----------------------Plot for treatment heterogeneity------------------------------------
extract_full_cates <- function(file_path) {
  load(file_path)
  data.frame(
    math_3_5 = apply(my_mod$predictions_tau4[, 1, ], 1, mean),
    math_1_2 = apply(my_mod$predictions_tau5[, 1, ], 1, mean),
    sci_3_5  = apply(my_mod$predictions_tau4[, 2, ], 1, mean),
    sci_1_2  = apply(my_mod$predictions_tau5[, 2, ], 1, mean)
  )
}

# 2. loading all 5 chains (Plausible Values)
#Use the 2019 student-reported results for Figs 4-5 and the 2019 teacher-reported results for Figs 6-7
model_files <- paste0(
  "yourdirectory\\ModelResultsChain",
  1:5,
  ".RData"
)

all_chains <- lapply(model_files, extract_full_cates)
final_averages <- Reduce(`+`, all_chains) / length(all_chains)

final_results <- final_averages %>%
  bind_cols(NZ_XY %>% select(BSDGEDUP.x, BSBG04.x))

plot_ready <- final_results %>% 
  #  Pivot subject and frequency
  pivot_longer(cols = starts_with("math") | starts_with("sci"), 
               names_to = "Sub_Freq", values_to = "ATE") %>%
  separate(Sub_Freq, into = c("Subject", "Freq_Low", "Freq_High"), sep = "_") %>% 
  mutate(Frequency = paste(Freq_Low, Freq_High, "Times/Week")) %>% 
  # Pivot moderators
  pivot_longer(cols = c(BSDGEDUP.x, BSBG04.x), 
               names_to = "Moderator_Type", values_to = "Moderator_Value") %>% 
  filter(!is.na(Moderator_Value), 
         !Moderator_Value %in% c("Omitted or invalid", "Don't Know")) %>%  # 4. Clean up labels
  mutate(Subject = ifelse(Subject == "math", "Mathematics", "Science"), 
         Moderator_Type = recode(Moderator_Type, 
                                 "BSDGEDUP.x" = "Parental Education", 
                                 "BSBG04.x" = "Books at Home"))

ggplot(plot_ready, aes(x = Moderator_Value, y = ATE, fill = Frequency)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(Subject ~ Moderator_Type, scales = "free", space = "free_y") +
  coord_flip() +
  labs(
    y = "Effect of Receiving Homework (ATE)",
    x = NULL
  )

#-------------Plot for project based learning------------------
final_results <- final_averages %>%
  bind_cols(NZ_XY %>% select(BTBM20E, BTBS19E, BTBM15A, BTBS15A))

plot_ready <- final_results %>% 
  # Pivot subjects and frequencies
  pivot_longer(cols = starts_with("math") | starts_with("sci"), 
               names_to = "Sub_Freq", values_to = "ATE") %>%
  separate(Sub_Freq, into = c("Subject", "Freq_Low", "Freq_High"), sep = "_") %>% 
  mutate(Frequency = paste(Freq_Low, Freq_High, "Times/Week"),
         Subject_Label = ifelse(Subject == "math", "Mathematics", "Science")) %>% 
  
  pivot_longer(cols = c(BTBM20E, BTBS19E, BTBM15A, BTBS15A), 
               names_to = "Var_Name", values_to = "Moderator_Value") %>%
  
  filter(
    (Subject == "math" & grepl("BTBM", Var_Name)) | 
      (Subject == "sci"  & grepl("BTBS", Var_Name))
  ) %>%
  
  # Group the variables by Question Type for the 2x2 grid
  mutate(Question_Type = case_when(
    grepl("20E|19E", Var_Name) ~ "Long-term Projects",
    grepl("15A", Var_Name)     ~ "Teacher Explains New Content"
  )) %>%
  
  # Final Cleanup
  filter(!is.na(Moderator_Value), 
         !Moderator_Value %in% c("Omitted or invalid", "Don't Know", "NA"))

# Generate the 2x2 Plot
ggplot(plot_ready, aes(x = Moderator_Value, y = ATE, fill = Frequency)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  # Facet by Subject (Rows) and Question Type (Columns)
  facet_grid(Subject_Label ~ Question_Type, scales = "free", space = "free_y") +
  coord_flip() +
  labs(
    y = "Effect of Receiving Homework (ATE)",
    x = "Frequency of Teacher Practice",
    fill = "Homework Frequency"
  ) +
  theme(strip.text = element_text(face = "bold"))



