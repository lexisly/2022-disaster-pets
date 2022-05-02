#### ASPCA Disaster Survey #####
##Load packages
library(tidyverse)
library(readxl)#importing excel documents
library(table1)#Summary statistic tables
library(RColorBrewer) #Colours for graphs
library(TAM)#Weighted stats
library(Hmisc)#Aggregate frequencies by weight
library(esquisse)#Data visualization
library(broom)#tidy function
library(rcompanion)#nagelkerke - R2 value, McNemar test
library(ggeffects)#estimated marginal means
library(RVAideMemoire)#Cochran's Q test
library(scales) #Wrap text in ggplot

##Load data (text) and question key
setwd("C:/Users/lexis/Documents/ASPCA Disaster")
disaster_raw<-read.csv("ASPCA_text_header_new.csv", stringsAsFactors = FALSE)
question_key<-read.csv("ASPCA_questions.csv", stringsAsFactors = FALSE)

disaster_raw <- disaster_raw %>% mutate_all(na_if,"")

###### Recode outcome variables
##RQ1: Left at least one pet (0), brought all pets (1)
disaster_raw$brought_pet[(disaster_raw$dog_left_behind == "Yes")]<-"0"
disaster_raw$brought_pet[(disaster_raw$cat_left_behind == "Yes")]<-"0"
disaster_raw$brought_pet[(disaster_raw$horse_left_behind == "Yes")]<-"0"
disaster_raw$brought_pet[(disaster_raw$small_left_behind == "Yes")]<-"0"
disaster_raw$brought_pet[(disaster_raw$livestock_left_behind == "Yes")]<-"0"
disaster_raw$brought_pet[(disaster_raw$none_left_behind == "Yes")]<-"1"
table(disaster_raw$brought_pet)
disaster_raw$brought_pet_RQ2<-disaster_raw$brought_pet
disaster_raw$brought_pet_RQ2[is.na(disaster_raw$brought_pet_RQ2)] <- 0
disaster_raw$brought_pet<-as.factor(disaster_raw$brought_pet)
disaster_raw$brought_pet_RQ2<-as.factor(disaster_raw$brought_pet_RQ2)
table(disaster_raw$brought_pet_RQ2)

##RQ2: Did not intend to bring all pets (0), Intend to bring all pets (1)
disaster_raw$would_bring_recode[(disaster_raw$would_bring_pet == "Yes")]<-"1"
disaster_raw$would_bring_recode[(disaster_raw$would_bring_pet == "No")]<-"0"
disaster_raw$would_bring_recode[(disaster_raw$would_bring_pet == "Not sure")]<-"0"
disaster_raw$would_bring_recode[(disaster_raw$would_bring_pet == "Some but not all")]<-"0"
table(disaster_raw$would_bring_recode)
disaster_raw$would_bring_recode<-as.factor(disaster_raw$would_bring_recode)

##RQ3: Do not support government fund (0), Support government fund (1)
disaster_raw$support_gov_recode[(disaster_raw$support_gov_fund == "Yes")]<-"1"
disaster_raw$support_gov_recode[(disaster_raw$support_gov_fund == "No")]<-"0"
disaster_raw$support_gov_recode[(disaster_raw$support_gov_fund == "Not sure")]<-"0"
table(disaster_raw$support_gov_recode)
disaster_raw$support_gov_recode<-as.factor(disaster_raw$support_gov_recode)

###### Recoding independent variables
##Total pets owned (continuous)
disaster_raw<-disaster_raw %>%
  mutate(total_pets_owned = select(., dog_current_own:livestock_current_own) %>% rowSums(na.rm = TRUE))

disaster_raw<-disaster_raw %>%
  mutate(household_animals_owned = select(., dog_current_own:small_current_own) %>% rowSums(na.rm = TRUE))%>%
  mutate(large_animals_owned = select(., horse_current_own:livestock_current_own) %>% rowSums(na.rm = TRUE))

total_pets<-disaster_raw %>%
  select(dog_current_own:livestock_current_own)%>%
  mutate(total_pets_owned = select(., dog_current_own:livestock_current_own) %>% rowSums(na.rm = TRUE))

#Gender --> Men, Women, Gender minority
disaster_raw$gender_recode[disaster_raw$gender=="Man"] <- 'Men'
disaster_raw$gender_recode[disaster_raw$gender=="Woman"] <- 'Women'
disaster_raw$gender_recode[is.na(disaster_raw$gender_recode)] <- 'Gender minority'
disaster_raw$gender_recode<-as.factor(disaster_raw$gender_recode)

#Sexual Orientation --> Heterosexual (0), LGB+ (1)
disaster_raw$sexual_orientation_recode[disaster_raw$sexual_orientation=="Straight/heterosexual"] <- '0'
disaster_raw$sexual_orientation_recode[is.na(disaster_raw$sexual_orientation_recode)] <- 1
disaster_raw$sexual_orientation_recode<-as.factor(disaster_raw$sexual_orientation_recode)

#Marital Status --> Not in a relationship (0) In a relationship (1)
disaster_raw$marital_recode[disaster_raw$marital=="Married"] <- '1'
disaster_raw$marital_recode[disaster_raw$marital=="Living with partner"] <- '1'
disaster_raw$marital_recode[is.na(disaster_raw$marital_recode)] <- 0
disaster_raw$marital_recode<-as.factor(disaster_raw$marital_recode)

#Education --> High school diploma/GED or equivalent or fewer years (0) Some post-secondary or greater (1)
disaster_raw$education_recode[disaster_raw$education=="Grade 4 or less"] <- '0'
disaster_raw$education_recode[disaster_raw$education=="Grade 5 to 8"] <- '0'
disaster_raw$education_recode[disaster_raw$education=="Grade 9 to 11"] <- '0'
disaster_raw$education_recode[disaster_raw$education=="Grade 12 (no diploma)"] <- '0'
disaster_raw$education_recode[disaster_raw$education=="Regular High School Diploma"] <- '0'
disaster_raw$education_recode[disaster_raw$education=="GED or alternative credential"] <- '0'
disaster_raw$education_recode[is.na(disaster_raw$education_recode)] <- 1
disaster_raw$education_recode<-as.factor(disaster_raw$education_recode)

#Race --> White, Non-Latinx (0) Minority race, ethnic minority, multiple races (1) (Did not use)
disaster_raw$race_recode[disaster_raw$race=="White or Caucasian (not Hispanic or Latino)"] <- '0'
disaster_raw$race_recode[is.na(disaster_raw$race_recode)] <- 1
disaster_raw$race_recode<-as.factor(disaster_raw$race_recode)

#Race v2 --> Combine Hispanic (Black, AA, All other), Combine Native American, All other
disaster_raw$race<-as.character(disaster_raw$race)
disaster_raw$race_recode2<-disaster_raw$race
disaster_raw$race_recode2[disaster_raw$race_recode2=="Hispanic or Latino (Black or African-American)"|disaster_raw$race_recode2=="Hispanic or Latino (all other races/multiple races)"] <- "Hispanic or Latino (Black, African-American, all other races)"
disaster_raw$race_recode2[disaster_raw$race_recode2=="Native American, Alaska Native, Aleutian"|disaster_raw$race_recode2=="Other"|disaster_raw$race_recode2=="Prefer not to answer"] <- "Native American and all others"
disaster_raw$race_recode2<-as.factor(disaster_raw$race_recode2)
levels(disaster_raw$race_recode2)

#Age of children --> None under 18 (0) Under 18 (1)
disaster_raw$age_children_recode[disaster_raw$age_children=="None Under 18"] <- '0'
disaster_raw$age_children_recode[is.na(disaster_raw$age_children_recode)] <- 1
disaster_raw$age_children_recode<-as.factor(disaster_raw$age_children_recode)

#Household Income --> 25,000 increments (Did not use)
disaster_raw$income_recode[disaster_raw$household_income=="Less than $5,000"|
                             disaster_raw$household_income=="$5,000-$9,999"|
                             disaster_raw$household_income=="$10,000-$14,999"|
                             disaster_raw$household_income=="$15,000-$19,999"|
                             disaster_raw$household_income=="$20,000-$24,999"] <- "Less than $24,999"
disaster_raw$income_recode[disaster_raw$household_income=="$25,000-$29,999"|
                             disaster_raw$household_income=="$30,000-$34,999"|
                             disaster_raw$household_income=="$35,000-$39,999"|
                             disaster_raw$household_income=="$40,000-$44,999"|
                             disaster_raw$household_income=="$45,000-$49,999"]<- "$25,000-$49,999"
disaster_raw$income_recode[disaster_raw$household_income=="$50,000-$54,999"|
                             disaster_raw$household_income=="$55,000-$59,999"|
                             disaster_raw$household_income=="$60,000-$64,999"|
                             disaster_raw$household_income=="$65,000-$69,999"|
                             disaster_raw$household_income=="$70,000-$74,999"]<- "$50,000-$74,999"
disaster_raw$income_recode[disaster_raw$household_income=="$75,000-$79,999"|
                             disaster_raw$household_income=="$80,000-$89,999"|
                             disaster_raw$household_income=="$90,000-$99,999"|
                             disaster_raw$household_income=="$65,000-$69,999"|
                             disaster_raw$household_income=="$70,000-$74,999"]<- "$75,000-$99,999"
disaster_raw$income_recode[disaster_raw$household_income=="$100,000-$124,999"]<- "$100,000-$124,999"
disaster_raw$income_recode[disaster_raw$household_income=="$125,000-$149,999"]<- "$125,000-$149,999"
disaster_raw$income_recode[disaster_raw$household_income=="$150,000-$199,999"|
                             disaster_raw$household_income=="$200,000-$249,999"|
                             disaster_raw$household_income=="$250,000 or more"]<- "$150,000 or greater"

levels(disaster_raw$income_recode)
disaster_raw$income_recode<-factor(disaster_raw$income_recode, ordered = TRUE,
                                   levels = c("Less than $24,999", "25,000-$49,999",
                                              "$50,000-$74,999", "$75,000-$99,999",
                                              "$100,000-$124,999", "$125,000-$149,999", "$150,000 or greater"))

disaster_raw$household_income<-factor(disaster_raw$household_income, ordered = TRUE,
                                      levels = c("Less than $5,000", "$5,000-$9,999",
                                                 "$10,000-$14,999", "$15,000-$19,999",
                                                 "$20,000-$24,999", "$25,000-$29,999",
                                                 "$30,000-$34,999", "$35,000-$39,999",
                                                 "$40,000-$44,999", "$45,000-$49,999",
                                                 "$50,000-$54,999", "$55,000-$59,999",
                                                 "$60,000-$64,999", "$65,000-$69,999",
                                                 "$70,000-$74,999", "$75,000-$79,999",
                                                 "$80,000-$89,999",  "$90,000-$99,999",
                                                 "$100,000-$124,999", "$125,000-$149,999", 
                                                 "$150,000-$199,999",  "$200,000-$249,999", 
                                                 "$250,000 or more"))

levels(disaster_raw$household_income)

#Microchip/ID tag --> Yes, at least one (1) None (0)
disaster_raw$micro_id[disaster_raw$microchip_have=="Yes"]<- 1
disaster_raw$micro_id[disaster_raw$id_have=="Yes"]<- 1
disaster_raw$micro_id[disaster_raw$none_have=="Yes"]<- 0
table(disaster_raw$micro_id)
disaster_raw$micro_id<-as.factor(disaster_raw$micro_id)
levels(disaster_raw$micro_id)

#Have a disaster plan --> No, Not sure (0), Yes (1)
disaster_raw$disaster_plan_recode[(disaster_raw$disaster_plan == "Yes")]<-"1"
disaster_raw$disaster_plan_recode[(disaster_raw$disaster_plan == "No")]<-"0"
disaster_raw$disaster_plan_recode[(disaster_raw$disaster_plan == "Not sure")]<-"0"
table(disaster_raw$disaster_plan_recode)
disaster_raw$disaster_plan_recode<-as.factor(disaster_raw$disaster_plan_recode)

##Disaster type --> total number of disasters experienced in the area (Did not use)
disaster_raw$hurricane_type<-as.factor(ifelse(disaster_raw$hurricane_type=="Yes", 1, 0))
disaster_raw$flood_type<-as.factor(ifelse(disaster_raw$flood_type=="Yes", 1, 0))
disaster_raw$wildfire_type<-as.factor(ifelse(disaster_raw$wildfire_type=="Yes", 1, 0))
disaster_raw$tornado_type<-as.factor(ifelse(disaster_raw$tornado_type=="Yes", 1, 0))
disaster_raw$earthquake_type<-as.factor(ifelse(disaster_raw$earthquake_type=="Yes", 1, 0))
disaster_raw$ice_storm_type<-as.factor(ifelse(disaster_raw$ice_storm_type=="Yes", 1, 0))
disaster_raw$hazardous_type<-as.factor(ifelse(disaster_raw$hazardous_type=="Yes", 1, 0))
disaster_raw$other_type<-as.factor(ifelse(disaster_raw$other_type=="Yes", 1, 0))
disaster_raw$none_type<-as.factor(ifelse(disaster_raw$none_type=="Yes", 1, 0))

##Total number of supplies (Did not use)
disaster_raw$secured_transportation<-ifelse(disaster_raw$secured_transportation=="Yes", 1, 0)
disaster_raw$secured_emergency_housing<-ifelse(disaster_raw$secured_emergency_housing=="Yes", 1, 0)
disaster_raw$travel_carrier<-ifelse(disaster_raw$travel_carrier=="Yes", 1, 0)
disaster_raw$food_water<-ifelse(disaster_raw$food_water=="Yes", 1, 0)
disaster_raw$litter<-ifelse(disaster_raw$litter=="Yes", 1, 0)
disaster_raw$pet_id<-ifelse(disaster_raw$pet_id=="Yes", 1, 0)
disaster_raw$medication<-ifelse(disaster_raw$medication=="Yes", 1, 0)
disaster_raw$other_supplies<-ifelse(disaster_raw$other_supplies=="Yes", 1, 0)
disaster_raw$none_supplies<-ifelse(disaster_raw$none_supplies=="Yes", 1, 0)
disaster_raw<-disaster_raw %>%  mutate(total_supplies= select(., secured_transportation:other_supplies) %>% rowSums(na.rm = FALSE))

##Turn independent variables into factors
disaster_raw<-disaster_raw %>% mutate(across(secured_transportation:none_supplies, as.factor))

disaster_raw$concern_disaster<-factor(disaster_raw$concern_disaster, ordered = TRUE,
                                      levels = c("Not concerned at all", "Somewhat not concerned",
                                                 "Neither concerned nor not concerned", "Somewhat concerned",
                                                 "Very concerned"))
disaster_raw$prepared_disaster<-factor(disaster_raw$prepared_disaster, ordered = TRUE,
                                       levels = c("Not prepared at all", "Somewhat not prepared",
                                                  "Neither prepared nor not prepared", "Somewhat prepared",
                                                  "Very prepared"))
disaster_raw$urb_rur<-as.factor(disaster_raw$urb_rur)

disaster_raw$ever_evacuate<-as.factor(ifelse(disaster_raw$ever_evacuate=="Yes", 1, 0))

##Weight Variable - Descriptive results
table1(~ Weightvar, data = disaster_raw)

##Independent variables - Unweighted descriptive results
table1(~ resp_age + gender + sexual_orientation + census_region + state + urb_rur + census_region + marital + household_income + education + race +
         hispanic_ethnicity + as.factor(num_children) + age_children, data = disaster_raw)
table1(~ hurricane_type + flood_type + wildfire_type + tornado_type + earthquake_type + ice_storm_type +
         hazardous_type + other_type + none_type, data = disaster_raw)
table1(~ prepared_disaster + concern_disaster + disaster_plan, data = disaster_raw)
table1(~ total_pets_owned + dog_current_own + cat_current_own + horse_current_own+
         small_current_own+livestock_current_own, data = disaster_raw)
current_pet_owners<-disaster_raw%>%
  mutate_all(na_if,"") %>%
  filter(would_bring_pet != "NA")%>%
  filter(total_pets_owned != 0)%>%
  select(Weightvar, secured_transportation , secured_emergency_housing , 
         travel_carrier, food_water , litter ,pet_id , medication ,
         other_supplies , none_supplies, microchip_have, id_have, none_have)
table1(~ secured_transportation + secured_emergency_housing + 
         travel_carrier+ food_water + litter +pet_id + medication +
         other_supplies + none_supplies +microchip_have+ id_have+ none_have, data = current_pet_owners)
disaster_raw$yes_pets<-ifelse(disaster_raw$total_pets_owned>0, 1, 0)
##Sociodemographic factors - Weighted descriptive results 
wtd.mean(disaster_raw$resp_age, disaster_raw$Weightvar)
wtd.var(disaster_raw$resp_age, disaster_raw$Weightvar)
sqrt(wtd.var(disaster_raw$resp_age, disaster_raw$Weightvar))
wtd.mean(disaster_raw$num_children, disaster_raw$Weightvar)
wtd.var(disaster_raw$num_children, disaster_raw$Weightvar)
sqrt(wtd.var(disaster_raw$num_children, disaster_raw$Weightvar))
aggregate(Weightvar ~ resp_age, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ gender_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ sexual_orientation_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ census_region, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ state, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ urb_rur, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ marital_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ income_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ education_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ race_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ hispanic_ethnicity, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ num_children, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ age_children_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ pet_id, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ disaster_plan_recode, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ brought_pet, data = disaster_raw, FUN = sum)
##Pets left behind and pets owned - Weighted descriptive results 
aggregate(Weightvar ~ left_pet, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ livestock_left_behind, data = disaster_raw, FUN = sum)
weighted_mean(x=disaster_raw$total_pets_owned, w=disaster_raw$Weightvar)
weighted_sd(x=disaster_raw$total_pets_owned, w=disaster_raw$Weightvar) 
weighted_mean(x=disaster_raw$livestock_current_own, w=disaster_raw$Weightvar)
weighted_sd(x=disaster_raw$livestock_current_own, w=disaster_raw$Weightvar)
#Type disaster - Weighted descriptive results 
aggregate(Weightvar ~ hurricane_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ flood_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ wildfire_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ tornado_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ earthquake_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ ice_storm_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ hazardous_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ other_type, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ none_type, data = disaster_raw, FUN = sum)
#Concern for disaster - Weighted descriptive results 
aggregate(Weightvar ~ concern_disaster, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ prepared_disaster, data = disaster_raw, FUN = sum)
#Disaster Plan - Weighted descriptive results 
aggregate(Weightvar ~ disaster_plan, data = disaster_raw, FUN = sum)
#Disaster plan supplies - Weighted descriptive results 
aggregate(Weightvar ~ secured_transportation, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ secured_emergency_housing, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ travel_carrier, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ food_water, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ litter, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ pet_id, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ medication, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ other_supplies, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ none_supplies, data = disaster_raw, FUN = sum)
#Microchip, ID  - Weighted descriptive results 
aggregate(Weightvar ~ microchip_have, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ id_have, data = disaster_raw, FUN = sum)
aggregate(Weightvar ~ none_have, data = disaster_raw, FUN = sum)

###Correlation matrix - independent variables
disaster_raw$household_income<-as.numeric(disaster_raw$household_income)
corr_data<-disaster_raw%>%
  select(resp_age, gender_recode, sexual_orientation_recode, marital_recode, household_income,
         education_recode, race_recode, age_children_recode, total_pets_owned, hurricane_type,
         flood_type, wildfire_type, tornado_type, earthquake_type, ice_storm_type, concern_disaster,
         prepared_disaster, disaster_plan_recode, secured_transportation, secured_emergency_housing, travel_carrier, 
         total_supplies)
corr_data <- corr_data %>% 
  mutate_at(c(1:3, 5, 7:9, 11:16, 19:22), as.numeric)
corr_data<-corr_data%>%
  mutate_if(is.factor, ~ as.numeric(.x))
corr_table<-rcorr(as.matrix(corr_data), type = "spearman")
corr_table_r<-as.data.frame(corr_table$r)
corr_table_p<-as.data.frame(corr_table$P)
write.csv(corr_table_r, "corr_table_r_apr1.csv")

#RQ1: Are sociodemographic factors associated with pet owners' likelihood of leaving pets behind during evacuation experiences?
#Have you ever had to evacuate from your home during a disaster or emergency? = YES
#When you evacuated from your home, were you a pet owner? = YES
RQ1_data<-disaster_raw%>%
filter(ever_evacuate==1 & evacuated_pet_owner == 'Yes')%>%
  dplyr::select(Weightvar, brought_pet,resp_age, gender_recode, urb_rur, sexual_orientation_recode, marital_recode, 
         education_recode, race_recode2, age_children_recode, hurricane_type,
         flood_type, wildfire_type, tornado_type, earthquake_type, ice_storm_type, household_income)
aggregate(Weightvar ~ brought_pet, data = RQ1_data, FUN = sum)
table(RQ1_data$brought_pet)

###Weighted mean/SD by 'brought_pet'
RQ1_data$household_income<-as.numeric(RQ1_data$household_income)
brought_pet_1<-RQ1_data %>%
  filter(brought_pet==1)
brought_pet_1%>%
  summarise(mean_age=mean(brought_pet_1$household_income), sd_age=sqrt(var(brought_pet_1$household_income)), med_age=median(brought_pet_1$household_income))
brought_pet_1 %>%
  summarise(wm_var=weighted.mean(brought_pet_1$household_income, brought_pet_1$Weightvar), wsd_var=sqrt(wtd.var(brought_pet_1$household_income, brought_pet_1$Weightvar)))

brought_pet_0<-RQ1_data %>%
  filter(brought_pet==0)
brought_pet_0%>%
  summarise(mean_age=mean(brought_pet_0$household_income), sd_age=sqrt(var(brought_pet_0$household_income)), med_age=median(brought_pet_0$household_income))
brought_pet_0 %>%
  summarise(wm_var=weighted.mean(brought_pet_0$household_income, brought_pet_0$Weightvar), wsd_var=sqrt(wtd.var(brought_pet_0$household_income, brought_pet_0$Weightvar)))

###Weighted frequencies by 'brought_pet'
RQ1_data %>%
  group_by(gender_recode)%>%
  count(brought_pet , wt = Weightvar) %>% mutate(wt_per = (n/sum(n))*100)

RQ1_data %>%
  group_by(gender_recode)%>%
  count(brought_pet) %>% mutate(per = (n/sum(n))*100)

###Full Model - RQ1 
RQ1_data$household_income<-as.numeric(RQ1_data$household_income)
RQ1_data$race_recode2<- relevel(RQ1_data$race_recode2,"White or Caucasian (not Hispanic or Latino)")
RQ1_data$gender_recode<- relevel(RQ1_data$gender_recode,"Men")

RQ1_full<- glm(brought_pet~resp_age+ gender_recode+ urb_rur+ sexual_orientation_recode+ marital_recode+ household_income+
                       education_recode+race_recode2+ age_children_recode+ hurricane_type+
                       flood_type+ wildfire_type+ tornado_type+ earthquake_type+ ice_storm_type+race_recode2*household_income, data = RQ1_data, family = "binomial", weights=Weightvar)

#Full model summary
summary(RQ1_full) #Summary
car::vif(RQ1_full) #Variance inflation model
RQ1_full_summary<-tidy(RQ1_full, exponentiate = TRUE, conf.level = 0.95)
OR_RQ1_full<-exp(cbind(coef(RQ1_full), confint(RQ1_full)))
write.csv(RQ1_full_summary, 'RQ1_full_summary.csv')
write.csv(OR_RQ1_full, 'OR_RQ1_full.csv')

#Estimated marginal means: 
ggeffect(RQ1_full)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ1_full)
# Backwards stepwise elimination
step(RQ1_full,direction="backward")


###STEP 5: removed Education, Hurricane, Age children, Tornado, Ice Storm
RQ1_step5<- glm(brought_pet~resp_age+ gender_recode+ urb_rur+ sexual_orientation_recode+ marital_recode+ household_income+
                 race_recode2+flood_type+ wildfire_type+ earthquake_type+ race_recode2*household_income, data = RQ1_data, family = "binomial", weights=Weightvar)

#Final model summary
summary(RQ1_step5) #Summary
car::vif(RQ1_step5) #Variance inflation factor
RQ1_step5_summary<-tidy(RQ1_step5, exponentiate = TRUE, conf.level = 0.95)
OR_RQ1_step5<-exp(cbind(coef(RQ1_step5), confint(RQ1_step5)))
write.csv(RQ1_step5_summary, 'RQ1_step5_summary.csv')
write.csv(OR_RQ1_step5, 'OR_RQ1_step5.csv')

#Estimated marginal means: 
ggeffect(RQ1_step5)
emmeans(RQ1_step5)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ1_step5)
# Backwards stepwise elimination
step(RQ1_step5,direction="backward")

###RQ1A: What reasons do people leave a pet behind?
#Chi-squared goodness of fit test
RQ1A_data<-disaster_raw%>%
  filter(ever_evacuate==1 & evacuated_pet_owner == 'Yes' & pet_friendly_accom_reason != 'NA')%>%
  dplyr::select(pet_friendly_accom_reason:other_text_reason)%>%
  dplyr::mutate(obs = 1:n())
table(RQ1A_data$obs)

RQ1A_group<-RQ1A_data%>%
  pivot_longer(pet_friendly_accom_reason:other_reason, names_to= "reason", values_to = "yes_no")%>%
  group_by(reason,yes_no)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  filter(yes_no == "Yes")
RQ1A_group$reason<-as.factor(RQ1A_group$reason)
levels(RQ1A_group$reason)

#Cochran's Q: Compare proportion of successes across groups
RQ1A_long<-RQ1A_data%>%
  select(-other_reason,-other_text_reason)%>%
  pivot_longer(pet_friendly_accom_reason:behaviour_reason, names_to= "reason", values_to = "outcome")%>%
  arrange(obs)
RQ1A_long$outcome<-as.factor(ifelse(RQ1A_long$outcome=="Yes", 1, 0))
RQ1A_long$reason<-as.factor(RQ1A_long$reason)
RQ1A_long$obs<-as.factor(RQ1A_long$obs)

cochran.qtest(outcome ~ reason| obs, RQ1A_long)
options(scipen = 999)

RQ1A_pairwise<-as.data.frame(pairwiseMcnemar(outcome ~ reason| obs,
                     data   = RQ1A_long,
                     test   = "permutation",
                     method = "fdr",
                     digits = 3))

write.csv(RQ1A_pairwise, 'RQ1A_pairwise.csv')
table(RQ1A_long$reason, RQ1A_long$obs)
xtabs(~Respondent_Serial,RQ1A_long)

####RQ2: Are sociodemographic factors associated with pet owners' intent of leaving pets behind during evacuation experiences?
RQ2_data<-disaster_raw%>%
  mutate_all(na_if,"") %>%
  filter(would_bring_pet != "NA")%>%
  filter(total_pets_owned != 0)%>%
  select(Weightvar, would_bring_recode, brought_pet_RQ2, resp_age, gender_recode, urb_rur, sexual_orientation_recode, marital_recode, 
         education_recode, race_recode2, age_children_recode, total_pets_owned, hurricane_type,
         flood_type, wildfire_type, tornado_type, earthquake_type, ice_storm_type, concern_disaster,
         prepared_disaster, disaster_plan_recode, household_income, secured_emergency_housing, secured_transportation,
         travel_carrier, ever_evacuate)
aggregate(Weightvar ~ would_bring_recode, data = RQ2_data, FUN = sum)
table(RQ2_data$would_bring_recode)

###Weighted mean/SD by 'would_bring_recode'
RQ2_data$prepared_disaster<-as.numeric(RQ2_data$prepared_disaster)
would_bring_recode_1<-RQ2_data %>%
  filter(would_bring_recode==1)
would_bring_recode_1%>%
  summarise(mean_age=mean(would_bring_recode_1$prepared_disaster), sd_age=sqrt(var(would_bring_recode_1$prepared_disaster)), med_age=median(would_bring_recode_1$prepared_disaster))
would_bring_recode_1 %>%
  summarise(wm_var=weighted.mean(would_bring_recode_1$prepared_disaster, would_bring_recode_1$Weightvar), wsd_var=sqrt(wtd.var(would_bring_recode_1$prepared_disaster, would_bring_recode_1$Weightvar)))

would_bring_recode_0<-RQ2_data %>%
  filter(would_bring_recode==0)
would_bring_recode_0%>%
  summarise(mean_age=mean(would_bring_recode_0$prepared_disaster), sd_age=sqrt(var(would_bring_recode_0$prepared_disaster)), med_age=median(would_bring_recode_0$prepared_disaster))
would_bring_recode_0 %>%
  summarise(wm_var=weighted.mean(would_bring_recode_0$prepared_disaster, would_bring_recode_0$Weightvar), wsd_var=sqrt(wtd.var(would_bring_recode_0$prepared_disaster, would_bring_recode_0$Weightvar)))

###Weighted frequencies by 'would_bring_recode'
RQ2_data %>%
  group_by(brought_pet_RQ2)%>%
  count(would_bring_recode ) %>% mutate(per = (n/sum(n))*100)

RQ2_data %>%
  group_by(brought_pet_RQ2)%>%
  count(would_bring_recode , wt = Weightvar) %>% mutate(weight_per = (n/sum(n))*100)

aggregate(Weightvar ~ ever_evacuate, data = RQ2_data, FUN = sum)
table(RQ2_data$would_bring_recode)

###Full Model - RQ2
RQ2_data$household_income<-as.numeric(RQ2_data$household_income)
RQ2_data$race_recode2<- relevel(RQ2_data$race_recode2,"White or Caucasian (not Hispanic or Latino)")
RQ2_data$concern_disaster<-as.numeric(RQ2_data$concern_disaster)
RQ2_data$prepared_disaster<-as.numeric(RQ2_data$prepared_disaster)
RQ2_data$gender_recode<- relevel(RQ2_data$gender_recode,"Men")

RQ2_full<- glm(would_bring_recode~resp_age+ gender_recode+ urb_rur+ sexual_orientation_recode+ marital_recode+ household_income+
                 education_recode+ race_recode2+ age_children_recode+ total_pets_owned+ hurricane_type+
                 flood_type+ wildfire_type+ tornado_type+ earthquake_type+ ice_storm_type+ concern_disaster+
                 prepared_disaster+ disaster_plan_recode + secured_emergency_housing + secured_transportation + 
               travel_carrier + ever_evacuate +brought_pet_RQ2+ race_recode2*household_income, data = RQ2_data, family = "binomial", weights=Weightvar)

#Full model summary
summary(RQ2_full) #Summary
car::vif(RQ2_full) #Variance inflation model
RQ2_full_summary<-tidy(RQ2_full, exponentiate = TRUE, conf.level = 0.95)
OR_RQ2_full<-exp(cbind(coef(RQ2_full), confint(RQ2_full)))
write.csv(RQ2_full_summary, 'RQ2_full_summary.csv')
write.csv(OR_RQ2_full, 'OR_RQ2_full.csv')

#Estimated marginal means: 
ggeffect(RQ2_full)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ2_full)
# Backwards stepwise elimination
step(RQ2_full,direction="backward")

#RQ2 (Step 12)
RQ2_step12<- glm(would_bring_recode~ gender_recode+ urb_rur+ household_income+
                   race_recode2+ age_children_recode+ total_pets_owned+ hurricane_type+
                   wildfire_type+  secured_transportation + 
                   travel_carrier + ever_evacuate +brought_pet_RQ2, data = RQ2_data, family = "binomial", weights=Weightvar)

#Final model summary
summary(RQ2_step12) #Summary
car::vif(RQ2_step12) #Variance inflation model
RQ2_step12_summary<-tidy(RQ2_step12, exponentiate = TRUE, conf.level = 0.95)
OR_RQ2_step12<-exp(cbind(coef(RQ2_step12), confint(RQ2_step12)))
write.csv(RQ2_step12_summary, 'RQ2_step12_summary.csv')
write.csv(OR_RQ2_step12, 'OR_RQ2_step12.csv')

#Estimated marginal means: 
ggeffect(RQ2_step12)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ2_step12)

####RQ3: Are sociodemographic factors associated with support for government funding for pet-friendly housing?
RQ3_data<-disaster_raw%>%
  select(Weightvar, support_gov_recode,resp_age, gender_recode, urb_rur, sexual_orientation_recode, marital_recode, 
         education_recode, race_recode2, age_children_recode, total_pets_owned, hurricane_type,
         flood_type, wildfire_type, tornado_type, earthquake_type, ice_storm_type, concern_disaster,
         prepared_disaster, disaster_plan_recode, household_income, secured_emergency_housing, secured_transportation,
         travel_carrier, ever_evacuate, brought_pet_RQ2)

aggregate(Weightvar ~ support_gov_recode, data = RQ3_data, FUN = sum)
table(RQ3_data$support_gov_recode)
sum(is.na(RQ3_data))

#Weighted mean/SD by 'support_gov_recode'
RQ3_data$prepared_disaster<-as.numeric(RQ3_data$prepared_disaster)
support_gov_recode_1<-RQ3_data %>%
  filter(support_gov_recode==1)
support_gov_recode_1%>%
  summarise(mean_age=mean(support_gov_recode_1$prepared_disaster), sd_age=sqrt(var(support_gov_recode_1$prepared_disaster)), med_age=median(support_gov_recode_1$prepared_disaster))
support_gov_recode_1 %>%
  summarise(wm_var=weighted.mean(support_gov_recode_1$prepared_disaster, support_gov_recode_1$Weightvar), wsd_var=sqrt(wtd.var(support_gov_recode_1$prepared_disaster, support_gov_recode_1$Weightvar)))

support_gov_recode_0<-RQ3_data %>%
  filter(support_gov_recode==0)
support_gov_recode_0%>%
  summarise(mean_age=mean(support_gov_recode_0$prepared_disaster), sd_age=sqrt(var(support_gov_recode_0$prepared_disaster)), med_age=median(support_gov_recode_0$prepared_disaster))
support_gov_recode_0 %>%
  summarise(wm_var=weighted.mean(support_gov_recode_0$prepared_disaster, support_gov_recode_0$Weightvar), wsd_var=sqrt(wtd.var(support_gov_recode_0$prepared_disaster, support_gov_recode_0$Weightvar)))

#Weighted frequencies by 'support_gov_recode'
RQ3_data %>%
  group_by(brought_pet_RQ2)%>%
  count(support_gov_recode) %>% mutate(per = (n/sum(n))*100)

RQ3_data %>%
  group_by(brought_pet_RQ2)%>%
  count(support_gov_recode , wt = Weightvar) %>% mutate(per = (n/sum(n))*100)

aggregate(Weightvar ~ ever_evacuate, data = RQ3_data, FUN = sum)
table(RQ3_data$support_gov_recode)

esquisser()

###Full Model - RQ3
RQ3_data$household_income<-as.numeric(RQ3_data$household_income)
RQ3_data$race_recode2<- relevel(RQ3_data$race_recode2,"White or Caucasian (not Hispanic or Latino)")
RQ3_data$concern_disaster<-as.numeric(RQ3_data$concern_disaster)
RQ3_data$prepared_disaster<-as.numeric(RQ3_data$prepared_disaster)
RQ3_data$gender_recode<- relevel(RQ3_data$gender_recode,"Men")

RQ3_full<- glm(support_gov_recode~resp_age+ gender_recode+ urb_rur+ sexual_orientation_recode+ marital_recode+ household_income+
                 education_recode+ race_recode2+ age_children_recode+ total_pets_owned+ hurricane_type+
                 flood_type+ wildfire_type+ tornado_type+ earthquake_type+ ice_storm_type+ concern_disaster+
                 prepared_disaster+ disaster_plan_recode + ever_evacuate + brought_pet_RQ2+ race_recode2*household_income, data = RQ3_data, family = "binomial", weights=Weightvar)

#Full model summary
summary(RQ3_full) #Summary
car::vif(RQ3_full) #Variance inflation model
RQ3_full_summary<-tidy(RQ3_full, exponentiate = TRUE, conf.level = 0.95)
OR_RQ3_full<-exp(cbind(coef(RQ3_full), confint(RQ3_full)))
write.csv(RQ3_full_summary, 'RQ3_full_summary.csv')
write.csv(OR_RQ3_full, 'OR_RQ3_full.csv')

#Estimated marginal means: 
ggeffect(RQ3_full)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ3_full)
# Backwards stepwise elimination
step(RQ3_full,direction="backward")

#RQ3 Final Model (Step 10)
RQ3_step11<- glm(support_gov_recode~resp_age+ gender_recode+ urb_rur+ sexual_orientation_recode+ household_income+
                    race_recode2+ total_pets_owned+ earthquake_type+ concern_disaster+
                  disaster_plan_recode + ever_evacuate, data = RQ3_data, family = "binomial", weights=Weightvar)

#Final model summary
summary(RQ3_step11) #Summary
car::vif(RQ3_step11) #Variance inflation model
RQ3_step11_summary<-tidy(RQ3_step11, exponentiate = TRUE, conf.level = 0.95)
OR_RQ3_step11<-exp(cbind(coef(RQ3_step11), confint(RQ3_step11)))
write.csv(RQ3_step11_summary, 'RQ3_step11_summary.csv')
write.csv(OR_RQ3_step11, 'OR_RQ3_step11.csv')

#Estimated marginal means: 
ggeffect(RQ3_step11)
# Pseudo R_squared values and Likelyhood ratio test
nagelkerke(RQ3_step11)

