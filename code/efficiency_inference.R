"
Authors: Sophie Berdugo & Arran J. Davis
Emails: sophie.berdugo@keble.ox.ac.uk | arran.davis@anthro.ox.ac.uk; davis.arran@gmail.com
Affiliation: Social Body Lab, Institute of Human Sciences, University of Oxford
Date: 4 July 2023
"

library(lme4)
library(lmerTest)
library(sjPlot)
library(effects)
library(tidyr)
library(ggplot2)
library(extrafont)
library(performance)
library(RColorBrewer)
library(corrplot)
library(ggcorrplot)
library(car)
library(VGAM)
library(glmmTMB)
library(MASS)
library(dplyr)
library(GGally)
library(geomtextpath)
library(smplot2)
library(forcats)

#clean environment
rm(list = ls())

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

################################################################################################################################################

### LOAD AND PREPARE THE DATA ###

#function for "not in"
'%!in%' = function(x,y)!('%in%'(x,y))

#load and subset the data
efficiency_data = read.csv('../clean_data/cleaned_efficiency_data.csv')
efficiency_data = subset(efficiency_data, efficiency_data$bout_outcome != "Failed" & 
                         efficiency_data$nut_species != "Coula nut" &
                         efficiency_data$bout_outcome != "None" &
                         efficiency_data$learner %!in% c("Clinging","Clinging and Peering"))

#make individual and sex a factor
efficiency_data$sex = as.factor(efficiency_data$sex)
efficiency_data$subject = as.factor(efficiency_data$subject)

################################################################################################################################################

### GRAPH THEMES ###

#theme
header_size = 15
axis_size = 12

#theme for plots
helvetica_theme = theme(text=element_text(size = header_size,family='Helvetica'),
                        axis.text.x = element_text(color = 'black', size = axis_size, vjust = 1),
                        axis.text.y = element_text(color = 'black', size = axis_size),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
                        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), face = "bold"),
                        panel.background = element_blank(),
                        panel.grid.major.x = element_line(color = '#e7e7e7'),
                        panel.grid.major.y = element_line(color = '#e7e7e7'),
                        legend.key = element_blank(),
                        legend.title = element_text(face = "bold"),
                        plot.title = element_text(hjust = 0.5, face = "bold"))

nature_human_behavior = theme(text=element_text(size = 7 , family = 'Helvetica'),
                          axis.text.x = element_text(color = 'black', size = 7, vjust = 1),
                          axis.text.y = element_text(color = 'black', size = 7),
                          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold", size = 7),
                          axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), face = "bold", size = 7),
                          panel.background = element_blank(),
                          panel.grid.major.x = element_line(color = '#e7e7e7'),
                          panel.grid.major.y = element_line(color = '#e7e7e7'),
                          legend.key = element_blank(),
                          legend.title = element_text(face = "bold"),
                          plot.title = element_text(hjust = 0.5, face = "bold"))

#set the font
par(family = 'helvetica')

################################################################################################################################################

### VISUALISE THE DATA ###

#subset the data into age groups
age_6_10 = subset(efficiency_data, (efficiency_data$age >= 6 & efficiency_data$age <= 10))
age_11_20 = subset(efficiency_data, (efficiency_data$age >= 11 & efficiency_data$age <= 20))
age_21_30 = subset(efficiency_data, (efficiency_data$age >= 21 & efficiency_data$age <= 30))
age_31_40 = subset(efficiency_data, (efficiency_data$age >= 31 & efficiency_data$age <= 40))
age_41_50 = subset(efficiency_data, (efficiency_data$age >= 41 & efficiency_data$age <= 50))
age_51_60 = subset(efficiency_data, (efficiency_data$age >= 51))

#demarcate each of the age groupings by adding a new column for "AgeGroup"
age_6_10$AgeGroup = "6 to 10" 
age_11_20$AgeGroup = "11 to 20"
age_21_30$AgeGroup = "21 to 30"
age_31_40$AgeGroup = "31 to 40"
age_41_50$AgeGroup = "41 to 50"
age_51_60$AgeGroup = "51 to 60"

#add all of the age groups together into a variable called all_age
all_age = do.call("rbind", list(age_6_10, age_11_20, age_21_30,
                                age_31_40, age_41_50, age_51_60)) 

#order the age group factor into ascending order
all_age$AgeGroup = factor(all_age$AgeGroup, levels = c("6 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60")) 

#order the individual factor by age group (youngest age group first)
all_age$subject_ordered = factor(all_age$subject, levels = c("Fanwaa", "Flanle", "Ja", "Joya", "Nto", "Poni", "Fotaiu", "Na", "Peley", "Pili", "Vuavua",
                                                             "Vui", "Yolo", "Fanle", "Jeje", "Foaf", "Fana", "Jire", "Kai", "Tua", "Velu", "Yo"))


#reverse the order of `subject_ordered` and `AgeGroup`
all_age$subject_ordered_rev = fct_rev(all_age$subject_ordered)
all_age$AgeGroup_rev = fct_rev(all_age$AgeGroup)

#order of age colors
age_col_order = c("#a63603", "#2171b5", "#f16913", "#6baed6", "#fdae6b", "#c6dbef")
age_col_order_rev = c("#c6dbef", "#fdae6b", "#6baed6", "#f16913", "#2171b5", "#a63603")

## PLOT BOUT DURATION ##

#plot the data as box plots
boxplot_boutduration = ggplot(all_age, aes(x = bout_duration, y = subject_ordered_rev, fill = AgeGroup)) +
                          geom_boxplot(outlier.size = .4, width = .8, position = position_dodge2(preserve = "single"), alpha = 0.9) + 
                          scale_x_continuous(breaks = seq(0, 30, 2), limits = c(0, 30)) +
                          scale_fill_manual(values = age_col_order, name = "Age (years)", 
                                            breaks = c("51 to 60", "41 to 50", "31 to 40", "21 to 30", "11 to 20", "6 to 10")) +
                          xlab("Bout duration (seconds)") +
                          ylab("Individual") + 
                          scale_y_discrete(limits = rev) + 
                          helvetica_theme

boxplot_boutduration

#save the plot 
ggsave("../plots/bout_duration_age_group_plot.jpg", boxplot_boutduration, height = 16, width = 9)

## PLOT STRIKES PER NUT ##

#plot the data as box plots
boxplot_strikespernut = ggplot(all_age, aes(x = strike_count, y = subject_ordered_rev, fill = AgeGroup)) +
                          geom_boxplot(outlier.size = .4, width = .8, position = position_dodge2(preserve = "single"), alpha= 0.9) + 
                          scale_x_continuous(breaks = seq(0, 30, 2), limits = c(1, 30)) +
                          scale_fill_manual(values = age_col_order, name = "Age (years)", 
                                            breaks = c("51 to 60", "41 to 50", "31 to 40", "21 to 30", "11 to 20", "6 to 10")) +
                          xlab("Strikes per nut") +
                          ylab("Individual") + 
                          scale_y_discrete(limits = rev) + 
                          helvetica_theme

boxplot_strikespernut

#save the plot 
ggsave("../plots/strikes_per_nut_age_group_plot.jpg", boxplot_strikespernut, height = 16, width = 9)

## PLOT DISPLACEMENT RATE ##

#load and subset the data
rate_data = read.csv('../clean_data/cleaned_efficiency_data.csv')
rate_data = subset(rate_data, rate_data$nut_species != "Coula nut" &
                     rate_data$bout_outcome != "None" &
                     rate_data$learner %!in% c("Clinging","Clinging and Peering"))

#subset the data into age groups
age_6_10_rate = subset(rate_data, (rate_data$age >= 6 & rate_data$age <= 10))
age_11_20_rate = subset(rate_data, (rate_data$age >= 11 & rate_data$age <= 20))
age_21_30_rate = subset(rate_data, (rate_data$age >= 21 & rate_data$age <= 30))
age_31_40_rate = subset(rate_data, (rate_data$age >= 31 & rate_data$age <= 40))
age_41_50_rate = subset(rate_data, (rate_data$age >= 41 & rate_data$age <= 50))
age_51_60_rate = subset(rate_data, (rate_data$age >= 51))

#demarcate each of the age groupings by adding a new column for "AgeGroup"
age_6_10_rate$AgeGroup = "6 to 10" 
age_11_20_rate$AgeGroup = "11 to 20"
age_21_30_rate$AgeGroup = "21 to 30"
age_31_40_rate$AgeGroup = "31 to 40"
age_41_50_rate$AgeGroup = "41 to 50"
age_51_60_rate$AgeGroup = "51 to 60"

#add all of the age groups together into a variable called all_age_rate
all_age_rate = do.call("rbind", list(age_6_10_rate, age_11_20_rate, age_21_30_rate,
                                     age_31_40_rate, age_41_50_rate, age_51_60_rate)) 

#order the age group factor into ascending order
all_age_rate$AgeGroup = factor(all_age_rate$AgeGroup, levels = c("6 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60"))

#order the individual factor by age group (youngest age group first
all_age_rate$subject_ordered = factor(all_age_rate$subject, levels = c("Fanwaa", "Flanle", "Ja", "Joya", "Nto", "Poni", "Fotaiu", "Na", "Peley", "Pili", "Vuavua",
                                                                       "Vui", "Yolo", "Fanle", "Jeje", "Foaf", "Fana", "Jire", "Kai", "Tua", "Velu", "Yo"))

#reverse the order of `subject_ordered` and `AgeGroup`
all_age_rate$subject_ordered_rev = fct_rev(all_age_rate$subject_ordered)
all_age_rate$AgeGroup_rev = fct_rev(all_age_rate$AgeGroup)

#plot the data
displacement_forest_plot = ggplot(all_age_rate, aes(x = displacement_count, y = subject_ordered_rev, color = AgeGroup, group = AgeGroup)) +
                              sm_forest(sep_level = 2, position = position_dodge(width = 0.5), points = FALSE, 
                                        legends = TRUE, borders = FALSE, errorbar_type = "se", point.params = list(color = "black", size = 0.4), 
                                        avgPoint.params = list(alpha = 0.9, shape = 18, size = 4.5)) +                            
                              scale_x_continuous(breaks = seq(0, 4, 1), limits = c(0, 4)) +                      
                              scale_color_manual(values = age_col_order, name = "Age (years)", 
                                                breaks = c("51 to 60", "41 to 50", "31 to 40", "21 to 30", "11 to 20", "6 to 10")) +
                              xlab("Displacements per bout") +
                              ylab("Individual") + 
                              guides(color = guide_legend(title = "Age (years)")) +
                              scale_y_discrete(limits = rev) +                      
                              helvetica_theme

displacement_forest_plot

#save the plot 
ggsave("../plots/displacement_forest_plot.jpg", displacement_forest_plot, height = 16, width = 9, bg = "white")

## PLOT TOOL SWITCH RATE ##

#save the plot
tool_switch_forest_plot = ggplot(all_age_rate, aes(x = tool_switch_count, y = subject_ordered_rev, color = AgeGroup, group = AgeGroup)) +
                              sm_forest(sep_level = 2, position = position_dodge(width = 0.5), points = TRUE, 
                                        legends = TRUE, borders = FALSE, errorbar_type = "se", point.params = list(color = "black", size = 0.35), 
                                        avgPoint.params = list(alpha = 0.9, shape = 18, size = 4.5)) +                            
                              scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5)) +                      
                              scale_color_manual(values = age_col_order, name = "Age (years)", 
                                                 breaks = c("51 to 60", "41 to 50", "31 to 40", "21 to 30", "11 to 20", "6 to 10")) +
                              xlab("Tool switches per bout") +
                              ylab("Individual") + 
                              guides(color = guide_legend(title = "Age (years)")) +
                              scale_y_discrete(limits = rev) +                      
                              helvetica_theme

tool_switch_forest_plot

#save the plot 
ggsave("../plots/tool_switch_forest_plot.jpg", tool_switch_forest_plot, height = 16, width = 9, bg = "white")

## PLOT SUCCESS RATE ##

#age 6 to 10 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_6_10 = as.data.frame(table(age_6_10_rate$subject, age_6_10_rate$bout_outcome))
success_rate_6_10 = spread(success_rate_6_10, Var2, Freq)
success_rate_6_10$AgeGroup = "6 to 10"
colnames(success_rate_6_10)[1] = "subject"

#make new columns for the proportions of each bout outcome
success_rate_6_10 <- success_rate_6_10 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_6_10 <- success_rate_6_10 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_6_10 <- success_rate_6_10 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#age 11 to 20 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_11_20 = as.data.frame(table(age_11_20_rate$subject, age_11_20_rate$bout_outcome))
success_rate_11_20 = spread(success_rate_11_20, Var2, Freq)
success_rate_11_20$AgeGroup = "11 to 20"
colnames(success_rate_11_20)[1] = "subject" 

#make new columns for the proportions of each bout outcome
success_rate_11_20 <- success_rate_11_20 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_11_20 <- success_rate_11_20 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_11_20 <- success_rate_11_20 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#age 21 to 30 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_21_30 = as.data.frame(table(age_21_30_rate$subject, age_21_30_rate$bout_outcome))
success_rate_21_30 = spread(success_rate_21_30, Var2, Freq)
success_rate_21_30$AgeGroup = "21 to 30" 
colnames(success_rate_21_30)[1] = "subject"

#make new columns for the proportions of each bout outcome
success_rate_21_30 <- success_rate_21_30 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_21_30 <- success_rate_21_30 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_21_30 <- success_rate_21_30 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#age 31 to 40 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_31_40 = as.data.frame(table(age_31_40_rate$subject, age_31_40_rate$bout_outcome))
success_rate_31_40 = spread(success_rate_31_40, Var2, Freq)
success_rate_31_40$AgeGroup = "31 to 40"
colnames(success_rate_31_40)[1] = "subject"

#make new columns for the proportions of each bout outcome
success_rate_31_40 <- success_rate_31_40 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_31_40 <- success_rate_31_40 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_31_40 <- success_rate_31_40 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#age 41 to 50 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_41_50 = as.data.frame(table(age_41_50_rate$subject, age_41_50_rate$bout_outcome))
success_rate_41_50 = spread(success_rate_41_50, Var2, Freq)
success_rate_41_50$AgeGroup = "41 to 50"
colnames(success_rate_41_50)[1] = "subject"

#make new columns for the proportions of each bout outcome
success_rate_41_50 <- success_rate_41_50 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_41_50 <- success_rate_41_50 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_41_50 <- success_rate_41_50 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#age 51 to 60 (make data frame; switch to wide format; add in the age group; rename the subject column)
success_rate_51_60 = as.data.frame(table(age_51_60_rate$subject, age_51_60_rate$bout_outcome))
success_rate_51_60 = spread(success_rate_51_60, Var2, Freq)
success_rate_51_60$AgeGroup = "51 to 60"
colnames(success_rate_51_60)[1] = "subject"

#make new columns for the proportions of each bout outcome
success_rate_51_60 <- success_rate_51_60 %>% mutate ("prop_success" = (Successful / (Successful + Smash + Failed)))
success_rate_51_60 <- success_rate_51_60 %>% mutate ("prop_smash" = (Smash / (Successful + Smash + Failed)))
success_rate_51_60 <- success_rate_51_60 %>% mutate ("prop_fail" = (Failed/ (Successful + Smash + Failed)))

#add all of the age groups together into a variable called all_age_rate
success_rate_all_ages = do.call("rbind", list(success_rate_6_10, success_rate_11_20, success_rate_21_30, success_rate_31_40, success_rate_41_50, success_rate_51_60)) 

#make `success_rate_all_ages` wide
success_rate_all_ages = gather(success_rate_all_ages, outcome, proportion, prop_success:prop_fail, factor_key = TRUE)

success_rate_all_ages = subset(success_rate_all_ages, success_rate_all_ages$outcome == "prop_success")

success_rate_all_ages$subject_ordered = factor(success_rate_all_ages$subject, levels = c("Fanwaa", "Flanle", "Ja", "Joya", "Nto", "Poni", "Fotaiu", "Na", "Peley", "Pili", "Vuavua",
                                                                                         "Vui", "Yolo", "Fanle", "Jeje", "Foaf", "Fana", "Jire", "Kai", "Tua", "Velu", "Yo"))
#order the age group factor into ascending order
success_rate_all_ages$AgeGroup = factor(success_rate_all_ages$AgeGroup, levels = c("6 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60")) 
success_rate_all_ages$outcome = factor(success_rate_all_ages$outcome, levels = c("prop_fail", "prop_smash", "prop_success")) 

#order the data by age and then success rate
success_rate_all_ages_ordered = success_rate_all_ages[with(success_rate_all_ages, order(AgeGroup, proportion)), ]

#add a row number
success_rate_all_ages_ordered$row = 1:nrow(success_rate_all_ages_ordered)

#get a list of the names for each individual
names_list = success_rate_all_ages_ordered$subject

#plot the data
success_proportion_age_o = ggplot(data = success_rate_all_ages_ordered, aes(x = row, y = proportion, fill = AgeGroup)) +
                            geom_bar(stat="identity") + 
                            scale_x_continuous(breaks = seq(1, nrow(success_rate_all_ages_ordered), 1), labels = names_list) + 
                            scale_fill_manual(breaks = c("51 to 60", "41 to 50", "31 to 40", "21 to 30", "11 to 20", "6 to 10"),
                                              values = c("#a63603", "#2171b5", "#f16913", "#6baed6", "#fdae6b", "#c6dbef"), name = "Age Group") +
                            xlab("Individual") + 
                            ylab("Proportion of bouts ending in success") +
                            guides(fill = guide_legend(title = "Age (years)")) +
                            coord_flip() + 
                            helvetica_theme
success_proportion_age_o

#save the plot 
ggsave("../plots/success_proportion_age_ordered.jpg", success_proportion_age_o, height = 16, width = 9)

################################################################################################################################################

### BOUT DURATION - INDIVIDUAL VARIATION ###

#fit models
mod1 = lmer(log(bout_duration) ~ 1 + (1 | subject), data = efficiency_data)
summary(mod1)

mod2 = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data)
summary(mod2)

#test whether the `Subject` term is significant (compare models with and without a random effect of `Subject`; it is highly significant)
mod3 = lm(log(bout_duration) ~ 1 + age + sex, data = efficiency_data)
anova(mod2, mod3)
tab_model(mod3, mod2)

### ### ###

#choose model to plot
model_re = mod2

#get random effects and their variances
model_re_tmp = lme4::ranef(model_re, condVar = TRUE)
temp_vars = as.data.frame.table((attr(lme4::ranef(model_re, condVar = TRUE)[[1]], "postVar")))

#add to a dataframe
rand_ints = model_re_tmp$subject
colnames(rand_ints)[1] = "intercept"
rand_ints$variance = temp_vars$Freq

#calculate standard deviation for intercepts and 95% CI
rand_ints$error = 2*sqrt(rand_ints$variance)
rand_ints$lower_95 = rand_ints$intercept - rand_ints$error
rand_ints$upper_95 = rand_ints$intercept + rand_ints$error

#create a subject variable
rand_ints$subject = rownames(rand_ints)

#order by size of intercept
rand_ints$subject = factor(rand_ints$subject, levels = rand_ints$subject[order(rand_ints$intercept)])

#add sex to the random intercept data frame and set as factors
rand_ints$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
rand_ints$sex = as.factor(rand_ints$sex)

#plot the data
bout_duration_ri = ggplot(rand_ints, aes(intercept, subject, color=sex)) + 
                    geom_point() + 
                    scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
                    geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
                    geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed", color = "black") +                    
                    scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = 0.5)) +    
                    ylab("Individual") +
                    xlab("Random intercept") +
                    ggtitle("Log bout duration\n(linear random intercept model)") +
                    helvetica_theme
bout_duration_ri

#save the plot 
ggsave("../plots/bout_duration_subject_random_intercepts.jpg", bout_duration_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept and create rank variable
rand_int_duration_ordered = rand_ints[order(rand_ints$intercept), ]
rand_int_duration_ordered$rank = seq.int(nrow(rand_int_duration_ordered))

################################################################################################################################################

### STRIKE PER NUT - INDIVIDUAL VARIATION ###

#fit zero truncated poisson models
spn_mod_tmb_simp = glmmTMB(strike_count ~ 1 + (1 | subject), data = efficiency_data, family = truncated_poisson(link="log"))
summary(spn_mod_tmb_simp)

spn_mod_tmb = glmmTMB(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data, family = truncated_poisson(link="log")) 
summary(spn_mod_tmb)

spn_mod_tmb_lm = glmmTMB(strike_count ~ 1 + age + sex, data = efficiency_data, family = truncated_poisson(link="log")) 
summary(spn_mod_tmb_lm)

#check for over dispersion (overdispersion detected)
check_overdispersion(spn_mod_tmb)

#fit truncated negative binomial model
spn_mod_tmb_nb = glmmTMB(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data, family = truncated_nbinom2(link="log")) 
summary(spn_mod_tmb_nb)

spn_mod_tmb_nb_lm = glmmTMB(strike_count ~ 1 + age + sex, data = efficiency_data, family = truncated_nbinom2(link="log")) 
summary(spn_mod_tmb_nb_lm)

#compare the models
anova(spn_mod_tmb_nb, spn_mod_tmb_nb_lm)
tab_model(spn_mod_tmb_nb_lm, spn_mod_tmb_nb)

### ### ###

#choose model to plot
mod = spn_mod_tmb_nb

#get random effects and confidence intervals
mod_re = as.data.frame(lme4::ranef(mod, condVar = TRUE))
mod_re$intercept = mod_re$condval
mod_re$lower_95 = mod_re$intercept - (qnorm(0.975) * mod_re$condsd)
mod_re$upper_95 = mod_re$intercept + (qnorm(0.975) * mod_re$condsd)

#add the `Subject` variable and order it according to intercept size
mod_re$subject = mod_re$grp
mod_re$subject = factor(mod_re$subject, levels = mod_re$subject[order(mod_re$intercept)])

#add sex to the random intercept data frame and set as factors
mod_re$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
mod_re$sex = as.factor(mod_re$sex)

#plot the data
strikes_ri = ggplot(mod_re, aes(intercept, subject, color=sex)) + 
              geom_point() + 
              scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
              geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
              geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
              scale_x_continuous(breaks = seq(-2, 2, .5), limits = c(-2,2)) +
              ylab("Individual") +
              xlab("Random intercept") +
              ggtitle("Strikes per nut\n(zero-truncated negative binomial random intercept model)") +
              helvetica_theme
strikes_ri

#save the plot
ggsave("../plots/strikes_subject_random_intercepts.jpg", strikes_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept and create rank variable
rand_int_strikes_ordered = mod_re[order(mod_re$intercept), ]
rand_int_strikes_ordered$rank = seq.int(nrow(rand_int_strikes_ordered))

################################################################################################################################################

### SUCCESS RATE - INDIVIDUAL VARIATION ###

library(ordinal)

#function for "not in"
'%!in%' = function(x,y)!('%in%'(x,y))

#load and subset the data
sr_data = read.csv('../data/cleaned_efficiency_data.csv')
sr_data = subset(sr_data,
                 sr_data$nut_species != "Coula nut" &
                 sr_data$bout_outcome != "None" &
                 sr_data$learner %!in% c("Clinging","Clinging and Peering"))

#make individual and sex a factor
sr_data$sex = as.factor(sr_data$sex)
sr_data$subject = as.factor(sr_data$subject)

### ### ###

#order the outcome variable
sr_data$bout_outcome_ordered = ordered(sr_data$bout_outcome, c("Failed", "Smash", "Successful"))

#fit the model (model may not have converged; potentially a false negative)
clmm1 = clmm2(bout_outcome_ordered ~ age + sex, random =subject, data = sr_data, Hess = TRUE, nAGQ = 7)
summary(clmm1)

#the `age` predictor was the problem; this model converges - use this model
clmm1_simp = clmm2(bout_outcome_ordered ~ sex, random = subject, data = sr_data, Hess = TRUE, nAGQ = 7)
summary(clmm1_simp)
df.residual(clmm1_simp)

#get the confidence interval for this model
clmm1_simp_sum = summary(clmm1_simp)
upper = clmm1_simp_sum$coefficients[3] + (1.96 * clmm1_simp_sum$coefficients[6])
lower = clmm1_simp_sum$coefficients[3] - (1.96 * clmm1_simp_sum$coefficients[6])
upper
lower

#get the odds ratio for the `sex` and `age` variable
exp(coef(clmm1_simp)[4])
exp(coef(clmm1_simp)[3])

#test whether the `Subject` term is significant (compare models with and without a random effect of `Subject`)
clmm1_simp_lm = clm2(bout_outcome_ordered ~ sex, data = sr_data)
summary(clmm1_simp_lm)
anova(clmm1_simp, clmm1_simp_lm)
tab_model(clmm1_simp_lm, clmm1_simp)

### ### ###

#choose model from which to extract random effects (the publication should use the model that is not ill-defined)
mod = clmm1_simp

#get random effects and confidence intervals
mod_re = as.data.frame(mod$ranef)
names(mod_re) = "intercept"
mod_re[, c("lower_95", "upper_95")] = mod$ranef + qnorm(0.975) * sqrt(mod$condVar) %o% c(-1, 1)

#add the `Subject` variable and order it according to intercept size
mod_re$subject = unique(sr_data$subject)
mod_re$subject = factor(mod_re$subject, levels = mod_re$subject[order(mod_re$intercept)])

#add sex to the random intercept data frame and set as factors
mod_re$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
mod_re$sex = as.factor(mod_re$sex)

#plot the data
success_ri = ggplot(mod_re, aes(intercept, subject, color=sex)) + 
              geom_point() + 
              scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
              geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
              scale_x_continuous(breaks = seq(-1.5, 1, .5), limits = c(-1.5,1)) +            
              geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
              ylab("Individual") +
              xlab("Random intercept") +
              ggtitle("Success rate\n(cummulative link random intercept model)") +
              helvetica_theme
success_ri

#save the plot
ggsave("../plots/success_subject_random_intercepts.jpg", success_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept and create rank variable
rand_int_success_ordered = mod_re[order(mod_re$intercept, decreasing = TRUE), ]
rand_int_success_ordered$rank = seq.int(nrow(rand_int_success_ordered))

################################################################################################################################################

### DISPLACEMENT RATE - INDIVIDUAL VARIATION ###

#load and subset the data
rate_data = read.csv('../data/cleaned_efficiency_data.csv')
rate_data = subset(rate_data, rate_data$nut_species != "Coula nut" &
                              rate_data$bout_outcome != "None" &
                              rate_data$learner %!in% c("Clinging","Clinging and Peering"))

#look at the data distribution by individual
displacement_individual = ggplot(rate_data, aes(displacement_count)) + 
                            geom_histogram(binwidth = 0.5, fill = "#6baed6") + 
                            facet_wrap(~subject) +
                            scale_x_continuous(breaks = seq(0, 10, 2)) +
                            ylab("Frequency") +
                            xlab("Number of displacements per bout") +
                            helvetica_theme
displacement_individual

#fit models
dr_mod_tmb_simp = glmmTMB(displacement_count ~ 1 + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_tmb_simp)

dr_mod_tmb = glmmTMB(displacement_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_tmb)

dr_mod_lm = glmmTMB(displacement_count ~ 1 + age + sex, data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_lm)

anova(dr_mod_tmb, dr_mod_lm)
tab_model(dr_mod_lm, dr_mod_tmb)

### ### ###

#choose model from which to extract random effects (the publication should use the model that is not ill-defined)
mod = dr_mod_tmb

#get random effects and confidence intervals
mod_re = as.data.frame(lme4::ranef(mod, condVar = TRUE))
mod_re$intercept = mod_re$condval
mod_re$lower_95 = mod_re$intercept - (qnorm(0.975) * mod_re$condsd)
mod_re$upper_95 = mod_re$intercept + (qnorm(0.975) * mod_re$condsd)

#add the `subject` variable and order it according to intercept size
mod_re$subject = mod_re$grp
mod_re$subject = factor(mod_re$subject, levels = mod_re$subject[order(mod_re$intercept)])

#add sex to the random intercept data frame and set as factors
mod_re$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
mod_re$sex = as.factor(mod_re$sex)

#plot the data
displacement_ri = ggplot(mod_re, aes(intercept, subject, color=sex)) + 
                    geom_point() + 
                    scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
                    geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
                    geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
                    scale_x_continuous(breaks = seq(-1.5, 1.5, .5), limits = c(-1.5,1.5)) +
                    ylab("Individual") +
                    xlab("Random intercept") +
                    ggtitle("Displacement rate\n(zero-inflated negative binomial random intercept model)") +
                    helvetica_theme
displacement_ri

#save the plot
ggsave("../plots/displacements_subject_random_intercepts.jpg", displacement_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept and create rank variable
rand_int_displacement_ordered = mod_re[order(mod_re$intercept), ]
rand_int_displacement_ordered$rank = seq.int(nrow(rand_int_displacement_ordered))

################################################################################################################################################

### TOOL SWITCH RATE - INDIVIDUAL VARIATION ###

#look at the data distribution by individual
tool_switch_rate = ggplot(rate_data, aes(tool_switch_count)) + 
                    geom_histogram(binwidth = 0.5, fill = "#6baed6") + 
                    facet_wrap(~subject) +
                    ylab("Frequency") +
                    xlab("Number of tool switches per bout") +
                    ggtitle("Histogram of Tool Switch Frequency by Individual") +
                    helvetica_theme

tool_switch_rate

#fit models
ts_mod_tmb_simp = glmmTMB(tool_switch_count ~ 1 + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(ts_mod_tmb_simp)

ts_mod_tmb = glmmTMB(tool_switch_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(ts_mod_tmb)

ts_mod_tmb_lm = glmmTMB(tool_switch_count ~ 1 + age + sex, data = rate_data, family = nbinom1, zi = ~ 1) 
summary(ts_mod_tmb_lm)

#compare the models
anova(ts_mod_tmb, ts_mod_tmb_lm)
tab_model(ts_mod_tmb_lm, ts_mod_tmb)

### ### ###

#choose model from which to extract random effects (the publication should use the model that is not ill-defined)
mod = ts_mod_tmb

#get random effects and confidence intervals
mod_re = as.data.frame(lme4::ranef(mod, condVar = TRUE))
mod_re$intercept = mod_re$condval
mod_re$lower_95 = mod_re$intercept - (qnorm(0.975) * mod_re$condsd)
mod_re$upper_95 = mod_re$intercept + (qnorm(0.975) * mod_re$condsd)

#add the `Subject` variable and order it according to intercept size
mod_re$subject = mod_re$grp
mod_re$subject = factor(mod_re$subject, levels = mod_re$subject[order(mod_re$intercept)])

#add sex to the random intercept data frame and set as factors
mod_re$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
mod_re$sex = as.factor(mod_re$sex)

#plot the data
tool_swtich_ri = ggplot(mod_re, aes(intercept, subject, color=sex)) + 
                    geom_point() + 
                    scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
                    geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
                    geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
                    scale_x_continuous(breaks = seq(-0.75, 0.75, .25), limits = c(-0.75, 0.75)) +
                    ylab("Individual") +
                    xlab("Random intercept") +
                    ggtitle("Tool switch rate\n(zero-inflated negative binomial random intercept model)") +
                    helvetica_theme
tool_swtich_ri

#save the plot
ggsave("../plots/tool_switch_subject_random_intercepts.jpg", tool_swtich_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept and create rank variable
rand_int_tool_switch_ordered = mod_re[order(mod_re$intercept), ]
rand_int_tool_switch_ordered$rank = seq.int(nrow(rand_int_tool_switch_ordered))

################################################################################################################################################

### STABILITY OVER ADULT PROFICIENCY PERIOD (AGES 11-40) ###

detach(package:lmerTest)
library(nlme)
library(ggpubr)

#check the data
table(efficiency_data$subject, efficiency_data$age)

#individuals to keep in the dataset
keep = c("Fanle", "Foaf", "Jeje", "Jire", "Tua", "Velu", "Fana")

#subset the dataset and check it
around_no_failed = subset(efficiency_data, efficiency_data$subject %in% keep)
around_no_failed$subject = droplevels(around_no_failed$subject)

around_no_failed = subset(around_no_failed, around_no_failed$age >= 11)
around_no_failed = subset(around_no_failed, around_no_failed$age <= 40)

table(around_no_failed$subject, around_no_failed$age)

#get size of dataset (then by individual)
nrow(around)
nrow(around_no_failed)

table(around$subject)

### BOUT DURATION ###

#run the model with the `age` variable (model fails to converge)
mod_sub_age_slopes = lmer(log(bout_duration) ~ 1 + age + sex + (1 + age | subject), data = around_no_failed)
summary(mod_sub_age_slopes)

### STRIKES PER NUT ###

#fit truncated negative binomial model with random slope
spn_mod_tmb_nb_slopes = glmmTMB(strike_count ~ 1 + age + sex + (1 + age | subject), data = around_no_failed, family = truncated_nbinom2(link="log")) 
summary(spn_mod_tmb_nb_slopes)

#extract the data to plot
strike_slope_list = sjPlot::plot_model(spn_mod_tmb_nb_slopes, type = "pred", terms = c("age [all]","subject"), pred.type = "re", se = TRUE)
strike_slope_data = as.data.frame(strike_slope_list["data"]$data)

#drop missing data for individuals
strike_slope_data_cleaned = rbind(subset(strike_slope_data, group == "Fanle" & x <= 20), 
                                  subset(strike_slope_data, group == "Foaf" & x <= 37), 
                                  subset(strike_slope_data, group == "Jeje" & x <= 20), 
                                  subset(strike_slope_data, group == "Jire" & x >= 34), 
                                  subset(strike_slope_data, group == "Tua" & x >= 35), 
                                  subset(strike_slope_data, group == "Velu" & x >= 35),
                                  subset(strike_slope_data, group == "Fana" & x >= 38))

#plot the raw data and random slopes
plot_strikes_sub_age = ggplot(around_no_failed, aes(x = age, y = strike_count, color = subject)) + 
                        geom_point(position = position_dodge(0.5), alpha = 0.5) + 
                        geom_line(data = strike_slope_data_cleaned, aes(x = x, y = predicted, color = group), size = 0.75) + 
                        geom_ribbon(data = strike_slope_data_cleaned, aes(x = x, y = predicted, ymin = (predicted - std.error), ymax = (predicted + std.error), 
                                                                          color = group), alpha=0.1, linetype = 0) +
                        scale_color_manual(values = c("#465177", "#E4C22B", "#965127", "#29483A", "#9FB6DA", "#759C44", "#DF3383"), name = "Individual" )+
                        scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) +
                        scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) +
                        xlab("Age (years)") + 
                        ylab("Strikes per nut") +
                        coord_cartesian(ylim = c(0.9, 20)) + 
                        helvetica_theme

#save the plot
ggsave("../plots/strikes_per_nut_random_slopes_age.jpg", plot_strikes_sub_age, height = 9, width = 16)

### SUCCESS RATE ###

#individuals to keep in the dataset
keep = c("Fanle", "Foaf", "Jeje", "Jire", "Tua", "Velu", "Fana")

#subset the dataset and check it
around = subset(rate_data, rate_data$subject %in% keep)
around$subject = droplevels(around$subject)

around = subset(around, around$age >= 11)
around = subset(around, around$age <= 40)

table(around$subject, around$age)

#just look at successful (or not instances)
around$bout_outcome_numeric = ifelse(around$bout_outcome == "Successful", 1, 0)

#fit model with random slope (model does not provide random slope standard errors when the `sex` variable is included; model is singular)
glmer_simp_slopes = glmer(bout_outcome_numeric ~ 1 + age + (1 + age | subject), data = around, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(glmer_simp_slopes)

### DISPLACEMENT RATE ###

#fit model with random slope
dr_mod_tmb_slope = glmmTMB(displacement_count ~ 1 + age + sex + (1 + age | subject), data = around, family = nbinom1) 
summary(dr_mod_tmb_slope)

#extract the data to plot
displacement_slope_list = sjPlot::plot_model(dr_mod_tmb_slope, type = "pred", terms = c("age [all]","subject"), pred.type = "re", se = TRUE)
displacement_slope_data = as.data.frame(displacement_slope_list["data"]$data)

#drop missing data for individuals
displacement_slope_data_cleaned = rbind(subset(displacement_slope_data, group == "Fanle" & x <= 20), 
                                        subset(displacement_slope_data, group == "Foaf" & x <= 37), 
                                        subset(displacement_slope_data, group == "Jeje" & x <= 20), 
                                        subset(displacement_slope_data, group == "Jire" & x >= 34), 
                                        subset(displacement_slope_data, group == "Tua" & x >= 35), 
                                        subset(displacement_slope_data, group == "Velu" & x >= 35),
                                        subset(displacement_slope_data, group == "Fana" & x >= 38))

#plot the raw data and random slopes
plot_displacement_sub_age = ggplot(around, aes(x = age, y = displacement_count, color = subject)) + 
                              geom_point(position = position_dodge(0.5), alpha = 0.5) + 
                              geom_line(data = displacement_slope_data_cleaned, aes(x = x, y = predicted, color = group), size = 0.75) + 
                              geom_ribbon(data = displacement_slope_data_cleaned, aes(x = x, y = predicted, ymin = (predicted - std.error), ymax = (predicted + std.error), 
                                                                                color = group), alpha=0.1, linetype = 0) + 
                              scale_color_manual(values = c("#465177", "#E4C22B", "#965127", "#29483A", "#9FB6DA", "#759C44", "#DF3383"), name = "Individual") +
                              scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) + 
                              scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) + 
                              xlab("Age (years)") + 
                              ylab("Displacements per bout") + 
                              coord_cartesian(ylim = c(0.2, 6)) + 
                              helvetica_theme

#save the plot
ggsave("../plots/displacement_random_slopes_age.jpg", plot_displacement_sub_age, height = 9, width = 16)

################################################################################################################################################

### RANK STABILITY AND RELIABILITY ###

library(dplyr)
library(reshape)
library(irr)
library(corrplot)
library(ggcorrplot)

#combine data frames (first create outcome column)
rand_int_displacement_ordered$outcome = "Displacement rate"
rand_int_duration_ordered$outcome = "Bout duration"
rand_int_tool_switch_ordered$outcome = "Tool switch rate"
rand_int_strikes_ordered$outcome = "Strikes per nut"
rand_int_success_ordered$outcome = "Success rate"

all_ranks = do.call("rbind", list(rand_int_displacement_ordered[, c("subject", "rank", "outcome")],
                                  rand_int_duration_ordered[, c("subject", "rank", "outcome")],
                                  rand_int_tool_switch_ordered[, c("subject", "rank", "outcome")],
                                  rand_int_strikes_ordered[, c("subject", "rank", "outcome")],
                                  rand_int_success_ordered[, c("subject", "rank", "outcome")]))

#add mean rank by individual
all_ranks = all_ranks %>% group_by(subject) %>% dplyr::mutate(mean_rank = mean(rank))

#find duplicate ranks
rank_count_dat = data.frame()
for(i in unique(all_ranks$subject)){
  
  #subset the data
  i_sub = subset(all_ranks, all_ranks$subject == i)
  
  #get counts for each rank
  i_rank_dat = i_sub %>% group_by(rank) %>% dplyr::mutate(count = n())
  
  #add to the dataframe
  rank_count_dat = rbind(rank_count_dat, i_rank_dat)

}

#subset of ranks with one and more than one instance per individual (for plotting, add NA to ranks that exist more than once per individual)
one_ranks = rank_count_dat
one_ranks$rank = ifelse(one_ranks$count == 1, one_ranks$rank, NA)
multiple_ranks = subset(rank_count_dat, rank_count_dat$count != 1)

#order subject list by mean rank
subjects_ranked = all_ranks %>% group_by(subject) %>% dplyr::summarise(mean_rank = mean(mean_rank))
subjects_ranked = subjects_ranked[order(subjects_ranked$mean_rank), ]
all_ranks$subject = factor(all_ranks$subject, levels = as.character(subjects_ranked$subject))

#change order of factors
one_ranks$subject = factor(one_ranks$subject, levels = as.character(subjects_ranked$subject))
multiple_ranks$subject = factor(multiple_ranks$subject, levels = as.character(subjects_ranked$subject))

#plot the ranks by outcome
set.seed(5)
rank_plot = ggplot(one_ranks, aes(x = rank, y = subject, color = outcome)) +
              geom_point() +
              geom_jitter(width = 0, height = 0.3, alpha = 0.5, data = multiple_ranks) +
              scale_x_continuous(limits = c(1, 21), breaks = c(1, 5, 10, 15, 20) ) +
              scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "black"), name = "Measure") +
              ylab("Individual") +
              xlab("Rank") +
              labs(colour = "Outcome") +
              helvetica_theme 
rank_plot

#save the plot
ggsave("../plots/rank_by_outcome_and_subject.jpg", rank_plot, height = 10, width = 7.5)

### ### ###

#make the data wide
ranks_wide = cast(all_ranks, subject ~ outcome, value = "rank")
names(ranks_wide) = c("subject", "bout_duration", "displacement_count", "strike_count", "success_rate", "tool_switch_count")

#calculate the inter class correlation coefficient
icc_results = icc(ranks_wide[, c("bout_duration", "displacement_count", "strike_count", "success_rate", "tool_switch_count")], model = "twoway", type = "agreement", unit = "single")
icc_results

#look at correlation matrix
data_cor = ranks_wide[, c("bout_duration", "strike_count", "success_rate", "displacement_count", "tool_switch_count")]
M1 = cor(data_cor)[1:5, 1:5]
colnames(M1) <- c("Bout duration", "Strikes/nut",  "Success rate", "Displacement\nrate", "Tool switch\nrate")
rownames(M1) <- c("Bout duration", "Strikes/nut",  "Success rate", "Displacement\nrate", "Tool switch\nrate")

#plot and save the correlation matrix
correlation_plot = ggcorrplot(M1, hc.order = FALSE, type = "upper", outline.col = "white", lab = TRUE, colors = c("#0072B2", "white", "#D55E00"),
                     legend.title = expression(paste("Correlation (", italic("r"), ")"))) + 
                     xlab("") +
                     ylab("") +
                     helvetica_theme
correlation_plot

#save the plot
ggsave("../plots/correlation_matrix.jpg", correlation_plot, height = 7.5, width = 7.5, bg="white")

#re-calculate the inter class correlation coefficient, removing tool switch count
icc_results_new = icc(ranks_wide[, c("bout_duration", "displacement_count", "success_rate", "strike_count")], model = "twoway", type = "agreement", unit = "single")
icc_results_new

#get the mean of all the correlations
mean(c(M1[ 2:5, 1], M1[ 3:5, 2], M1[ 4:5, 3], M1[5, 4]))

#get the mean of all the correlations excluding tool switch rate
mean(c(M1[ 2:4, 1], M1[ 3:4, 2], M1[ 4, 3]))

#get tool switch rate correlation with the other variables
mean(M1[1:4, 5])

### ### ###

#create rank plot removing tool switch count
one_ranks_eff = subset(one_ranks, one_ranks$outcome != "Tool switch rate")
multiple_ranks_eff = subset(multiple_ranks, multiple_ranks$outcome != "Tool switch rate")

set.seed(5)
rank_plot_eff = ggplot(one_ranks_eff, aes(x = rank, y = subject, color = outcome)) +
                  geom_point() +
                  geom_jitter(width = 0, height = 0.3, alpha = 0.75, data = multiple_ranks_eff) +
                  scale_x_continuous(limits = c(1, 21), breaks = c(1, 5, 10, 15, 20) ) +
                  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"), name = "Measure") +
                  ylab("Individual") +
                  xlab("Rank") +
                  labs(colour = "Outcome") + 
                  theme(legend.position = "top", legend.spacing.x = unit(.1, 'mm')) + 
                  guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
                  nature_human_behavior 


rank_plot_eff

#save the plot
ggsave("../plots/efficiency_rank_plot.jpg", rank_plot_eff, height = 10, width = 7.5)
ggsave("../plots/efficiency_rank_plot.pdf", rank_plot_eff, width = 180, height = 120, units = "mm")

################################################################################################################################################

### COMBINED FIGURE FOR PUBLICATION ###

#make plots for combined figure
plot_strikes_sub_age_both = ggplot(around_no_failed, aes(x = age, y = strike_count, color = subject)) + 
                              geom_point(position = position_dodge(0.5), alpha = 0.5) + 
                              geom_line(data = strike_slope_data_cleaned, aes(x = x, y = predicted, color = group), size = 0.75) + 
                              geom_ribbon(data = strike_slope_data_cleaned, aes(x = x, y = predicted, ymin = (predicted - std.error), ymax = (predicted + std.error), 
                                                                                color = group), alpha=0.1, linetype = 0) +
                              scale_color_manual(values = c("#465177", "#E4C22B", "#965127", "#29483A", "#9FB6DA", "#759C44", "#DF3383"), name = "Individual" )+
                              scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) +
                              scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) +
                              xlab("Age (years)") + 
                              ylab("Strikes per nut") +
                              coord_cartesian(ylim = c(0.9, 20)) + 
                              theme(legend.position = "none") +
                              nature_human_behavior


plot_displacement_sub_age_both = ggplot(around, aes(x = age, y = displacement_count, color = subject)) + 
                                  geom_point(position = position_dodge(0.5), alpha = 0.5) + 
                                  geom_line(data = displacement_slope_data_cleaned, aes(x = x, y = predicted, color = group), size = 0.75) + 
                                  geom_ribbon(data = displacement_slope_data_cleaned, aes(x = x, y = predicted, ymin = (predicted - std.error), ymax = (predicted + std.error), 
                                                                                          color = group), alpha=0.1, linetype = 0) + 
                                  scale_color_manual(values = c("#465177", "#E4C22B", "#965127", "#29483A", "#9FB6DA", "#759C44", "#DF3383"), name = "Individual") +
                                  scale_x_continuous(limits = c(9, 41), breaks = c(10, 15, 20, 25, 30, 35, 40)) + 
                                  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) + 
                                  xlab("Age (years)") + 
                                  ylab("Displacements per bout") + 
                                  guides(colour = guide_legend(nrow = 1)) +
                                  theme(legend.position = "none") +
                                  coord_cartesian(ylim = c(0.2, 6)) + 
                                  nature_human_behavior

#combine the plots
strikes_plus_displacement = ggarrange(plot_strikes_sub_age_both, plot_displacement_sub_age_both, 
                                      common.legend = TRUE, legend = "right", labels = c("a", "b"), font.label=list(color="black",size=7))
strikes_plus_displacement

#save the plot
ggsave("../plots/figure4.pdf", strikes_plus_displacement, width = 180, height = 120, units = "mm")
ggsave("../plots/figure4.jpg", strikes_plus_displacement, width = 180, height = 120, units = "mm")

################################################################################################################################################

### ASSUMPTION CHECKS ###

library(car)
library(rcompanion)
library(influence.ME)
library(sure)

#re-estimate models
bout_d = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data)
spn = glmmTMB(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data, family = truncated_nbinom2(link="log"))
dr = glmmTMB(displacement_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
ts = glmmTMB(tool_switch_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
sr = clmm2(bout_outcome_ordered ~ sex, random = subject, data = sr_data, Hess = TRUE, nAGQ = 7)

#check multicollinearity for each model (not necessary for success rate as there is only one predictor variable)
vif(bout_d)

### RESIDUAL NORMALITY ##

#bout duration
plotNormalHistogram(residuals(bout_d), xlab = "Log bout duration residuals") 
qqnorm(residuals(bout_d), ylab="Sample quantiles for residuals")
qqline(residuals(bout_d), col="blue")

### HETEROSCEDASTICITY ###

#bout duration
plot(fitted(bout_d), resid(bout_d)^2,
     ylab = "Squared log bout duration residuals",
     xlab = "Fitted log bout duration values")

#plot assumption checks as 2 by 2
bout_d_assumtpion_checks = par(mfrow=c(2,2))
                              plotNormalHistogram(residuals(bout_d),
                                                  xlab = "Log bout duration residuals") #plot 1
                              qqnorm(residuals(bout_d),
                                     ylab= "Sample quantiles for residuals")
                              qqline(residuals(bout_d), 
                                     col="blue") #plot 2
                              plot(fitted(bout_d), resid(bout_d)^2,
                                   ylab = "Squared log bout duration residuals",
                                   xlab = "Fitted log bout duration values") #plot 3
                              qqnorm(rand_ints$intercept) #plot 4

### RANDOM INTERCEPT NORMALITY ###

#random intercept normality for bout duration
model_re_tmp = lme4::ranef(bout_d, condVar = TRUE)
rand_ints = model_re_tmp$subject
colnames(rand_ints)[1] = "intercept"
qqnorm(rand_ints$intercept)
qqline(rand_ints$intercept, col="blue")
shapiro.test(rand_ints$intercept)

#random intercept normality for strikes per nut
qqnorm(rand_int_strikes_ordered$intercept)
qqline(rand_int_strikes_ordered$intercept)
shapiro.test(rand_int_strikes_ordered$intercept)

#random intercept normality for displacement rate
qqnorm(rand_int_displacement_ordered$intercept)
qqline(rand_int_displacement_ordered$intercept)
shapiro.test(rand_int_displacement_ordered$intercept)

#random intercept normality for tool switch rate
qqnorm(rand_int_tool_switch_ordered$intercept)
qqline(rand_int_tool_switch_ordered$intercept)
shapiro.test(rand_int_tool_switch_ordered$intercept)

#random intercept normality for success rate
qqnorm(rand_int_success_ordered$intercept)
qqline(rand_int_success_ordered$intercept)
shapiro.test(rand_int_success_ordered$intercept)

### SUBJECT (LEVEL-TWO) INFLUENCE ###

#get the DFBETAS values for each level-two unit (individual) for bout duration
bout_subject_influence = influence(bout_d, "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#only Yo and Fana have DFBETAS values above the cut-off
dfbetas(bout_subject_influence)

#run models with these individuals excluded and check for the same result
efficiency_data_subset = subset(efficiency_data, efficiency_data$subject %!in% c("Yo", "Fana"))
efficiency_data_subset$subject = droplevels(efficiency_data_subset$subject)

#the result is the same even when removing individuals with large influences on the model
eff_dat_sub_lmer = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data_subset)
eff_dat_sub_lm = lm(log(bout_duration) ~ 1 + age + sex, data = efficiency_data_subset)
anova(eff_dat_sub_lmer, eff_dat_sub_lm)

### ### ###

source(system.file("other_methods", "influence_mixed.R", package = "glmmTMB"))

#get influence of each level-two unit (individual) for strikes per nut
spn_influence = influence_mixed(spn, groups = "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#none of the individuals have DFBETAS score above the cut-off
dfbeta(spn_influence)

### ### ###

#get influence of each level-two unit (individual) for displacement rate
dr_influence = influence_mixed(dr, groups = "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#none of the individuals have DFBETAS score above the cut-off
dfbeta(dr_influence)

### ### ###

#get influence of each level-two unit (individual) for tool switching rate
ts_influence = influence_mixed(ts, groups = "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#none of the individuals have DFBETAS score above the cut-off
dfbeta(ts_influence)

### ### ###

#visually assess presence of influential cases using surrogate residuals (see Liu and Zhang 2018, doi: 10.1080/01621459.2017.1292915)
sr_lm = clm(bout_outcome_ordered ~ sex, data = sr_data)
surrogate(sr_lm,method=c("latent"), nsim=1L)

set.seed(1225) #for reproducibility 
grid.arrange(autoplot.clm(sr_lm, nsim=10,what="qq"), #qq plot
             autoplot.clm(sr_lm, nsim=10,what="fitted",alpha=0.5), ncol =2 ) #residual-vs-fitted value plot
