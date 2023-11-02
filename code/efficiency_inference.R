"
Authors: Sophie Berdugo & Arran J. Davis
Emails: sophie.berdugo@keble.ox.ac.uk | arran.davis@anthro.ox.ac.uk | davis.arran@gmail.com
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
efficiency_data = read.csv('../cleaned_data/cleaned_efficiency_data.csv')
efficiency_data = subset(efficiency_data, efficiency_data$bout_outcome != "Failed" & 
                         efficiency_data$nut_species != "Coula nut" &
                         efficiency_data$bout_outcome != "None" &
                         efficiency_data$learner %!in% c("Clinging","Clinging and Peering"))

#make individual and sex a factor
efficiency_data$sex = as.factor(efficiency_data$sex)
efficiency_data$subject = as.factor(efficiency_data$subject)

################################################################################################################################################

### GRAPH THEMES ###

#fonts
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", "Avenir Black Oblique"))

#theme
header_size = 10
axis_size = 10

#theme for plots
avenir_theme = theme(text=element_text(size=header_size,family='avenir'),
                axis.text.x = element_text(color = 'black', size = axis_size, vjust = 1),
                axis.text.y = element_text(color = 'black', size = axis_size),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
                axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), face = "bold"),
                panel.background = element_blank(),
                panel.grid.major.x = element_line(color = '#e7e7e7'),
                panel.grid.major.y = element_line(color = '#e7e7e7'),
                legend.key = element_blank(),
                plot.title = element_text(hjust = 0.5, face = "bold"))

#theme for plots
helvetica_theme = theme(text=element_text(size = 7,family='Helvetica'),
                     axis.text.x = element_text(color = 'black', size = 7, vjust = 1),
                     axis.text.y = element_text(color = 'black', size = 7),
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
                     axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), face = "bold"),
                     panel.background = element_blank(),
                     panel.grid.major.x = element_line(color = '#e7e7e7'),
                     panel.grid.major.y = element_line(color = '#e7e7e7'),
                     legend.key = element_blank(),
                     plot.title = element_text(hjust = 0.5, face = "bold"))

#set the font (chose one)
#par(family = 'avenir')
par(family = 'helvetica')

################################################################################################################################################

### BOUT DURATION ###

#fit models
mod1 = lmer(log(bout_duration) ~ 1 + (1 | subject), data = efficiency_data)
summary(mod1)

mod2 = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data)
summary(mod2)

#test whether the `Subject` term is significant (compare models with and without a random effect of `Subject`; it is highly significant)
mod3 = lm(log(bout_duration) ~ 1 + age + sex, data = efficiency_data)
anova(mod2, mod3)

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
                    ggtitle("Log bout duration (linear random intercept model)") +
                    helvetica_theme

bout_duration_ri

#save the plot (as a JPG and PDF)
ggsave("../plots/bout_duration_subject_random_intercepts.jpg", bout_duration_ri, height = 170, width = 90, units = "mm", dpi = 600)
ggsave("../plots/bout_duration_subject_random_intercepts.pdf", bout_duration_ri, height = 170, width = 90, units = "mm", dpi = 600)

### ### ###

#sort data frame by intercept
rand_int_duration_ordered = rand_ints[order(rand_ints$intercept), ]
rand_int_duration_ordered$rank = seq.int(nrow(rand_int_duration_ordered))

################################################################################################################################################

### STRIKE PER NUT ###

### ### ###

#fit models
spn_mod_simp = vglm(strike_count ~ 1 + (1 | subject), data = efficiency_data, family = pospoisson())
summary(spn_mod_simp)

spn_mod = vglm(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data, family = pospoisson())
summary(spn_mod)

spn_mod_lm_vglm = vglm(strike_count ~ 1 + age + sex, data = efficiency_data, family = pospoisson())
summary(spn_mod_lm_vglm)

spn_mod_lm = glm(strike_count ~ 1 + age + sex, data = efficiency_data, family = poisson(link = "log"))
summary(spn_mod_lm)

anova(spn_mod_lm, spn_mod_lm_vglm)

### HERE HERE HERE - FIGURE OUT HOW TO MODEL THESE ###



















spn_mod_simp = glmer(strike_count ~ 1 + (1 | subject), data = efficiency_data, family = poisson(link = "log"))
summary(spn_mod_simp)

spn_mod = glmer(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data, family = poisson(link = "log"))
summary(spn_mod)

spn_mod_lm = glm(strike_count ~ 1 + age + sex, data = efficiency_data, family = poisson(link = "log"))
summary(spn_mod_lm)

### ### ###

#check for overdispersion 
check_overdispersion(spn_mod)

#use a negative binomial model because of overdispersion
spn_mod_nb = glmer.nb(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data)
summary(spn_mod_nb)

#compare the models
tab_model(spn_mod_nb, spn_mod_lm)
anova(spn_mod_nb, spn_mod_lm)

#choose model to plot
model_re = spn_mod_nb

#get random effects and their variances
model_re_tmp = lme4::ranef(model_re, condVar = TRUE)
temp_vars = as.data.frame.table((attr(lme4::ranef(model_re, condVar = TRUE)[[1]], "postVar")))

#add to a data frame
rand_ints = model_re_tmp$subject
colnames(rand_ints)[1] = "intercept"
rand_ints$variance = temp_vars$Freq

#calculate standard deviation for intercepts and 95% CI
rand_ints$error = 2*sqrt(rand_ints$variance)
rand_ints$lower_95 = rand_ints$intercept - rand_ints$error
rand_ints$upper_95 = rand_ints$intercept + rand_ints$error

#create a subject variable
rand_ints$subject = rownames(rand_ints)

#add sex to the random intercept data frame and set as factors
rand_ints$sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")
rand_ints$sex = as.factor(rand_ints$sex)

#order by size of intercept
rand_ints$subject = factor(rand_ints$subject, levels = rand_ints$subject[order(rand_ints$intercept)])

#plot the data
strikes_ri = ggplot(rand_ints, aes(intercept, subject, color=sex)) + 
              geom_point() + 
              scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
              geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
              geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
              scale_x_continuous(breaks = seq(-1, 1.5, .5), limits = c(-1,1.5)) +
              ylab("Individual") +
              xlab("Random intercept") +
              ggtitle("Strikes per nut (Poisson random intercept model)") +
              avenir_theme

strikes_ri

ggsave("../plots/strikes_subject_random_intercepts.jpg", strikes_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept
rand_int_strikes_ordered = rand_ints[order(rand_ints$intercept), ]
rand_int_strikes_ordered$rank = seq.int(nrow(rand_int_strikes_ordered))

################################################################################################################################################

### SUCCESS RATE ###

library(ordinal)

#function for "not in"
'%!in%' = function(x,y)!('%in%'(x,y))

#load and subset the data
sr_data = read.csv('../cleaned_data/cleaned_efficiency_data.csv')
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
clmm1 = clmm2(bout_outcome_ordered ~ age + sex, random =  subject, data = sr_data, Hess = TRUE, nAGQ = 7)
summary(clmm1)

#the `age` predictor was the problem; this model converges - use this model
clmm1_simp = clmm2(bout_outcome_ordered ~ sex, random =  subject, data = sr_data, Hess = TRUE, nAGQ = 7)
summary(clmm1_simp)

#get the odds ratio for the `sex` and `age` variable
exp(coef(clmm1)[4])
exp(coef(clmm1)[3])

#test whether the `Subject` term is significant (compare models with and without a random effect of `Subject`)
clmm1_simp_lm = clm2(bout_outcome_ordered ~ sex, data = sr_data)
summary(clmm1_simp_lm)
anova(clmm1_simp, clmm1_simp_lm)
tab_model(clmm1_simp, clmm1_simp_lm)

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
              ggtitle("Success rate (cummulative link random intercept model)") +
              avenir_theme
success_ri

ggsave("../plots/success_subject_random_intercepts.jpg", success_ri, height = 10, width = 7.5)

#sort data frame by intercept
rand_int_success_ordered = mod_re[order(mod_re$intercept, decreasing = TRUE), ]
rand_int_success_ordered$rank = seq.int(nrow(rand_int_success_ordered))

################################################################################################################################################

### DISPLACEMENT RATE ###

#load and subset the data
rate_data = read.csv('../cleaned_data/cleaned_efficiency_data.csv')
rate_data = subset(rate_data, rate_data$nut_species != "Coula nut" &
                              rate_data$bout_outcome != "None" &
                              rate_data$learner %!in% c("Clinging","Clinging and Peering"))

#look at the data distribution by individual
displacement_individual = ggplot(rate_data, aes(displacement_count)) + 
                            geom_histogram(binwidth = 0.5, fill = "#6baed6") + 
                            facet_wrap(~subject) +
                            ylab("Frequency") +
                            xlab("Number of displacements per bout") +
                            ggtitle("Histogram of Displacement Frequency by Individual") +
                            avenir_theme
displacement_individual

#fit models
dr_mod_tmb_simp = glmmTMB(displacement_count ~ 1 + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_tmb_simp)

dr_mod_tmb = glmmTMB(displacement_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_tmb)

dr_mod_lm = glmmTMB(displacement_count ~ 1 + age + sex, data = rate_data, family = nbinom1, zi = ~ 1) 
summary(dr_mod_lm)

anova(dr_mod_tmb, dr_mod_lm)

### ### ###

#choose model from which to extract random effects (the publication should use the model that is not ill-defined)
mod = dr_mod_tmb

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
displacement_ri = ggplot(mod_re, aes(intercept, subject, color=sex)) + 
                    geom_point() + 
                    scale_color_manual(values = c("#f1a340", "#998ec3"), name = "Sex") +
                    geom_errorbar(aes(xmin = lower_95, xmax = upper_95), width=.2, position=position_dodge(0.05), color = "#525252") +
                    geom_vline(xintercept = 0, size=0.3, linetype = "dashed", color = "black") +  
                    scale_x_continuous(breaks = seq(-1.5, 1.5, .5), limits = c(-1.5,1.5)) +
                    ylab("Individual") +
                    xlab("Random intercept") +
                    ggtitle("Displacement rate (zero-inflated negative binomial random intercept model)") +
                    avenir_theme
displacement_ri

ggsave("../plots/displacements_subject_random_intercepts.jpg", displacement_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept
rand_int_displacement_ordered = mod_re[order(mod_re$intercept), ]
rand_int_displacement_ordered$rank = seq.int(nrow(rand_int_displacement_ordered))

################################################################################################################################################

### TOOL SWITCH RATE ###

#look at the data distribution by individual
tool_switch_rate = ggplot(rate_data, aes(tool_switch_count)) + 
                    geom_histogram(binwidth = 0.5, fill = "#6baed6") + 
                    facet_wrap(~subject) +
                    ylab("Frequency") +
                    xlab("Number of tool switches per bout") +
                    ggtitle("Histogram of Tool Switch Frequency by Individual") +
                    avenir_theme
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
                    ggtitle("Tool switch rate (zero-inflated negative binomial random intercept model)") +
                    avenir_theme

tool_swtich_ri

ggsave("../plots/tool_switch_subject_random_intercepts.jpg", tool_swtich_ri, height = 10, width = 7.5)

### ### ###

#sort data frame by intercept
rand_int_tool_switch_ordered = mod_re[order(mod_re$intercept), ]
rand_int_tool_switch_ordered$rank = seq.int(nrow(rand_int_tool_switch_ordered))

################################################################################################################################################

### RANK STABILITY ###

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
              #scale_color_brewer(palette = "Set2") + 
              ylab("Individual") +
              xlab("Rank") +
              labs(colour = "Outcome") +
              avenir_theme 
rank_plot

ggsave("../plots/rank_by_outcome_and_subject.jpg", rank_plot, height = 10, width = 7.5)

### ### ###

#make the data wide
ranks_wide = cast(all_ranks, subject ~ outcome, value = "rank")
names(ranks_wide) = c("subject", "bout_duration", "displacement_count", "strike_count", "success_rate", "tool_switch_count")

#calculate the inter class correlation coefficient
icc_results = icc(ranks_wide[, c("bout_duration", "displacement_count", "strike_count", "success_rate", "tool_switch_count")], model = "twoway", type = "consistency", unit = "single")
icc_results

#look at correlation matrix
data_cor = ranks_wide[, c("bout_duration", "strike_count", "success_rate", "displacement_count", "tool_switch_count")]
M1 = cor(data_cor)[1:5, 1:5]
colnames(M1) <- c("Bout duration", "Strikes/nut",  "Success rate", "Displacement\nrate", "Tool switch\nrate")
rownames(M1) <- c("Bout duration", "Strikes/nut",  "Success rate", "Displacement\nrate", "Tool switch\nrate")

#plot and save the correlation matrix
correlation_plot = ggcorrplot(M1, hc.order = FALSE, type = "upper", outline.col = "white", lab = TRUE, colors = c("#0072B2", "white", "#D55E00"),
                     legend.title = expression(paste("Correlation (", italic("r"), ")")),
                     title = "Correlation of rankings for all pairs of nut cracking efficiency measures") + 
                     theme(text=element_text(size=header_size,family='avenir'),
                           axis.text.x = element_text(color = 'black', size = axis_size, vjust = 1),
                           axis.text.y = element_text(color = 'black', size = axis_size),
                           panel.background = element_blank(),
                           panel.grid.major.x = element_line(color = '#e7e7e7'),
                           panel.grid.major.y = element_line(color = '#e7e7e7'),
                           legend.key = element_blank(),
                           plot.title = element_text(hjust = 0.5, face = "bold"))
correlation_plot

ggsave("../plots/correlation_matrix.jpg", correlation_plot, height = 7.5, width = 7.5, bg="white")

#re-calculate the interclass correlation coefficient, removing tool switch count
icc_results_new = icc(ranks_wide[, c("bout_duration", "displacement_count", "success_rate", "strike_count")], model = "twoway", type = "consistency", unit = "single")
icc_results_new

### ### ###

#create rank plot removing tool switch count
one_ranks_eff = subset(one_ranks, one_ranks$outcome != "Tool switch rate")
multiple_ranks_eff = subset(multiple_ranks, multiple_ranks$outcome != "Tool switch rate")

set.seed(5)
rank_plot_eff = ggplot(one_ranks_eff, aes(x = rank, y = subject, color = outcome)) +
                  geom_point() +
                  geom_jitter(width = 0, height = 0.3, alpha = 0.5, data = multiple_ranks_eff) +
                  scale_x_continuous(limits = c(1, 21), breaks = c(1, 5, 10, 15, 20) ) +
                  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"), name = "Measure") +
                  ylab("Individual") +
                  xlab("Rank") +
                  labs(colour = "Outcome") +
                  avenir_theme 
                  rank_plot

ggsave("../plots/efficiency_rank_plot.jpg", rank_plot_eff, height = 10, width = 7.5)

################################################################################################################################################

### ASSUMPTION CHECKS ###

library(car)
library(rcompanion)
library(influence.ME)

#re-estimate models
bout_d = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data)
spn = glmer.nb(strike_count ~ 1 + age + sex + (1 | subject), data = efficiency_data)
dr = glmmTMB(displacement_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
ts = glmmTMB(tool_switch_count ~ 1 + age + sex + (1 | subject), data = rate_data, family = nbinom1, zi = ~ 1) 
sr = clmm2(bout_outcome_ordered ~ sex, random =  subject, data = sr_data, Hess = TRUE, nAGQ = 7)

#check multicollinearity for each model (not necessary for success rate as there is only one predictor variable)
vif(bout_d)
vif(spn)
vif(dr)
vif(ts)

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

#the result is the same even when remove individuals with large influences on the model
eff_dat_sub_lmer = lmer(log(bout_duration) ~ 1 + age + sex + (1 | subject), data = efficiency_data_subset)
eff_dat_sub_lm = lm(log(bout_duration) ~ 1 + age + sex, data = efficiency_data_subset)
anova(eff_dat_sub_lmer, eff_dat_sub_lm)

### ### ###

source(system.file("other_methods", "influence_mixed.R", package = "glmmTMB"))

#get influence of each level-two unit (individual) for displacement rate
dr_influence = influence_mixed(dr, groups = "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#none of the individuals have DFBETAS score above the cut-off
dfbeta(dr_influence)

### ### ###

#get influence of each level-two unit (individual) for displacement rate
ts_influence = influence_mixed(ts, groups = "subject")

#get the DFBETAS cut-off according to Nieuwenhuis et al. (2012)
print(paste("Cut-off: ", 2 / sqrt(21)))

#none of the individuals have DFBETAS score above the cut-off
dfbeta(ts_influence)











#plot assumption checks as 2 by 2
bout_d_assumtpion_checks <- par(mfrow=c(2,2))
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

#save plots
bout_d_residual_plot = plotNormalHistogram(residuals(bout_d),
                                            xlab = "Log bout duration residuals") 
plot(bout_d_residual_plot)
ggsave(file="../plots/bout_d_residual_plot.jpg", bout_d_residual_plot, width = 850)
dev.off()
