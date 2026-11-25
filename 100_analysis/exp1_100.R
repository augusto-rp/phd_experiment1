
# Dependencies ------------------------------------------------------------

library(llmcoder)
library(psych)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(misty)
library(effectsize)
library(patchwork)
library(lme4)
library(lmerTest)
library(corrr)
library(writexl)
library(emmeans)
library(purrr)



# Data Preparation and Cleaning --------------------------------------------------------

options(scipen = 999)

data<-read_excel("100_analysis/exp1_100.xlsx") #139 unclean
data[data == -9]<-NA # transform -9 into NA



# Identify cases where CI01 != 1  No consent to use data
data %>%
  filter(!is.na(CI01), CI01 != 1)  # 3 cases

# Not finished
data %>%
  filter(!is.na(FINISHED), FINISHED != 1) # 11 cases


# Identify cases where TIME_RSI >2
data %>% filter(TIME_RSI > 2)  # 13 cases


# Failures in Attention Checks
# Code that creates a df with cases where CV01_14 != 4, or CQ24_03 != 4 or CQ12_03 != 4 .
# CV01_14 is first attention check and CQ12/24_03 is the second according to assignment
fail_checks <- data %>%
  filter(CV01_14 != 4 | CQ24_03 != 4 | CQ12_03 != 4) %>%
  mutate(
    fail_count = rowSums(cbind(
      CV01_14 != 4, #supposed to be 4
      CQ24_03 != 4, #supposed to be 4
      CQ12_03 != 4 #supposed to be 4
    ), na.rm = TRUE)
  )

# Delete all columns in fail_any_check except CASE, REF,AS02 (H/S assignment) TIME_RSI, CV01_14, CQ12_03, CQ24_03
fail_checks <- fail_checks %>% select(CASE, TIME_RSI, AS02, CV01_14, CQ12_03, CQ24_03, fail_count)


# 18 cases with RSI<2 AND THAT FAIL ONE ATTENTION CHECK

# So 3+11+13+18 a total of 45 cases are eliminated

# Filter cases
data <- data %>%
  filter(!CASE %in% c("154", "156", "157", "177", "178", "179", "185", "190", "191", "202", "203", "206", "209", "211", "215", "216", "220", "221", "223", "225", "229", "230", "231", "236", "239", "241", "244", "246", "249", "251", "255", "267", "273", "274", "275", "277", "278", "282", "283", "286", "289", "295", "300", "301", "303")) 


# Search for tther cases with suspiciously low times and add to the list above (less than 2 minutes)
describe(data$TIME_SUM) 

rm(fail_checks)

# Definition of constructs

##### Cynicism Items
cinismo_list <- c("CY02_01","CY02_02","CY02_03","CY02_04","CY02_05","CY02_06", "CY02_07","CY02_08")


##### Tolerance for ambiguity
# The MSTAT-II scale from McLain adapted into spanish by Arquero and McLain consists of 13 items that measure differnt dimentions of these construct
# In these only the items concerning Attitudes towards ambiguous stimuli in general (CV01_01, CV01_03,CV01_07, CV01_11, CV01_13) AND insoluble stimuli (CV01_02, CV01_05, CV01_09) were considered

ta_list <- c("CV01_01","CV01_02","CV01_03","CV01_05","CV01_07","CV01_09", "CV01_11","CV01_13")


##### Relevance of Topics
relevance<-c("SD03_01","SD03_02","SD03_03","SD03_04","SD03_05","SD03_06")


##### Manipulation checks (rating of funniness of images)

  # Rating of funniness for people in H condition / Rating of offensiveness
fun_h<-c("CQ01_01","CQ02_01","CQ03_01","CQ04_01","CQ05_01","CQ06_01","CQ07_01","CQ08_01","CQ09_01","CQ10_01","CQ11_01","CQ12_01")
of_h<-c("CQ01_02","CQ02_02","CQ03_02","CQ04_02","CQ05_02","CQ06_02","CQ07_02","CQ08_02","CQ09_02","CQ10_02","CQ11_02","CQ12_02")

  # Rating of funniness for people in S condition
fun_s<-c("CQ13_01","CQ14_01","CQ15_01","CQ16_01","CQ17_01","CQ18_01","CQ19_01","CQ20_01","CQ21_01","CQ22_01","CQ23_01","CQ24_01")
of_s<-c("CQ13_02","CQ14_02","CQ15_02","CQ16_02","CQ17_02","CQ18_02","CQ19_02","CQ20_02","CQ21_02","CQ22_02","CQ23_02","CQ24_02")


  # Funniness of rw memes in H condition
fun_h_rw<-c("CQ01_01","CQ03_01","CQ05_01")
  # Funnines of lw memes in H condition
fun_h_lw<-c("CQ02_01","CQ04_01","CQ06_01")


# Funniness of rw memes in S condition
fun_s_rw<-c("CQ13_01","CQ15_01","CQ17_01")
# Funnines of lw memes in S condition
fun_s_lw<-c("CQ14_01","CQ16_01","CQ18_01")


# in h from CQ01 to CQ06 are the political memes and the other not political
# in s from CQ13 to CQ18 are the political texts and the other not political


# Funniness of non political memes in H condition
fun_h_np<-c("CQ07_01","CQ08_01","CQ09_01","CQ10_01","CQ11_01","CQ12_01")

# Funniness of of non political memes in S condition
fun_s_np<-c("CQ19_01","CQ20_01","CQ21_01","CQ22_01","CQ23_01","CQ24_01")



# Creation of composed variables ------------------------------------------

# Creating OA towards lw
data <- data %>%
  mutate(ao_lw = ((AO03_01 + AO04_01) / 2) - abs(AO03_01 - AO04_01))


# Lets also check the average positive and negatives views towards the lw
describe(data$AO03_01) # mean value of positive views towards the left: 3.3
describe(data$AO04_01) # mean value of negative views towards the left: 3.68
describe(data$ao_lw) # mean oa: 0.85

  # Creating OA towards rw
data <- data %>%
  mutate(ao_rw = ((AO03_02 + AO04_02) / 2) - abs(AO03_02 - AO04_02))

describe(data$AO03_02) #mean value of positive views towards the right 2.68
describe(data$AO04_02) #mean value of negative views towards the right 4.56
describe(data$ao_rw) # mean oa: 0.44


table(data$ao_lw)
table(data$ao_rw)


# Create a correlation between ao_rw and ao_lw
cor.test(data$ao_rw, data$ao_lw, exact=FALSE,method="spearman", conf.level = 0.95) # 0.46


### AO OF TOPICS
  # Creating oa for identity register AO01 and 02_01 (AO01 is positive and A02 negative)
data <- data %>%
  mutate(ao_ir = ((AO01_01 + AO02_01) / 2) - abs(AO01_01 - AO02_01))

describe(data$AO01_01) #mean value of positive views towards creating a register of people that commit crimes. Higher is more positive
describe(data$AO02_01) #mean value of negative views towards creating a register of people that commit crimes. Higher is more negative
describe(data$ao_ir)


  # Creating oa for budget cuts AO01 and 02 _02
data <- data %>%
  mutate(ao_bc = ((AO01_02 + AO02_02) / 2) - abs(AO01_02 - AO02_02))

describe(data$AO01_02) #mean value of positive views towards budget cuts in gobverment programs
describe(data$AO02_02) #mean value of negative views towards budgets cuts in gobverment programs
describe(data$ao_bc)

  # Creating oa for security controls AO01 and 02 _03
data <- data %>%
  mutate(ao_sc = ((AO01_03 + AO02_03) / 2) - abs(AO01_03 - AO02_03))


describe(data$AO01_03) #mean value of positive views towards identity checks in the subway
describe(data$AO02_03) #mean value of negative views towards identity checks in the subway
describe(data$ao_ir)

  # Creating oa for enviroment policies  AO01 and 02 _04 (this was a topic not shown in stimuli)
data <- data %>%
  mutate(ao_ep = ((AO01_04 + AO02_04) / 2) - abs(AO01_04 - AO02_04))

describe(data$AO01_04) #mean value of positive views towards policies oriented toward environment protection
describe(data$AO02_04) #mean value of negative views towards policies oriented towards environment protection
describe(data$ao_ep)

  # Creating oa for winter  AO01 and 02 _05
data <- data %>%
  mutate(ao_w = ((AO01_05 + AO02_05) / 2) - abs(AO01_05 - AO02_05))

describe(data$AO01_05) #mean value of positive views towards the winter
describe(data$AO02_05) #mean value of negative views towards the winter
describe(data$ao_w)

  # Creating oa for audios  AO01 and 02 _06
data <- data %>%
  mutate(ao_audio = ((AO01_06 + AO02_06) / 2) - abs(AO01_06 - AO02_06))

describe(data$AO01_06) #mean value of positive views towards receiving long whatsapp audios
describe(data$AO02_06) #mean value of negative views towards receiving long whatsapp audios
describe(data$ao_audio)

  # Creating oa for cats  AO01 and 02 _07
data <- data %>%
  mutate(ao_cats = ((AO01_07 + AO02_07) / 2) - abs(AO01_07 - AO02_07))

describe(data$AO01_07) #mean value of positive views towards cat's behaviors
describe(data$AO02_07) #mean value of negative views towards cat's behaviors
describe(data$ao_cats)


# Lets check correlations between ao of political topics, and ao of non political

# Create an df that includes only the values of  ao_ir, ao_sc, ao_bc
ao_p <- data %>%
  select(ao_ir, ao_sc, ao_bc)

cor.matrix(ao_p, alpha=0.05,method="spearman", p.adj="bonferroni",print=c ("cor","p", "n")) # Overall positive but weak correlations


# Create an df that includes only the values of  ao_w, ao_cats, ao_audio
ao_np <- data %>%
  select(ao_w, ao_cats, ao_audio)


cor.matrix(ao_np, alpha=0.05,method="spearman", p.adj="bonferroni",print=c ("cor","p", "n")) # Overall positive but weak correlations



# Create an variable called ao_pol that is the average of ao_ir, ao_sc, ao_bc

# I have doubts this is a good ideas, as there seems to be weak correlations between topics of the same dimension

data <- data %>%
  mutate(ao_pol = (ao_ir + ao_sc + ao_bc) / 3)

data <- data %>%
  mutate(ao_npol = (ao_audio + ao_cats + ao_w) / 3)


cor(data$ao_pol, data$ao_npol, method = "pearson") # Correlation between dimensions is 0.56



data$AS02<-as.factor(data$AS02)

# Compare each ao_ variable between levels of AS02

data %>%
  # 1. Collapse the 9 AO columns into two columns: variable + value of AS02
  pivot_longer(
    cols = starts_with("ao_"),
    names_to = "ao_var",
    values_to = "value"
  ) %>%
  # 2. Summarise mean per (variable × group), keeping only groups 1 and 2
  filter(AS02 %in% c(1, 2)) %>%
  group_by(ao_var, AS02) %>%
  summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")%>%
  
  ggplot(aes(x = ao_var, y = mean, color = AS02,
             shape = AS02, linetype = AS02, group = AS02)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  labs(
    title = "Mean AO values by AS02 group",
    x = "AO variable",
    y = "Mean",
    color = "AS02", shape = "AS02", linetype = "AS02"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# It is extremely tempting to get excited over what appears here that apparently shows higher ao for H condition. 
# But it is important to avoid drawing conclusion out ot a heavily underpowered pilot!!!


  # Create grouping of ideological position

data <- data %>%
  mutate(
    id_pol = case_when(
      SD06 %in% c(1, 2, 3, 4) ~ 1, #leftwing
      SD06 %in% c(6, 7, 8, 9) ~ 2, #rw
      SD06 %in% c(-1, -2, 5)  ~ 3, #neither
      TRUE ~ NA_real_         # Keeps any other values or original NAs as NA
    )
  )




  # Lets create an overall value of cynicism per person

data$cinis_g <- rowMeans(data[, cinismo_list], na.rm = TRUE)

describe(data$cinis_g)



  # Tolerance for ambiguity - Greater score is greater tolerance

# Some items are reversed. Also remember scale include two different dimensions of the construct
# Ambiguous stimuli in general (CV01_01, CV01_03,CV01_07, CV01_11, CV01_13) 
    # Reversed: CV01_01, CV01_03, CV01_11
# AND insoluble stimuli (CV01_02, CV01_05, CV01_09) were considered
    # Reversed: CV01_02, CV01_05, CV01_09


# Reverse item CV01_01 the scale goes from 1 to 5

describe(data$CV01_01)#check before change
data <- data %>%
  mutate(CV01_01 = 6 - CV01_01)  # Reverse scale: max + min - original value (5 + 1 - x = 6 - x)
describe(data$CV01_01)#check after change

# Reverse item CV01_02 the scale goes from 1 to 5

describe(data$CV01_02)#check before change
data <- data %>%
  mutate(CV01_02 = 6 - CV01_02)
describe(data$CV01_02)#check after change

# Reverse item CV01_03 the scale goes from 1 to 5

describe(data$CV01_03)#check before change
data <- data %>%
  mutate(CV01_03 = 6 - CV01_03)
describe(data$CV01_03)#check after change

# Reverse item CV01_05 the scale goes from 1 to 5

describe(data$CV01_05)#check before change
data <- data %>%
  mutate(CV01_05 = 6 - CV01_05)
describe(data$CV01_05)#check after change

# Reverse item CV01_09 the scale goes from 1 to 5

describe(data$CV01_09)#check before change
data <- data %>%
  mutate(CV01_09 = 6 - CV01_09)
describe(data$CV01_09)#check after change

# Reverse item CV01_11 the scale goes from 1 to 5

describe(data$CV01_11)#check before change
data <- data %>%
  mutate(CV01_11 = 6 - CV01_11)
describe(data$CV01_11)#check after change

# Overall rating of tolerance for ambiguity
data$ta_g <- rowSums(data[, ta_list], na.rm = TRUE)

describe(data$ta_g) #maximum possible value is 40, minimum possible is 8 this is not normalized

data$scale_ta<-scale(data$ta_g)
describe(data$scale_ta)



# Descriptives  -----------------------------------------------------------

# Gender
table(data$SD01) # 1=men
round(100 * prop.table(table(data$SD01)), 2) # 32% men and #68% women

# Age
table(data$SD04_01) #more towards the higher end of the spectrum

ggplot(data, aes(x = factor(SD04_01))) +
  geom_bar() +
  labs(
    x = "AGE", 
    y = "Count",) +
  theme_minimal()

# Education
table(data$SD07) #overwhelming majority of university educated people
round(100 * prop.table(table(data$SD07)), 2)

#- 1 no answer 2.13%
#2  Educacion Básica Completa 1.06%
#3	Educación Media Incompleta 3.19%
#4	Educación Media Completa  20.21%
#5	Instituto Técnico Profesional/Centro de Educación Técnica Incompleta 7.45% 
#7	Educación Superior Universitaria Incompleta  8.51%
#6	Instituto Técnico Profesional/Centro de Educación Técnica Completa 17.02 % 
#8	Educación Superior Universitaria Completa 40.43 %


# Political Positioning
table(data$SD06)

ggplot(data, aes(x = factor(SD06))) +
  geom_bar() +
  labs(
    caption = "1-Muy de Izquierda, 9-Muy de Derecha, -1 Ninguno",
    x = "Political Positioning", 
    y = "Count",) +
  theme_minimal()

# Average political position (excluding independents)

describe(data$SD06[bd$SD06 >= 1]) #3.8 it seems more left leaning which will impact on the funniness of the rw memes as seen in the previous pilot

# Political Position Grouped

table(data$id_pol)
round(100 * prop.table(table(data$id_pol)), 2) # 28.72% lw  26.6% rw 44.68% center/not aligned 

data$id_pol<-as.factor(data$id_pol)


# Overall interest in politics  from 1-Not at all interested  to 5-Really interested
table(data$SD02)
round(100 * prop.table(table(data$SD02)), 2)
# not at all interested: 6.38% / not very interested: 10.64% / somewhat: 31.91% / really interested: 28.72% / extremely interested: 22.34%

ggplot(data, aes(x = factor(SD02))) +
  geom_bar() +
  labs(
    caption = "1-Not interested 5-Really interested",
    x = "Interest in Politics", 
    y = "Count",) +
  theme_minimal()
# People seem to be at least somewhat interested in politics.
# Worth considering if the variance of this variable allows to use it as a moderator in other analysis


# Overall perception of knowledge in politics  from 1-Nothing  to 4-A lot
table(data$CV02)
round(100 * prop.table(table(data$CV02)), 2)
# a little bi: 17.02% / some: 60,64% / a lot: 22.34%
# same consideration as before, none sasid they didnt knew anything

ggplot(data, aes(x = factor(CV02))) +
  geom_bar() +
  labs(
    caption = "1-Nothing 4-A lot",
    x = "Self perception of Political Knowledge", 
    y = "Count",) +
  theme_minimal()


# relevance of topics
relevance_table <- lapply(data[relevance], table)

  # Create 6 barplots shown together with the values on relevance_table


labels <- c(
  SD03_01 = "Development and Economic Growth",
  SD03_02 = "Environmental Protection",
  SD03_03 = "Human Rights and Equality",
  SD03_04 = "Security",
  SD03_05 = "Tech Innovation",
  SD03_06 = "Police Force Powers"
)


relevance_table %>%
  imap(\(tbl, var) {
    as.data.frame(tbl) |>
      setNames(c("value", "Freq")) |>
      mutate(variable = var)
  }) %>%
  bind_rows() %>%
  mutate(
    value    = factor(value, levels = sort(unique(value))),
    variable = factor(variable, levels = names(labels), labels = labels)
  ) %>%
  ggplot(aes(x = value, y = Freq)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Freq), vjust = -0.3, size = 2.4) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(x = "Response", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Lets see if the mean relevance for each topic varies across id_pol
relevance_pol <- data[, c(relevance, "id_pol")] 
describeBy(relevance_pol[, -ncol(relevance_pol)], group = relevance_pol$id_pol)


# Prepare data: compute means by group and topic
relevance_plot_data <- relevance_pol %>%
  pivot_longer(cols = starts_with("SD03_"), 
               names_to = "topic", values_to = "value") %>%
  group_by(id_pol, topic) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            se   = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))), 
            .groups = "drop") %>%
  mutate(
    topic_label = case_when(
      topic == "SD03_01" ~ "_01:Economic Growth",
      topic == "SD03_02" ~ "_02:Environmental Protection",
      topic == "SD03_03" ~ "_03:Human Rights & Equality",
      topic == "SD03_04" ~ "_04:Security",
      topic == "SD03_05" ~ "_05:Tech Innovation",
      topic == "SD03_06" ~ "_06:Police Powers"
    ),
    id_pol_label = factor(id_pol, 
                          levels = c(1, 2, 3),
                          labels = c("Left (n=27)", "Right (n=25)", "Center/Ind (n=42)"))
  )

# Plot using 1 SE for intervals as EXPLORATORY ->> CHANGE IN THE REAL EXPERIMENT!!
ggplot(relevance_plot_data, 
       aes(x = topic_label, y = mean, color = id_pol_label, group = id_pol)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(
    title = "Mean Topic Relevance Ratings by Ideological Group",
    subtitle = "Error bars show ±1 SE",
    x = "Topic",
    y = "Mean Relevance (1–4)",
    color = "Ideological Group",
    caption = "n per group shown in legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )


rm (relevance_table, relevance_plot_data)

# Cynicism 

cinismo <- data[, c(cinismo_list, "id_pol")]   #
describe(cinismo) #look at those SD and medians!


# Create a boxplot of cinis_g
ggplot(data, aes(y = cinis_g)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Overall Cynicism (cinis_g)",
    y = "Cynicism Score"
  ) +
  theme_minimal()


# run a correlation between all the variables in df cinismo 
cor.plot(cinismo[, !names(cinismo) %in% "id_pol"]) #this looks nice but uses pearson which is incorrect
cor.matrix(cinismo, method="spearman",alpha=0.05, p.adj="bonferroni",print=c ("cor","p", "n"))
# the correct one is using spearman


# Comparision of mean overall cynicism based on ideological group
data %>%
  group_by(id_pol) %>%
  summarise(
    n = n(),
    mean_cinis = mean(cinis_g, na.rm = TRUE),
    sd_cinis_g = sd(cinis_g, na.rm = TRUE),
    min_cinis_g = min(cinis_g, na.rm = TRUE),
    max_cinis_g = max(cinis_g, na.rm = TRUE)
  )


# Box plots per id group
data %>%
  ggplot(aes(x = id_pol, y = cinis_g)) +
  geom_boxplot()


# Dont look that much into group 3 as it includes independents, people in the center and not affiliated
# Higher cynicism among rw but huuuuuge sd. This will probably shrink with more data


# Lets see if differences between conditions (H and S) are significant
cinis_data<-subset(data, AS02 %in% c(1,2)) #remember that CY02_07 and CY02_08 come from a differente scale

aggregate(cinis_g~AS02, data=cinis_data,
          FUN = function(x) c(n =sum(!is.na(x)), mean = mean (x, na.rm =TRUE), sd= sd(x, na.rm = TRUE)))

wilcox.test(cinis_g~AS02, data= cinis_data) # no apparent effect of humor on political cynicism scale

# Let see if differences between id groups are significant

# Test using an appropiste test for independent samples and ordinal data if the values of cinis_g across the levels of id_pol are equivalent
# Kruskal-Wallis test for independent samples (ordinal data, non-parametric)
kruskal.test(cinis_g ~ id_pol, data = data)

# If significant, follow up with pairwise Wilcoxon tests (with Bonferroni correction)
pairwise.wilcox.test(data$cinis_g, data$id_pol, 
                     p.adjust.method = "bonferroni", 
                     alternative = "two.sided")

# So apparently lw are in fact less cynic that independents. Once again this is a problematic group, so the most important comparision is between lw and rw


# Tolerance for ambiguity

tolerance <- data[, c(ta_list)]   #, "id_pol"
describe(tolerance)
cor.matrix(tolerance, alpha=0.05,method="spearman", p.adj="bonferroni",print=c ("cor","p", "n"))

# Honestly this scale doesn't seem to be working 
# One of the few significant correlations are negative which makes no sense as I already transformed the inverted items
# During the testing of the questionnaire some people mentioned difficulties understanding the wording of some items.

# Lets check mean funniness of humorous stimuli vs mean funniness of serious stimuli


mean_f<-tibble(
  topic = c("sc+rw","sc+lw","bc+rw","bc+lw","ir+rw","ir+lw",
            "audio+anti","audio+pro","w+anti","w+pro","cats+anti","cats+pro"),
  meanH = map_dbl(fun_h, ~mean(data[[.x]], na.rm = TRUE)), # loops over each column name in fun_ calculates mean for each AS02=1
  meanS = map_dbl(fun_s, ~mean(data[[.x]], na.rm = TRUE)), # loops over each column name in fun_ calculates mean for each AS02=1
  sdH   = map_dbl(fun_h, ~sd(data[[.x]],   na.rm = TRUE)), # same but with sd
  sdS   = map_dbl(fun_s, ~sd(data[[.x]],   na.rm = TRUE)), # same but with sd
  nH    = map_int(fun_h, ~sum(!is.na(data[[.x]]))), # same but with sd
  nS    = map_int(fun_s, ~sum(!is.na(data[[.x]]))) # same but with sd
) %>%
  mutate(across(meanH:sdS, ~round(.x, 2))) #orders the results according to the order defined in topic


# the rw version of bc cuts is different in levels, the rw version of crimes register and the pro whatsapp audios
# this might be the result of the already observed tendency of lw people to give lower grades to opposing view

# Lets check the distribution of rating of funniness across id_pol

data <- data %>%
  rowwise() %>%
  mutate(
    mean_rw_h   = mean(c_across(all_of(fun_h_rw)), na.rm = TRUE),
    mean_rw_s    = mean(c_across(all_of(fun_s_rw)), na.rm = TRUE),
    mean_lw_h  = mean(c_across(all_of(fun_h_lw)), na.rm = TRUE),
    mean_lw_s  = mean(c_across(all_of(fun_s_lw)), na.rm = TRUE)
  ) %>%
  ungroup()


describe(data$mean_lw_h)
describe(data$mean_lw_s)
describe(data$mean_rw_h)
describe(data$mean_rw_s)

data %>%
  group_by(id_pol) %>%
  summarise(
    across(c(mean_rw_h, mean_rw_s, mean_lw_h, mean_lw_s),
           list(mean = ~mean(.x, na.rm = TRUE))),
    .groups = "drop"
  )

#LW memes seem to be rated similarly across groups, but there appears to be greater divergence with rw memes. Similarly to what was observed in pilot study

# Checking random assignment ----------------------------------------------

  # Participants per condition (humor/serious)
table(data$AS02) #1 IS HUMOR


  # Lets check if the distribution of ideological position (grouped) varies across conditions
  chi_idpol<- chisq.test(table(data$id_pol, data$AS02))
  chi_idpol
  
  # And differences in gender
  chi_gen<- chisq.test(table(data$SD01, data$AS02))
  chi_gen
  
  # And differences in interest in politics  SD02
  chi_interes<- chisq.test(table(data$SD02, data$AS02))
  chi_interes #not realiable as there is few data in not at all interested in politics
  
  
  # And differences in self perceptoion of knowledge in politics
  chi_know<- chisq.test(table(data$CV02, data$AS02))
  chi_know

  # I can check if the order of question regarding political position changes distribution of self identification
  # in one group (1) the oa question regarding lw and rw are before self placement on a scale. The other groups is reversed
table(data$AS01)


  # I also randomized whether they answerted the negative (1) r positive (2) questionf about topic first
table(data$AS03)




# Creation of Long Format for LMM -----------------------------------------

# Characterization variables
var_char<-c("SD01", "SD02","SD06", "SD07", "CV02") #gender, interest in politics, political alignment, education level, self perception of knowledge in politics

# List of ao variables
var_ao<-c("ao_ir","ao_bc", "ao_sc", "ao_w", "ao_audio", "ao_cats", "ao_pol", "ao_npol")


# Selection of variables taht will go in long format
selected_vars <- c("CASE",var_char, "AS02","id_pol", var_ao, cinismo_list, relevance, fun_h, of_h, fun_s, of_s)
# sociodemographic and some coovariables, assignment group, political alignment grouped, variables of ao, cynicism variables. relevance to topics
  
df<-data[, selected_vars]
  

# Creating long format

df$AS02<-as.integer(df$AS02)


# 1. Topic / stimulus lookup  Pairing of topic with measurements fo funniness and offensiveness

topics <- tribble(
  ~ao_col,    ~topic_type,    ~stim_h_1, ~stim_h_2, ~stim_s_1, ~stim_s_2,
  "ao_sc",    "political",    "CQ01",    "CQ02",    "CQ13",    "CQ14", #covariates pertaining to this topic
  "ao_bc",    "political",    "CQ03",    "CQ04",    "CQ15",    "CQ16",
  "ao_ir",    "political",    "CQ05",    "CQ06",    "CQ17",    "CQ18",
  "ao_audio", "nonpolitical", "CQ07",    "CQ08",    "CQ19",    "CQ20",
  "ao_w",     "nonpolitical", "CQ09",    "CQ10",    "CQ21",    "CQ22",
  "ao_cats",  "nonpolitical", "CQ11",    "CQ12",    "CQ23",    "CQ24"
)



# 2. Long ambivalence scores  →  6 rows per person

df_ao <- df %>%
  select(all_of("CASE"), id_pol, AS02,
         starts_with("ao_"), -ao_pol, -ao_npol) %>%
  pivot_longer(
    cols      = -c(all_of("CASE"), id_pol, AS02),
    names_to  = "ao_col",
    values_to = "ambivalence"
  ) %>%
  left_join(topics %>% select(ao_col, topic_type), by = "ao_col")


# 3. Long CQ ratings  →  12 rows per person

df_cq <- df %>%
  select(all_of("CASE"), id_pol, AS02, matches("^CQ\\d{2}_0[12]$")) %>%
  pivot_longer(
    cols          = -c(all_of("CASE"), id_pol, AS02),
    names_to      = c("stimulus", "rating"),
    names_pattern = "(CQ\\d{2})_(0[12])",
    values_to     = "value"
  ) %>%
  pivot_wider(
    id_cols      = c(all_of("CASE"), id_pol, AS02, stimulus),
    names_from   = rating,
    values_from  = value,
    names_prefix = "rating_"
  ) %>%
  rename(fun = rating_01, of = rating_02) %>%
  filter(!is.na(fun) | !is.na(of))



# 4. Map each stimulus to (ao_col, pos = 1|2) for the person's condition

topics_long <- topics %>%
  pivot_longer(
    cols          = -c(ao_col, topic_type),
    names_to      = c("cond", "pos"),
    names_pattern = "stim_(.)_(.)",
    values_to     = "stimulus"
  ) %>%
  mutate(AS02 = if_else(cond == "h", 1L, 2L)) %>%
  select(-cond)



# 5. Widen so each ao row carries fun_1, fun_2, of_1, of_2

df_cq_wide <- df_cq %>%
  left_join(topics_long, by = c("stimulus", "AS02")) %>%
  pivot_wider(
    id_cols     = c(all_of("CASE"), id_pol, AS02, ao_col, topic_type),
    names_from  = pos,
    values_from = c(fun, of)
  )
# columns created: fun_1, fun_2, of_1, of_2



# 6. Final long data.frame (+ person-level covariates)

df_long <- df_ao %>%
  left_join(df_cq_wide,
            by = c("CASE", "id_pol", "AS02", "ao_col", "topic_type")) %>%
  left_join(
    df %>% select(CASE, SD01, SD02, SD06, SD07, CV02,
                  CY02_01:CY02_08, SD03_01:SD03_06,
                  ao_pol, ao_npol),
    by = "CASE"
  ) #%>%
 # select(AS02)

rm(topics, df_ao, df_cq,topics_long, df_cq_wide)

# 7. Optional: collapse the pair to a mean covariate

# df_long <- df_long %>%
#   mutate(fun_mean = rowMeans(cbind(fun_1, fun_2), na.rm = TRUE),
#          of_mean  = rowMeans(cbind(of_1,  of_2 ), na.rm = TRUE))








# Analysis of OA ----------------------------------------------------------

df_long$AS02<-as.factor(df_long$AS02)
df_long$topic_type <-as.factor(df_long$topic_type) # 1  is non political 

str(df_long$topic_type)

# I should define as baselien comparisions: serious, and non political
df_long$AS02 <- relevel(df_long$AS02, ref = "2") # reference is serious
df_long$topic_type <- relevel(df_long$topic_type, ref = "political") # reference is political


factor(df_long$AS02, levels = c(2, 1), labels = c("Serious", "Humor"))

m1 <- lmer(ambivalence ~ AS02* topic_type + (1 | CASE), # 
               data = df_long, REML = TRUE)

summary(m1, ddf="Kenward-Roger")

emm <- emmeans(m1, ~ AS02 | topic_type, lmer.df = "kenward-roger")
contrast(emm, "revpairwise")                                # Humor − Serious within each topic type
contrast(emmeans(m1, ~ AS02), "revpairwise")   


# Does humor in messages expressing opposing views increases objective ambivalence?


# Does the effect of humor on oa changes across topic? Hypothesis is that it will be greater in non political


# Does using humor cues in political messages expressing opposing views increases objective ambivalence compared to not using humorous cues?


# This results might be the results of Winner's Curse. Humor does seem to be doing something. However this should be viewed with great skepticism, as this pilot is extremely underpowered and effect will probably be quite different in the final study!!




####
t.test(ao_cats ~ AS02, data = data, var.equal = FALSE)
cohens_d(ao_ep~AS02, data=data)

