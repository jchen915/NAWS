rm(list = ls())

install.packages("readxl")
library(readxl)

install.packages("tidyverse")
library(tidyverse) 
library(tidyr)

install.packages("data.table")
library(data.table)
library(dplyr)
library(tibble)

library(parallel)
library(ggplot2)
library(lubridate)

install.packages("reshape2")
library(reshape2)

library(stringr)
library(readr)
install.packages("openxlsx")
library(openxlsx)
install.packages("writexl")
library(writexl)
install.packages("assertthat")
library(assertthat)

install.packages("ggthemes")
library(ggthemes)
install.packages("plotrix")
library(plotrix)
install.packages("ggpubr")
library(ggpubr)

library(gridExtra)
install.packages("patchwork")
library(patchwork)

library(RColorBrewer)

install.packages("doParallel")
library(doParallel)

library(foreach)
install.packages("xlsx")
library("xlsx")

install.packages("grf")
library(grf)

library(plyr)
install.packages("MatchIt")
library(MatchIt)


df_NAWS <- read.csv("E:/MelissaFranco/Data/Orginal/nawscrtdvars1db20_CSV.csv")

view(head(df_NAWS))
saveRDS(df_NAWS, file = "E:/MelissaFranco/Data/Jackie/NAWS_2020.Rds")

#check for variables
c("REALAGE") %in% names(df_NAWS)
#search for relevant variables
grep("age", names(df_NAWS), value = TRUE)
#examine specific variable
table(df_NAWS$L01, useNA = "ifany")

#Clean up missing and ambiguous data
df_NAWS_clean <- df_NAWS[!df_NAWS$L01 %in% c(4, 5, 7, 8, 96) & !is.na(df_NAWS$L01), ]
saveRDS(df_NAWS_clean, file = "E:/MelissaFranco/Data/Jackie/NAWS_clean_2020.Rds")



######### Citizen vs. PR (C vs PR) #########
# Subset to Citizens (1,2) and PRs (3)
df_C_PR <- df_NAWS_clean[df_NAWS_clean$L01 %in% c(1, 2, 3), ]

# Create binary treatment variable: 1 = Citizen (1,2), 0 = PR (3)
df_C_PR$currstat_C_PR <- ifelse(df_C_PR$L01 %in% c(1, 2), 1,
                                ifelse(df_C_PR$L01 == 3, 0, NA))

# Check result
table(df_C_PR$currstat_C_PR, useNA = "ifany")
#  0(PR)     1(Citizen) 
#17982         18238 

# Filter out NA values from the binary treatment variable
df_C_PR <- df_C_PR[!is.na(df_C_PR$currstat_C_PR), ]

# Tabulate group sizes
currstat_C_PR <- table(df_C_PR$currstat_C_PR)
currstat_C_PR_CHECK <- as.data.frame(currstat_C_PR)

# Save the forked dataset
saveRDS(df_C_PR, file = "E:/MelissaFranco/Data/Jackie/df_C_PR.rds")


######### Citizen vs. Undocumented (C vs UD) #########
# Subset to Citizens (1,2) and Undocumented (6)
df_C_UD <- df_NAWS_clean[df_NAWS_clean$L01 %in% c(1, 2, 6), ]

# Create binary treatment variable: 1 = Citizen, 0 = Undocumented
df_C_UD$currstat_C_UD <- ifelse(df_C_UD$L01 %in% c(1, 2), 1,
                                ifelse(df_C_UD$L01 == 6, 0, NA))

# Check result
table(df_C_UD$currstat_C_UD, useNA = "ifany")
# 0(UD)     1(C) 
#29605    18238 

# Filter out NA values from the binary treatment variable
df_C_UD <- df_C_UD[!is.na(df_C_UD$currstat_C_UD), ]

# Tabulate group sizes
currstat_C_UD <- table(df_C_UD$currstat_C_UD)
currstat_C_UD_CHECK <- as.data.frame(currstat_C_UD)

# Save the forked dataset
saveRDS(df_C_UD, file = "E:/MelissaFranco/Data/Jackie/df_C_UD.rds")


######### PR vs. Undocumented (PR vs UD) #########
# Subset to only PRs (3) and Undocumented (6)
df_PR_UD <- df_NAWS_clean[df_NAWS_clean$L01 %in% c(3, 6), ]

# Create binary treatment variable: 1 = PR, 0 = Undocumented
df_PR_UD$currstat_PR_UD <- ifelse(df_PR_UD$L01 == 3, 1,
                                  ifelse(df_PR_UD$L01 == 6, 0, NA))

# Check result
table(df_PR_UD$currstat_PR_UD, useNA = "ifany")
#0(UD)     1(PR) 
#29605     17982

# Filter out NA values from the binary treatment variable
df_PR_UD <- df_PR_UD[!is.na(df_PR_UD$currstat_PR_UD), ]

# Tabulate group sizes
currstat_PR_UD <- table(df_PR_UD$currstat_PR_UD)
currstat_PR_UD_CHECK <- as.data.frame(currstat_PR_UD)

# Save the forked dataset
saveRDS(df_PR_UD, file = "E:/MelissaFranco/Data/Jackie/df_PR_UD.rds")

#================================== 
### Citizen(C) vs. PR ###
colnames_to_check <- c("currstat_C_PR", "A03", "REALage", "FAMPOV", "A21a", "A09", "PWTYCRD")
summary(df_C_PR[, colnames_to_check])

###PSM: no matching(raw data)###
#clean up forked dataset
df_C_PR_clean <- df_C_PR[complete.cases(df_C_PR[, colnames_to_check]), ]

#C & PR
df_C_PR <- readRDS("E:/MelissaFranco/Data/Jackie/df_C_PR.rds")
NM_C_PR.out <- matchit(currstat_C_PR ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                  data = df_C_PR_clean,
                  method = NULL, #no matching
                  distance = "glm")
summary(NM_C_PR.out)

#Covariates: A03 Gender, REALage age, FAMPOV fam income below pov level, 
#A21a having health insurance, A09 highest grade in school, PWTYCRD weight used when working with several years of data

#PSM: Nearest-neighbor (1:1)
M_C_PR.out <- matchit(currstat_C_PR ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                 data = df_C_PR_clean,
                 method = "nearest",
                 distance = "glm",
                 estimand = "ATT")
summary(M_C_PR.out)

#plot covariates balance
plot(summary(M_C_PR.out), var.order = "unmatched")

#Plot PS distributions
plot(M_C_PR.out, type = "hist")  # Overlapping histograms
plot(M_C_PR.out, type = "jitter")  # For overlap visualization

#extract matched sample
df_C_PR_matched <- match.data(M_C_PR.out)

#post-matching regression, outcome: 
fit_C_PR <- lm(NQ01x ~ currstat_C_PR, data = df_C_PR_matched)
summary(fit_C_PR)

###Visualizing Graphs
# Sample outcome summary
df_C_PR_matched$group <- ifelse(df_C_PR_matched$currstat_C_PR == 1, "Citizen", "PR")

#table(df_C_PR_matched$group, useNA = "ifany")

# Healthcare utilization(hu)
hu_summary <- df_C_PR_matched %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(mean_hu = mean(NQ01x, na.rm = TRUE), .groups = "drop")

#print(hu_summary)

ggplot(hu_summary, aes(x = group, y = mean_hu, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(mean_hu, accuracy = 0.1)),
            vjust = -0.5, size = 4.5) +  # Adjust `vjust` and `size` as needed
  labs(title = "Healthcare Utilization: Citizens vs PRs",
       x = "", y = "Sample Mean (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(hu_summary$mean_hu) + 0.1)) +
  theme_minimal()


#================================== 
### Citizen(C) vs. Undocumented ###
colnames_to_check <- c("currstat_C_UD", "A03", "REALage", "FAMPOV", "A21a", "A09", "PWTYCRD")
summary(df_C_UD[, colnames_to_check])

# Clean up forked dataset
df_C_UD_clean <- df_C_UD[complete.cases(df_C_UD[, colnames_to_check]), ]

# Load data
df_C_UD <- readRDS("E:/MelissaFranco/Data/Jackie/df_C_UD.rds")

# PSM: No matching (raw data)
NM_C_UD.out <- matchit(currstat_C_UD ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                       data = df_C_UD_clean,
                       method = NULL,
                       distance = "glm")
summary(NM_C_UD.out)

# PSM: Nearest-neighbor (1:1)
M_C_UD.out <- matchit(currstat_C_UD ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                      data = df_C_UD_clean,
                      method = "nearest",
                      distance = "glm",
                      estimand = "ATT")
summary(M_C_UD.out)

# Plot balance and PS distribution
plot(summary(M_C_UD.out), var.order = "unmatched")
plot(M_C_UD.out, type = "hist")
plot(M_C_UD.out, type = "jitter")

# Extract matched data
df_C_UD_matched <- match.data(M_C_UD.out)

# Post-matching regression
fit_C_UD <- lm(NQ01x ~ currstat_C_UD, data = df_C_UD_matched)
summary(fit_C_UD)

# Add group label
df_C_UD_matched$group <- ifelse(df_C_UD_matched$currstat_C_UD == 1, "Citizen", "Undocumented")

# Summary + Plot
hu_summary <- df_C_UD_matched %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(mean_hu = mean(NQ01x, na.rm = TRUE), .groups = "drop")

ggplot(hu_summary, aes(x = group, y = mean_hu, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(mean_hu, accuracy = 0.1)),
            vjust = -0.5, size = 4.5) +
  labs(title = "Healthcare Utilization: Citizens vs Undocumented",
       x = "", y = "Sample Mean (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(hu_summary$mean_hu) + 0.1)) +
  theme_minimal()


#================================== 
### PR vs. Undocumented(UD) ###
colnames_to_check <- c("currstat_PR_UD", "A03", "REALage", "FAMPOV", "A21a", "A09", "PWTYCRD")
summary(df_PR_UD[, colnames_to_check])

# Clean up forked dataset
df_PR_UD_clean <- df_PR_UD[complete.cases(df_PR_UD[, colnames_to_check]), ]

# Load data
df_PR_UD <- readRDS("E:/MelissaFranco/Data/Jackie/df_PR_UD.rds")

# PSM: No matching (raw data)
NM_PR_UD.out <- matchit(currstat_PR_UD ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                        data = df_PR_UD_clean,
                        method = NULL,
                        distance = "glm")
summary(NM_PR_UD.out)

# PSM: Nearest-neighbor (1:1)
M_PR_UD.out <- matchit(currstat_PR_UD ~ A03 + REALage + FAMPOV + A21a + A09 + PWTYCRD,
                       data = df_PR_UD_clean,
                       method = "nearest",
                       distance = "glm",
                       estimand = "ATT")
summary(M_PR_UD.out)

# Plot balance and PS distribution
plot(summary(M_PR_UD.out), var.order = "unmatched")
plot(M_PR_UD.out, type = "hist")
plot(M_PR_UD.out, type = "jitter")

# Extract matched data
df_PR_UD_matched <- match.data(M_PR_UD.out)

# Post-matching regression
fit_PR_UD <- lm(NQ01x ~ currstat_PR_UD, data = df_PR_UD_matched)
summary(fit_PR_UD)

# Add group label
df_PR_UD_matched$group <- ifelse(df_PR_UD_matched$currstat_PR_UD == 1, "PR", "Undocumented")

# Summary + Plot
hu_summary <- df_PR_UD_matched %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(mean_hu = mean(NQ01x, na.rm = TRUE), .groups = "drop")

ggplot(hu_summary, aes(x = group, y = mean_hu, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(mean_hu, accuracy = 0.1)),
            vjust = -0.5, size = 4.5) +
  labs(title = "Healthcare Utilization: PRs vs Undocumented",
       x = "", y = "Sample Mean (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(hu_summary$mean_hu) + 0.1)) +
  theme_minimal()



#-----------------
#Citizen(1+2) vs. PR(3)
df_NAWS_clean$currstat_C_PR <- mapvalues(df_NAWS_clean$L01,
                                         from = c(1, 2, 3, 6),
                                         to   = c(1, 1, 0, NA))  # 1 = Citizen, 0 = PR, exclude undocumented for now

#PR(3) vs. UD(6)
df_NAWS_clean$currstat_PR_UD <- mapvalues(df_NAWS_clean$L01,
                                          from = c(3, 6),
                                          to   = c(1, 0))  # 1 = PR, 0 = Undoc

table(df_NAWS_clean$currstat_C_PR, useNA = "ifany")

#    0     1  <NA> 
#17982 18238 29605 

table(df_NAWS_clean$currstat_PR_UD, useNA = "ifany")





library(dplyr)
df_NAWS <- df_NAWS %>%
  mutate(state_fips = substr(sprintf("%05d", County), 1, 2))  # ensures 5-digit formatting

state_counts <- df_NAWS %>%
  group_by(state_fips) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(state_counts)

# Filter for California counties only (state_fips == "06")
ca_county_counts <- df_NAWS %>%
  filter(state_fips == "06") %>%
  group_by(County) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(ca_county_counts)

df_NAWS_hhgrid   <- read.csv("E:/MelissaFranco/Data/Orginal/hhgrid20_CSV.csv")

view(head(df_NAWS_hhgrid))

df_NAWS_workgrid <- read.csv("E:/MelissaFranco/Data/Orginal/workgrid20_CSV.csv")

view(head(df_NAWS_workgrid))

# Example dataframe
df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))

# Save as Excel file
write_xlsx(df, "E:/MelissaFranco/Data/Jackie/my_file.xlsx")
