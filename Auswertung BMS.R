#<<<<<<<<<<<HEAD 

##Präambel
setwd("Z:/Aktuell/Eigene Dateien/Eigene Dateien_Sarah/R/Skripte")

#loading custom functions
source("R_functions.R")

# before loading packages check for updates (right side under "packages")
# abdawdwad
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)


#########################################
#                                       #
#       1. load and transform data      #
#                                       #
#########################################

# set working directory
setwd("Z:/Aktuell/Eigene Dateien/Eigene Dateien_Sarah/R/Daten")

filename <- "BMS_miRNAs.xlsx"
dat_raw <- read_xlsx(filename)
?read_xlsx
# data inspection
view(dat_BMS)

# filter only data from patients at time 0
# reduce Stadium to only I, II, III, IV
# filter T, N, M stage to only show main groups and not substages
# convert character columns containing numbers to numeric columns
dat_BMS <- dat_raw %>% filter(str_detect(dat_raw$BXT, "^BXT_\\d{4}_0")) %>%
  mutate(Stadium = str_extract(Stadium, "^[IV]{1,3}")) %>%
  mutate(T_stage = str_extract(T_stage, "T\\d{1}"))  %>% 
  mutate(N_stage = str_replace_all(N_stage, c("o","O"),"0")) %>% 
  mutate(N_stage = str_extract(N_stage, "N\\d{1}")) %>% 
  mutate(M_stage = str_extract(M_stage, "M\\d{1}")) %>% 
  mutate_at(c("CRP", "LDH", "Eosinophile", "S100"), parse_number) 
  
names(dat_BMS) <- str_replace_all(names(dat_BMS), "hsa-", "") %>% str_replace_all("-","_")  

###########################################
#                                         #
#       2. Exploratory data analysis      #
#                                         #
###########################################
  
# make histogram to look at distribution of numerical variables
# in 2. Zeile Tumordicke, CRP,LDH, Eosinophile, S100, age, miRNAs eintragen
dat_BMS %>% 
  ggplot(aes(CRP)) + #plot histogram with normal values
  # ggplot(aes(log(CRP))) + #plot histogram with log-transformed values
  geom_histogram(color = "black", fill ="lightgrey") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_pubr() 

# make histogram to look at distribution of categorical variables
# in 2. Zeile N_stage, T_stage, M_stage, Stadium, sex eintragen
dat_BMS %>% 
  ggplot(aes(M_stage)) + #plot histogram with normal values
  geom_histogram(color = "black", fill ="lightgrey", stat ="count") +
  scale_y_continuous(expand = c(0,0)) +
  theme_pubr() 




################################
#                              #
#       3.  data analysis      #
#                              #
#################################

# put all miRNAs into one column for better data representation
dat_plot <- dat_BMS %>% 
  gather(names(dat_BMS)[str_detect(names(dat_BMS), "mir|let")], key = "miRNA", value = "expression") %>%
  mutate(expression = parse_number(expression)) %>%
  mutate(expression = round(expression, 2)) %>%
  mutate(Stadium = factor(Stadium, levels = c("I", "II","III", "IV"))) %>%
  mutate(T_stage = factor(T_stage, levels = c("T0", "T1","T2", "T3","T4"))) %>%
  mutate(N_stage = factor(N_stage, levels = c("N0", "N1","N2", "N3"))) %>%
  mutate(M_stage = factor(M_stage, levels = c("M0", "M1"))) #%>%
  # mutate(expression = ifelse(expression < 0, NA, expression)) %>%
  # mutate(log_exp = log(expression+1))

# influence of categorical variables on miRNA expression
plot <- signif_plot_Melanoma(dat_plot, x="T_stage", y="expression", signif=0.05, 
                     plot.type = "boxplot", significance=F, Legend = F, filter.p.adj = F,
                     method ="t.test", p.label="{p.signif}", facet="miRNA", ylab = "miRNA expression")

plot # graph and stat results combined 
plot$graph # get the graph
plot$stat_test_results # get the statistical results

plot$stat_test_results %>% print(n="all")


# Probleme bzw. Dinge, die wir uns überlegen müssen
  # negative Werte -> als NA setzen? als 0 setzen? den niedrigsten Wert addieren sodass es keine negativen Werte gibt? miRNAs, die negative Werte haben
    # nicht mit auswerten?
  # welcher stat test --> wilcoxon oder t.test? 
  # normale Expressiondaten oder log-transformierte Daten? 
    # Expressiondaten sind log-normalverteilt, daher würde ich log-transformierte Daten mit einem T-Test auswerten
    # Das Problem dabei ist, dass manche miRNAs dann nicht genügend Observationen für statistische Tests haben --> möglicherweise muss man die miRNAs
      # dann komplett von der Analyse ausschließen

# Ansatz momentan: Normale Expressionsdaten ohne Korrektur negativer Werte, um eine Übersicht zu bekommen



