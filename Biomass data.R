
# ggplot
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("readxl")
install.packages("rcartocolor")
install.packages("ggbreak")
install.packages("gridExtra")
install.packages("rpsychi")
install.packages("gtools")
install.packages("remotes")
install_version("rpsychi", "0.8") 
install.packages("cowplot")
install.packages("multcomp")
install.packages("ggpubr")



library(cowplot)
library(ggplot2)
library(ggthemes)
library(readxl)
library(rcartocolor)
library(ggbreak)
library(dplyr)
library(gridExtra)
library(gtools)
library(remotes)
library(rpsychi)
library(multcomp)
library(tidyr)
library(ggpubr)



# Importing the biomass composition data as biomass_data
biomass_data <- read_excel("Biomass composition.xlsx", sheet = "Sheet1")
head(biomass_data)

# Displaying the color blind friendly palettes 
display_carto_all(colorblind_friendly = TRUE)

# Plotting a bar graph showing the content of each biomass component
# default colors
ggplot(data=biomass_data, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_bar(stat = "identity", position=position_dodge(), col="black") + theme_minimal() 

# Creating a colorblind friendly palette: 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)

# Same plot as before, but with colorblind friendly colors
ggplot(data=biomass_data, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_bar(stat = "identity", position=position_dodge(), col = "black") + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette)


# Making a plot of only the biomass components that we have actually measured, 
# because the rest is constant

# First, filtering biomass_data 
biomass_measured <- filter(biomass_data, component == "Prot" | component == "RNA" |
                             component == "DNA" | component == "Lipid")

biomass_measured

# Barplot with standard colors
ggplot(biomass_measured, aes(fill=component, x = biomass_sample, y = g_gDW_percent)) +
  geom_bar(position = "dodge", stat = "identity", col="black")

# Sorting the data:
biomass_measured$biomass_sample <- factor(biomass_measured$biomass_sample, 
                                      levels = c("Orig.", "WTG", "WTM", "MDHG", "MDHM"))


biomass_measured$component <- factor(biomass_measured$component, 
                                 levels = c("Prot","Lipid", "RNA", "DNA"))


# Creating barplot with correct labels, error bars, and axis break
ggplot(data=biomass_measured, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  scale_y_break(c(10,50)) + 
  geom_errorbar(aes(ymin=g_gDW_percent - sd, ymax = g_gDW_percent + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("g/gDW (%)") + labs(fill = "Component") +
  geom_text(
    aes(label = round(g_gDW_percent,1)), 
    position = position_dodge(0.9),
    col = "white",
    vjust = 2,
    fontface = "bold",
    size = 3
  )

ggsave(filename = "biomass composition measured.pdf",
       device = "pdf",
       height = 5, 
       width = 8)

# adjusted barplot. This barplot has corrected SD-values 
# and a different color scheme

# we actually dont know the SD of the values that have been used in iBsu1147, 
# so we change the SD values to 0 
biomass_measured$sd[1] <- 0
biomass_measured$sd[2] <- 0
biomass_measured$sd[3] <- 0
biomass_measured$sd[4] <- 0

ggplot(data=biomass_measured, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  scale_y_break(c(10,50)) + 
  geom_errorbar(aes(ymin=g_gDW_percent - sd, ymax = g_gDW_percent + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("g/gDW (%)") + labs(fill = "Component") +
  geom_text(
    aes(label = round(g_gDW_percent,1)), 
    position = position_dodge(0.9),
    col = "white",
    vjust = 2,
    fontface = "bold",
    size = 3
  )

ggsave(filename = "biomass composition measured.pdf",
       device = "pdf",
       height = 5, 
       width = 8)

# Plot that contains all the biomass components: 

# Sorting the data:
biomass_data$biomass_sample <- factor(biomass_data$biomass_sample, 
                                          levels = c("Orig.", "WTG", "WTM", "MDHG", "MDHM"))


biomass_data$component <- factor(biomass_data$component, 
                                     levels = c("Prot", "RNA","DNA","Lipid", 
                                                "Lipo_acid", "Cell wall", "Ions"))



# Bar plot that contains all biomass components
# A bit crowded imo. 

ggplot(data=biomass_data, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  scale_y_break(c(22,52)) +
  geom_errorbar(aes(ymin=g_gDW_percent - sd, ymax = g_gDW_percent + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("g/gDW (%)") + labs(fill = "Component")


# Sorting (Sorting is commented out, because when the order is changed, 
# the components get different colors between graphs)
# biomass_data$component <- factor(biomass_data$component, 
#                                 levels = c("Ions","Cell wall","Lipo_acid","Lipid","DNA","RNA", "Prot"))

# Building error bar data
error_bars = biomass_data %>%
  arrange(biomass_sample, desc(component)) %>%
  group_by(biomass_sample) %>%
  mutate(new = cumsum(g_gDW_percent)) %>%
  ungroup()

error_bars

# Creating a stacked bar graph with color blind friendly colors 
ggplot(data=biomass_data, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_col(col="black", width = 0.8) + theme_minimal() +
  geom_text(
    aes(label = round(g_gDW_percent,1)), 
    position = position_stack(vjust = 0.5),
    col = "black",
    size = 3.5
    ) + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)+
  geom_errorbar(data = error_bars, aes(ymin = new - sd, ymax = new + sd), 
                width = 0.3, position = "identity")+
  xlab("Biomass sample") + ylab("g/gDW (%)") + labs(fill = "Component")


# Making a stacked bar graph of the measured components

# Building error bar data
biomass_measured

error_bars = biomass_measured %>%
  arrange(biomass_sample, desc(component)) %>%
  group_by(biomass_sample) %>%
  mutate(new = cumsum(g_gDW_percent)) %>%
  ungroup()

error_bars

# Plotting
ggplot(data=biomass_measured, aes(x=biomass_sample, y = g_gDW_percent, fill = component)) + 
  geom_col(col="black", width = 0.8) + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)+
  geom_errorbar(data = error_bars, aes(ymin = new - sd, ymax = new + sd), 
                width = 0.3, position = "identity")+
  xlab("Biomass sample") + ylab("g/gDW (%)") + labs(fill = "Component") +
  geom_text(
    aes(label = round(g_gDW_percent,1)), 
    position = position_stack(vjust = 0.5),
    col = "white",
    fontface = "bold",
    size = 3.5) + 
  #scale_y_break(c(20,40)) + 
  scale_y_break(c(45,65)) + 
  theme(
    axis.text.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  )+ 
  scale_y_continuous(breaks = seq(0, 72, by = 5))
?scale_y_break  


# Plotting growth data 
# Importing the growth data as growth_data
growth_data <- read_excel("growth data.xlsx")

growth_data$biomass_sample <- factor(growth_data$biomass_sample, 
                                     levels = c("WTG", "WTM", "MDHG", "MDHM"))

# Trekk eksponentiell linje heller 
# Plotting growth curves based on OD600 measurements
od_plot <- ggplot(growth_data, aes(x = time_h, y = avg_OD600, colour=biomass_sample)) +
  geom_point() +
  geom_line()+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_errorbar(aes(ymin=avg_OD600 - SD_OD, ymax = avg_OD600 + SD_OD),
                width = 0.1) +
  xlab("Time (h)") + ylab("OD600") + 
  labs(colour = "Biomass \n sample") 

od_plot

# Trying to illustrate where we have decided that the bacteria grow exponentially: 
od_plot <- ggplot(growth_data, aes(x = time_h, y = avg_OD600, colour=biomass_sample)) +
  geom_point() +
  geom_line()+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_errorbar(aes(ymin=avg_OD600 - SD_OD, ymax = avg_OD600 + SD_OD),
                width = 0.1) +
  xlab("Time (h)") + ylab("OD600") + 
  labs(colour = "Biomass \n sample") 

ggsave(filename = "OD600_over_time.pdf",
       device = "pdf",
       height = 3, 
       width = 7)

# Removing the first row of MDHG and MDHM 
growth_data <- growth_data[-c(25,38),]

growth_data$biomass_sample <- factor(growth_data$biomass_sample, 
                                     levels = c("WTG", "WTM", "MDHG", "MDHM"))

# Plotting growth curves based on dry weight measurements
ggplot(data=subset(growth_data, !is.na(avg_CDW_mg_mL)), aes(x = time_h, y = avg_CDW_mg_mL, colour=biomass_sample)) +
  geom_point() +
  geom_line()+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_errorbar(aes(ymin=avg_CDW_mg_mL - SD_CDW, ymax = avg_CDW_mg_mL + SD_CDW),
                width = 0.1)+
  xlab("Time (h)") + ylab("CDW (mg/mL)") + 
  labs(colour = "Biomass sample")


# Growth rates with standard deviation 

growth_rates <- read_excel("growth_rates.xlsx")

growth_rates$biomass_sample <- factor(growth_rates$biomass_sample, 
                                          levels = c("WTG", "WTM", "MDHG", "MDHM"))

growth_rates$avg_slope <- round(growth_rates$avg_slope, 2)

growth_rates$sd
ggplot(data=growth_rates, aes(x=biomass_sample, y = avg_slope, fill=biomass_sample)) + 
  geom_col(col="black", width = 0.8) + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_errorbar(aes(ymin=avg_slope - sd, ymax = avg_slope + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("Growth rate (h^-1)") + labs(fill = "Biomass sample") +
  geom_text(aes(label=avg_slope), vjust = 4.5, color="white", size=3.5)


df1 <- data.frame(growth_rates$avg_slope, growth_rates$sd, growth_rates$predicted,
                  growth_rates$biomass_sample)

colnames(df1) <- c("Measured", "SD", "Predicted", "Biomass_sample")
# removing the rows with NA: 

df1 <- na.omit(df1)

df2 <- tidyr::pivot_longer(df1, cols=c(Measured, Predicted), 
                           names_to="type", values_to = "growth_rate")

df2$SD[2]<- NaN
df2$SD[4]<- NaN
df2$SD[6]<- NaN
df2$SD[8]<- NaN


ggplot(df2, aes(x=Biomass_sample, y=growth_rate, fill = type)) +
  geom_bar(stat='identity', position='dodge', col="black") +
  theme_minimal() +
  geom_errorbar(aes(ymin=growth_rate - SD, ymax = growth_rate + SD), 
                width = .3, position = position_dodge(.9)) + 
  xlab("Biomass sample") +
  ylab("Growth rate (h^-1)") + labs(fill = "Type")

ggsave(filename = "Growth rates measured vs predicted.pdf", 
       width = 7, 
       height = 4)
           
# Importing the secretion and uptake rate data: 
medium_data <- read_excel("uptake_and_secretion.xlsx")

# Scatter plots: 

glucose_plot <- ggplot(data = subset(medium_data, medium_data$compound=="glucose"), 
                       aes(x = time_h, y = avg_concentration_mmol_L, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)  

methanol_plot <- ggplot(data = subset(medium_data, medium_data$compound=="methanol"), 
                       aes(x = time_h, y = avg_concentration_mmol_L, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)

acetate_plot <- ggplot(data = subset(medium_data, medium_data$compound=="acetate"), 
                        aes(x = time_h, y = avg_concentration_mmol_L, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)

glu_plot <- ggplot(data = subset(medium_data, medium_data$compound=="l_glu"), 
                       aes(x = time_h, y = avg_concentration_mmol_L, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)
  
trp_plot <- ggplot(data = subset(medium_data, medium_data$compound=="l_trp"), 
                   aes(x = time_h, y = avg_concentration_mmol_L, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)

# Sorting the data 
medium_data$biomass_sample <- factor(medium_data$biomass_sample, 
                                          levels = c("WTG", "WTM", "MDHG", "MDHM"))

medium_data$compound <- factor(medium_data$compound, 
                                     levels = c("glucose", "methanol", "acetate","l_glu", "l_trp"))

#medium_data$uptake_rate_mmol_gDW_h <- round(medium_data$uptake_rate_mmol_gDW_h, 1)

rng = diff(range(medium_data$uptake_rate_mmol_gDW_h))

# Making a bar graph to visualize the calculated uptake and secretion rates 
ggplot(data=medium_data, aes(x=biomass_sample, y = uptake_rate_mmol_gDW_h, fill = compound)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  #scale_y_break(c(0,2)) +
  geom_errorbar(aes(ymin = uptake_rate_mmol_gDW_h - sd_uptake_rate, ymax = uptake_rate_mmol_gDW_h + sd_uptake_rate), width = .3, position = position_dodge(.9)) +
  xlab("Fermentation") + ylab("Specific uptake- or secretion rate (mmol/gDW h)") + labs(fill = "Compound") + 
  scale_fill_manual(values = cbPalette, name = "Compound", labels = c("Glucose", "Methanol", "Acetate", "L-glu", "L-trp")) 
  #geom_text(aes(label=round(uptake_rate_mmol_gDW_h, 1), 
               # y = ifelse(uptake_rate_mmol_gDW_h < 0, uptake_rate_mmol_gDW_h - sd_uptake_rate - 0.5, uptake_rate_mmol_gDW_h + sd_uptake_rate + 0.5)),
            #colour="black", fontface = "bold", size=3.5, position = position_dodge(0.9)) 

ggsave(filename = "uptake- and secretion rates.pdf", 
       width = 7, 
       height = 4)
# make a plot without l_trp, because uptake rate of l_trp is so low? 

ggplot(data=filter(medium_data, !compound=="l_trp"), aes(x=biomass_sample, y = uptake_rate_mmol_gDW_h, fill = compound)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  #scale_y_break(c(0,2)) +
  geom_errorbar(aes(ymin = uptake_rate_mmol_gDW_h - sd_uptake_rate, ymax = uptake_rate_mmol_gDW_h + sd_uptake_rate), width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("Uptake rate (mmol/gDW h)") + labs(fill = "Compound") +
  geom_text(aes(label=round(uptake_rate_mmol_gDW_h, 1), 
                y = ifelse(uptake_rate_mmol_gDW_h < 0, uptake_rate_mmol_gDW_h - sd_uptake_rate - 0.3, uptake_rate_mmol_gDW_h + sd_uptake_rate + 0.3)),
            colour="black", size=3.5, position = position_dodge(.9))


# Plotting the specific substrate uptake rates

q_data <- read_excel("q_substrate.xlsx")

ggplot(data = subset(q_data, q_data$substrate=="l_glu"), 
                   aes(x = time_h, y = q_substrate, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)

# Plotting predicted medium concentrations 

medium_predicted <- read_excel("predicted_medium_concentrations.xlsx")

ggplot(data = subset(medium_predicted, medium_predicted$compound=="acetate"), 
       aes(x = time_h, y = predicted_concentration, colour = biomass_sample)) +
  geom_point() + 
  geom_line() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette)

# Growth rates analysis: 
cdw_data <- read_excel("cdw_measurements.xlsx")

cdw_data <- cdw_data[-c(14,21),]

cdw_data$biomass_sample <- factor(cdw_data$biomass_sample,
                                  levels = c("WTG", "WTM", "MDHG", "MDHM"))

ggplot(data = cdw_data, aes(x=time_h, y=log_avg_mg_mL, colour=biomass_sample)) +
  geom_point() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_smooth(method='lm') +
  geom_errorbar(aes(ymin=log_avg_mg_mL - sd, ymax = log_avg_mg_mL + sd), 
                width = .1) +
  xlab("Time (h)") + ylab("Avg. log_(mg/mL)") + labs(colour="Biomass sample")


wtg <- lm(formula = log_avg_mg_mL ~ time_h,
            data = subset(cdw_data, biomass_sample=="WTG"))


wtm <- lm(formula = log_avg_mg_mL ~ time_h,
          data = subset(cdw_data, biomass_sample=="WTM"))


mdhg <- lm(formula = log_avg_mg_mL ~ time_h,
          data = subset(cdw_data, biomass_sample=="MDHG"))

  
mdhm <- lm(formula = log_avg_mg_mL ~ time_h,
          data = subset(cdw_data, biomass_sample=="MDHM"))

summary(wtg)
wtg$coefficients
wtm$coefficients
mdhg$coefficients
mdhm$coefficients

wtg2 <- lm(formula = avg ~ time_h,
          data = subset(cdw_data, biomass_sample=="WTG"))


wtm2 <- lm(formula = avg ~ time_h,
          data = subset(cdw_data, biomass_sample=="WTM"))


mdhg2 <- lm(formula = avg ~ time_h,
           data = subset(cdw_data, biomass_sample=="MDHG"))


mdhm2 <- lm(formula = avg ~ time_h,
           data = subset(cdw_data, biomass_sample=="MDHM"))

wtg2$coefficients
wtm2$coefficients
mdhg2$coefficients
mdhm2$coefficients

ggplot(data = cdw_data, aes(x=time_h, y=avg, colour=biomass_sample)) +
  geom_point() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_smooth(method='lm')

# ANOVA, want to see if the growth rates are significantly different from each other 

slope_anova <- aov(data = growth_rates, slope ~ biomass_sample)

summary(slope_anova)


slope_anova_mdh <- aov(data = subset(growth_rates, 
                                     growth_rates$biomass_sample=="MDHG"|
                                       growth_rates$biomass_sample=="MDHM"),
                       slope ~ biomass_sample)

summary(slope_anova_mdh)

slope_anova_wt<- aov(data = subset(growth_rates, 
                                     growth_rates$biomass_sample=="WTG"|
                                       growth_rates$biomass_sample=="WTM"),
                       slope ~ biomass_sample)

summary(slope_anova_wt)


# okay so according to these results, 
# for the anova conducted on all four slopes, we have a significant p-value for 
# the biomass_sample variable (p = 3.25e-05). This suggests that the 
# biomass_sample has a significant effect on the average growth rate). 

# If we compare the WT and MDH biomass samples pair wize, we get the following results
# comparing WTG and WTM: p-value = 0.718. The p-value is > 0.05, and therefore is not 
# significant. We can therefore not reject the null hypothesis here. 
# The null hypothesis is that biomass_sample does not affect growth rate. 

# Same goes for the comparison of the mdh biomass samples: 
# the p-value is 0.0677 which is not < 0.05. We can therefore not reject the 
# null hypothesis. 

# Checking the growth rate between WT and MDH? 
growth_rates

wt_mdh_anova <- aov(data = growth_rates, slope ~ strain)

summary(wt_mdh_anova)

# We compare the slopes of WT to the slopes of MDH, and the p-value is 2.95e-06
# This means that there seems to be a significant difference in growth rate 
# between the two strains, but not within the strain cultivated in different medium. 

# Laster inn excel-fil: 
cdw_test <- read_excel("cdw_measurements_test.xlsx")


# removing the first triplicate of mdhg and mdhm

cdw_test <- cdw_test[-c(40,47,54,61,68,75),]


cdw_test$biomass_sample <- factor(cdw_test$biomass_sample, 
                                  levels = c("WTG", "WTM", "MDHG", "MDHM"))

growth_rate_regression_plot <- ggplot(data = cdw_test, aes(x=time_h, y=log_mg_mL, colour=biomass_sample)) +
  geom_point() + theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_smooth(method='lm') + 
  xlab("Time (h)") + ylab("Log(CDW (mg/mL))") + labs(colour = "Biomass \n sample") +
  labs(title = "A") +
  theme(plot.title = element_text(face = "bold")) 
  

growth_rate_regression_plot

ggsave(filename = "growth_rate_regression.pdf", 
       width = 7, 
       height = 4)

# plotting growth curves based on the CDW triplicates: 
cdw_time_plot <- ggplot(data = cdw_test, aes(x=time_h, y=mg_mL, colour=biomass_sample)) +
  geom_point() + theme_minimal() + 
  geom_smooth(method = lm, formula = y ~ exp(x)) +
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  xlab("Time (h)") + ylab("CDW (mg/mL)") + labs(colour = "Biomass \n sample") +
  labs(title = "A") +
  theme(plot.title = element_text(face = "bold")) 

cdw_time_plot

ggsave(filename = "cdw_over_time.pdf", 
       width = 7, 
       height = 4)

od_plot
wtg_all <- lm(formula = log_mg_mL ~ time_h,
            data = subset(cdw_test, biomass_sample=="WTG"))

wtg_all$coefficients
summary(wtg_all)$r.squared

wtm_all <- lm(formula = log_mg_mL ~ time_h,
              data = subset(cdw_test, biomass_sample=="WTM" & 
                              time_h > 3.3))

wtm_all$coefficients
summary(wtm_all)$r.squared

mdhg_all <- lm(formula = log_mg_mL ~ time_h,
              data = subset(cdw_test, biomass_sample=="MDHG" & 
                              time_h > 5.6))

mdhg_all$coefficients
summary(mdhg_all)$r.squared


mdhm_all <- lm(formula = log_mg_mL ~ time_h,
               data = subset(cdw_test, biomass_sample=="MDHM" &
                               time_h > 6))

mdhm_all$coefficients
summary(mdhm_all)$r.squared


wtg_all$coefficients  # f??r: -0.9873489   0.3567489. n??: -0.9788489 0.3545042 
wtm_all$coefficients  # f??r: -1.2468142   0.3462991. n??: -1.5449611   0.4183334 
mdhg_all$coefficients  # f??r: -0.8024873   0.1268505. n??: 
mdhm_all$coefficients  # 0.1857492, 

summary(wtg_all) # sd 0.02763, R2 = 0.9124. N??: R2 = 0.8532
summary(wtm_all) # sd 0.04776, R2 = 0.7345
summary(mdhg_all) # sd 0.02618, R2 = 0.5947
summary(mdhm_all) # sd 0.02446, R2 = 0.78

# The R2 is meaningful for all four regressions. 
# also the p-value for the F-statistic is significant for all four models 

# Comparing models using Anova: 

# comparing wtg and wtm: 
aov(wtg_all, wtm_all)  # error message because the models are based on data
# sets of different size

?anova  # this can only be used if the models are fitted to the same dataset, 
# which they are not. 

group <- c("WTG", "WTM", "MDHG", "MDHM")
mean <- c(0.3567489, 0.3462991, 0.1268505, 0.1857492)
sd <- c(0.02763,0.04776,0.02618,0.02446)
n <- c(3,3,3, 3)
models <- data.frame(group, mean, sd, n)

?ind.oneway.second()

with(models,
     ind.oneway.second(models$mean, # Mean
                       models$sd, # Standard deviation
                       models$n, # Size of group
                       sig.level = 0.05)) # Alpha of the test

# Now get your critical F value
Fcrit <- qf(0.95, # Level of significance
            df1=3, # Numerator degrees of freedom, Between df
            df2=8) # Denominator degrees of freedom, Within df 

# the critical value is 4.066181 (Fcrit). The F value that we got was 36.877
# So it seems like the means are significantly different. 

# However, we want to see if WTG and WTM are different, 
# and if MDHG and MDHM are different. 
# Furthermore, if WT and MDH are different. 

wtg_wtm_anova <- with(subset(models, models$group=="WTG" | models$group=="WTM"),
     ind.oneway.second(mean, # Mean
                       sd, # Standard deviation
                       n, # Size of group
                       sig.level = 0.05)) # Alpha of the test

wtg_wtm_anova$anova.table
# Now get your critical F value
Fcrit <- qf(0.95, # Level of significance
             df1=1, # Numerator degrees of freedom, Between df
             df2=4) # Denominator degrees of freedom, Within df

# the critical value must be larger than 7.708647. We got a critical value of 
# 0.108. According to these results, the growth rate of WTG is not significantly 
# different from that of WTM 

mdhg_mdhm_anova <- with(subset(models, models$group=="MDHG" | models$group=="MDHM"),
     ind.oneway.second(mean, # Mean
                       sd, # Standard deviation
                       n, # Size of group
                       sig.level = 0.05)) # Alpha of the test

mdhg_mdhm_anova$anova.table
# Now get your critical F value
Fcrit <- qf(0.95, # Level of significance
            df1=1, # Numerator degrees of freedom, Between df
            df2=4) # Denominator degrees of freedom, Within df

# We get F value 8.107, and the critical value is 7.708647. 
# So the F-value is larger than the critical value. That means that 
# there is a significant difference between the growth rate of MDHG and MDHM

# Now we want to see if there is a significant difference in growth rate 
# between WT and MDH

cdw_test$biomass_sample <- factor(cdw_test$biomass_sample, 
                                  levels = c("WTG", "WTM", "MDHG", "MDHM"))

cdw_test$strain <- factor(cdw_test$strain, 
                          levels = c("WT", "MDH"))

wt_vs_mdh_regression_plot <- ggplot(data = cdw_test, aes(x=time_h, y=log_mg_mL, colour=strain)) +
  geom_point() + theme_minimal() + 
  scale_fill_manual(values = c("#100000", "#0072B2")) + 
  scale_colour_manual(values = c("#100000", "#0072B2")) + 
  labs(title = "A") +
  theme(plot.title = element_text(face = "bold")) +
  geom_smooth(method='lm') +
  xlab("Time (h)") + ylab("Log(CDW (mg/mL))") + labs(colour="Strain")



wt_vs_mdh_regression_plot

ggsave(filename = "WT_and_MDH_growth_rate_regression.pdf", 
       width = 7, 
       height = 4)

wt <- lm(formula = log_mg_mL ~ time_h,
              data = subset(cdw_test, strain=="WT"))


mdh <- lm(formula = log_mg_mL ~ time_h,
         data = subset(cdw_test, strain=="MDH"))


summary(wt)  # growth rate: 0.36300, sd: 0.04877, r2: 0.5996

summary(mdh)  # growth rate: 0.11630, sd: 0.02882, r2:0.3239

# creating a new df: 

groups <- c("WT", "MDH")
mean <- c(0.36300,0.11630)
sd <- c(0.04877, 0.02882)
n <- c(3,3)

df <- data.frame(groups, mean, sd, n)



wt_mdh_anova <- with(df,
     ind.oneway.second(mean, # Mean
                       sd, # Standard deviation
                       n, # Size of group
                       sig.level = 0.05)) # Alpha of the test

wt_mdh_anova$anova.table

# Now get your critical F value
Fcrit <- qf(0.95, # Level of significance
            df1=1, # Numerator degrees of freedom, Between df
            df2=4) # Denominator degrees of freedom, Within df

sessionInfo()

# we get F value 56.895, which is larger than the critical value 7.708647

# Alternatively we check if Mdhm and mdhg are different from WTG: 

wtg_mdhm_anova <- with(subset(models, models$group=="MDHM" | models$group=="WTG"),
     ind.oneway.second(mean, # Mean
                       sd, # Standard deviation
                       n, # Size of group
                       sig.level = 0.05)) # Alpha of the test

wtg_mdhm_anova$anova.table

# Now get your critical F value
Fcrit <- qf(0.95, # Level of significance
            df1=1, # Numerator degrees of freedom, Between df
            df2=4) # Denominator degrees of freedom, Within df

wtg_mdhg_anova <- with(subset(models, models$group=="MDHG" | models$group=="WTG"),
                       ind.oneway.second(mean, # Mean
                                         sd, # Standard deviation
                                         n, # Size of group
                                         sig.level = 0.05)) # Alpha of the test

wtm_mdhg_anova <- with(subset(models, models$group=="MDHG" | models$group=="WTM"),
                       ind.oneway.second(mean, # Mean
                                         sd, # Standard deviation
                                         n, # Size of group
                                         sig.level = 0.05)) # Alpha of the test

wtm_mdhm_anova <- with(subset(models, models$group=="MDHM" | models$group=="WTM"),
                       ind.oneway.second(mean, # Mean
                                         sd, # Standard deviation
                                         n, # Size of group
                                         sig.level = 0.05)) # Alpha of the test

# we get F value of 109.44 and 48.703 (MDHG vs WTG, MDHG vs WTM)

# 26.857 and 64.421 for the two other ones. 
# Okay so, according to our tests, there is a significant difference in growth 
# rate between the two strains: WT grows significantly faster than mutant. 
# There is no significant difference between the two WTs, 
# however, MDH in methanol grows significantly faster than MDH in glucose. 

# plotting the new growth rates with new sd: 

growth_rates_final <- read_excel("growth_rates_final.xlsx")

# sorting the data as usual: 

growth_rates_final$biomass_sample <- factor(growth_rates_final$biomass_sample, 
                                            levels = c("WTG", "WTM", "MDHG", "MDHM"))

growth_rates_final$strain <- factor(growth_rates_final$strain, 
                                            levels = c("WT", "MDH"))

growth_rates_final_plot <- ggplot(data=growth_rates_final, aes(x=biomass_sample, y = growth_rate, fill=biomass_sample)) + 
  geom_col(col="black", width = 0.8) + theme_minimal()  +
  theme(legend.position="none") +
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) + 
  geom_errorbar(aes(ymin=growth_rate - sd, ymax = growth_rate + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("Growth rate (h^-1)") +
  geom_text(aes(label=round(growth_rate,2)), vjust = 5.5,fontface = "bold", color="white", size=3.5) +
  labs(title = "B") +
  theme(plot.title = element_text(face = "bold"))

growth_rates_final_plot

ggsave(filename = "growth_rate_final.pdf",
       width = 5, 
       height = 4)

strain <- c("WT", "MDH")
growth_rate <- c(wt$coefficients[2], mdh$coefficients[2])
sd <- c(0.04877, 0.03942)
R2 <- c(0.5996, 0.3202)
growth_rate_strain  <- data.frame(strain, growth_rate, sd, R2)

growth_rate_strain$strain <- factor(growth_rate_strain$strain, 
                                    levels = c("WT", "MDH"))
  
growth_rates_strain_plot <- ggplot(data=growth_rate_strain, aes(x=strain, y = growth_rate, fill=strain)) + 
  geom_col(col="black", width = 0.8) + theme_minimal()  +
  theme(legend.position="none") +
  scale_fill_manual(values = c("#100000", "#0072B2")) + 
  scale_colour_manual(values = c("#100000", "#0072B2")) + 
  geom_errorbar(aes(ymin=growth_rate - sd, ymax = growth_rate + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Strain") + ylab("Growth rate (h^-1)") +
  geom_text(aes(label=round(growth_rate,2)), vjust = 5.5,fontface = "bold", color="white", size=3.5) +
  labs(title = "B") +
  theme(plot.title = element_text(face = "bold"))

growth_rates_strain_plot

wt$coefficients[2]

summary(wt)
summary(mdh)

# Visualizing OD over time next to growth rate: 
?grid.arrange(growth_rate_regression_plot, growth_rates_final_plot, ncol=2, nrow=1)

# writing the combined plot to pdf: 
pdf("regression_and_growth_rates.pdf", width = 8, height = 4)

#grid.arrange(growth_rate_regression_plot, growth_rates_final_plot, ncol=2, nrow=1)

#grid.arrange(arrangeGrob(growth_rate_regression_plot,growth_rates_final_plot, ncol=2, nrow=1))

plot_grid(growth_rate_regression_plot, growth_rates_final_plot,
          rel_widths = c(7,3))


dev.off()

pdf("regression_and_growth_rates_strain.pdf", width = 8, height = 4)

plot_grid(wt_vs_mdh_regression_plot, growth_rates_strain_plot,
          rel_widths = c(7,3))

dev.off()

# Making a plot for gas uptake rates: 
gas_rates <- read_excel("gas_uptake_rates.xlsx")

gas_rates$biomass_sample <- factor(gas_rates$biomass_sample, 
                                   levels = c("WTG", "WTM", "MDHG", "MDHM"))

ggplot(data=gas_rates, aes(x=biomass_sample, y = avg, fill = compound)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  #scale_y_break(c(0,2)) +
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd), width = .3, position = position_dodge(.9)) +
  xlab("Fermentation") + ylab("Uptake rate (mmol/gCDW h)") + labs(fill = "Compound") + 
  scale_fill_manual(values = cbPalette, name = "Compound", labels = c("CO2", "O2")) 
#geom_text(aes(label=round(uptake_rate_mmol_gDW_h, 1), 
# y = ifelse(uptake_rate_mmol_gDW_h < 0, uptake_rate_mmol_gDW_h - sd_uptake_rate - 0.5, uptake_rate_mmol_gDW_h + sd_uptake_rate + 0.5)),
#colour="black", fontface = "bold", size=3.5, position = position_dodge(0.9)) 

ggsave(filename = "gas uptake- and secretion rates.pdf", 
       width = 7, 
       height = 5)

?aov()
oxygen_anova <- aov(formula = uptake_rate ~ biomass_sample, data = subset(gas_rates, 
                    gas_rates$compound == "O2"))

summary(oxygen_anova)

co2_anova <- aov(formula = uptake_rate ~ biomass_sample, data = subset(gas_rates, 
                                                                          gas_rates$compound == "CO2"))

summary(co2_anova)

oxygen_tukey <- TukeyHSD(oxygen_anova)

oxygen_tukey

par(mfrow=c(1,1))
oxygen_tukey_plot <- plot(oxygen_tukey, las = 1)


pdf(file = "o2_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(oxygen_tukey, las = 1)

dev.off()

co2_tukey <- TukeyHSD(co2_anova)

co2_tukey

co2_tukey_plot <- plot(co2_tukey, las = 1) 


pdf(file = "co2_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(co2_tukey, las = 1)

dev.off()

medium_data$biomass_sample <- factor(medium_data$biomass_sample, 
                                     levels = c("WTG", "WTM", "MDHG", "MDHM"))

# ANOVA glucose uptake rate
gluc_aov <- aov(formula = uptake_rate ~ biomass_sample, data = subset(medium_data, 
                                                                          medium_data$compound == "glucose"))
summary(gluc_aov)

gluc_tukey <- TukeyHSD(gluc_aov)

gluc_tukey

plot(gluc_tukey, las = 1)

pdf(file = "gluc_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(gluc_tukey, las = 1)

dev.off()


# ANOVA methanol uptake rate 

meth_aov <- aov(formula = uptake_rate ~ biomass_sample, data = subset(medium_data, 
                                                                      medium_data$compound == "methanol"))
summary(meth_aov)

meth_tukey <- TukeyHSD(meth_aov)

meth_tukey

meth_tukey_plot <- plot(meth_tukey, las = 1)

pdf(file = "meth_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(meth_tukey, las = 1)

dev.off()


# Acetate: 

acetate_aov <- aov(formula = uptake_rate ~ biomass_sample, data = subset(medium_data, 
                                                                      medium_data$compound == "acetate"))
summary(acetate_aov)

acetate_tukey <- TukeyHSD(acetate_aov)

acetate_tukey

acetate_tukey_plot <- plot(acetate_tukey, las = 1)


pdf(file = "acetate_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(acetate_tukey, las = 1)

dev.off()

# glutamic acid: 
glu_aov <- aov(formula = uptake_rate ~ biomass_sample, data = subset(medium_data, 
                                                                         medium_data$compound == "l_glu"))
summary(glu_aov)

glu_tukey <- TukeyHSD(glu_aov)

glu_tukey

par(mfrow=c(1,1))
glu_tukey_plot <- plot(glu_tukey, las = 1)


pdf(file = "glu_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(glu_tukey, las = 1)

dev.off()

# l-trp: 
trp_aov <- aov(formula = uptake_rate ~ biomass_sample, data = subset(medium_data, 
                                                                     medium_data$compound == "l_trp"))
summary(trp_aov)

trp_tukey <- TukeyHSD(trp_aov)

trp_tukey

par(mfrow=c(1,1))
trp_tukey_plot <- plot(trp_tukey, las = 1)


pdf(file = "trp_tukey.pdf",   
    width = 4, 
    height = 4) 

plot(trp_tukey, las = 1)

dev.off()

od_plot

growth_data


# Looking at the amino acid distribtuion across biomass samples
amino_acid_df <- read_excel("amino_acid_distribution.xlsx")

amino_acid_df$biomass_sample <- factor(amino_acid_df$biomass_sample, 
                                       levels = c("WTG", "WTM", "MDHG", "MDHM"))
# Want to see the ANOVA and post hoc results of each amino acid:

aa <- c("Asp","Glu","Asn","His","Ser","Gln","Gly","Arg","Thr","Ala","Tyr","Val","Phe","Ile",
        "Leu","Lys","Cys","Pro","Trp","Met")

par(mfrow=c(2,3))

for (x in aa) {
  print(x)
  x_aov <- aov(formula = g_gProt ~ biomass_sample, data = subset(amino_acid_df, 
                                                         amino_acid_df$amino_acid == x))
  print(summary(x_aov))
  
  x_Tukey <- TukeyHSD(x_aov)
  title = x
  
  plot(x_Tukey, las = 1)  
  title(main = x)
}

?plot


# Plotting the predicted growth rates when all the models have the 
# same default medium 

model <- c("iBsu1147", "WTG", "WTM", "MDHG", "MDHM")
p_growth_rate <- c(0.216, 0.224, 0.219, 0.224, 0.224)
df <- data.frame(model, p_growth_rate)

df$model <- factor(df$model, 
                   levels = c("iBsu1147", "WTG", "WTM", "MDHG", "MDHM"))

# Making a bar graph to visualize the calculated uptake and secretion rates 
ggplot(data=df, aes(x=model, y = p_growth_rate, fill = model)) + 
  geom_col(position=position_dodge(), col="black", width = 0.8)+ theme_minimal() + 
  xlab("Model") + ylab("Predicted growth rate (h^-1)") + labs(fill = "Model") +
  scale_fill_manual(values = c("#CC79A7", "#999999", "#E69F00","#56B4E9","#009E73","#F0E442","#0072B2")) + 
  expand_limits(y = c(0.0, 0.3)) +
  geom_text(
    aes(label = p_growth_rate), 
    position = position_dodge(0.9),
    col = "white",
    vjust = 2,
    fontface = "bold",
    size = 3.5)
  

ggsave(filename = "predicted growth rates iBsu1147 to mdhm.pdf", 
       width = 7, 
       height = 4)


# Want to visualize the pairwize comparison of predicted growth rate for each
# biomass sample 

growth_rates_compared <- read_excel("compared_growth_rates.xlsx")

growth_rates_compared$model <- factor(growth_rates_compared$model, 
                                      levels= c("iBsu1147_wtg", "wtg", "exp_wtg", "iBsu1147_wtm", "wtm", "exp_wtm", 
                                                "iBsu1147_mdhg", "mdhg", "exp_mdhg", "iBsu1147_mdhm", "mdhm", "exp_mdhm")
)

ggplot(data=growth_rates_compared, aes(x=model, y = growth_rate, fill = model)) + 
  geom_col(position=position_dodge(), col="black", width = 0.8)+ theme_minimal() + 
  xlab("Model") + ylab("Growth rate (h^-1)") + labs(fill = "Model") +
  scale_x_discrete(labels=c("iBsu1147_wtg" = "iBsu1147", "wtg" = "WTG", "exp_wtg" = "Exp.", 
                            "iBsu1147_wtm" = "iBsu1147", "wtm" = "WTM", "exp_wtm" = "Exp.", 
                            "iBsu1147_mdhg" = "iBsu1147", "mdhg" = "MDHG", "exp_mdhg" = "Exp.",
                            "iBsu1147_mdhm" = "iBsu1147", "mdhm" = "MDHM", "exp_mdhm" = "Exp.")) + 
  scale_fill_manual(values = c("#999999","#999999","#999999", "#E69F00","#E69F00","#E69F00",
                               "#56B4E9","#56B4E9","#56B4E9","#009E73","#009E73","#009E73","#F0E442","#0072B2")) +
  geom_errorbar(aes(ymin=growth_rate - SD, ymax = growth_rate + SD), 
                width = .3, position = position_dodge(.9)) + 
  theme(axis.text.x = element_text(angle=90))
  
ggsave(filename = "predicted growth rate experimental secretion and uptake rates.pdf",
       device = "pdf",
       height = 5, 
       width = 8)

# we rather split this figure in two, and we can visualize them next to each other
# So first we want to show that the predicted growth rates are similar
# then we want to compare the predicted growth rates to the experimentally measured rates

growth_rates_compared

growth_rates_compared <- read_excel("compared_growth_rates.xlsx", sheet = "Sheet2")

growth_rates_compared$biomass_sample <- factor(growth_rates_compared$biomass_sample, 
                                               levels = c("WTG", "WTM", "MDHG", "MDHM"))

growth_rates_compared$variable <- factor(growth_rates_compared$variable, 
                                         levels = c("bof", "mbof", "exp"))

# this is still a plot showing all three
bof_mbof_exp <- ggplot(data=growth_rates_compared, aes(x=biomass_sample, y = growth_rate, fill = variable)) + 
  geom_col(position=position_dodge(), col="black", width = 0.8)+ theme_minimal() + 
  xlab("Fermentation") + ylab("Growth rate (h^-1)") + labs(fill = "Variable") +
  scale_fill_manual(values = cbPalette) +
  geom_errorbar(aes(ymin=growth_rate - sd, ymax = growth_rate + sd), 
                width = .3, position = position_dodge(.9)) 

bof_mbof_exp

ggsave(filename = "predicted growth rates bof mbof exp.pdf",
       device = "pdf",
       height = 5, 
       width = 8)

# let's compare only the predicted growth rates: 

# this is still a plot showing all three
bof_mbof <- ggplot(data=subset(growth_rates_compared, 
                   growth_rates_compared$variable == "bof" |
                     growth_rates_compared$variable == "mbof"), aes(x=biomass_sample, y = growth_rate, fill = variable)) + 
  geom_col(position=position_dodge(), col="black", width = 0.8)+ theme_minimal() + 
  xlab("Biomass sample") + ylab("Growth rate (h^-1)") + labs(fill = "Variable") +
  scale_fill_manual(values=c("#D55E00", "#0072B2", "#F0E442")) +
  geom_errorbar(aes(ymin=growth_rate - sd, ymax = growth_rate + sd), 
                width = .3, position = position_dodge(.9)) 
bof_mbof

ggsave(filename = "predicted growth rate bof mbof.pdf",
       device = "pdf",
       height = 5, 
       width = 8)

par(mfrow = c(2,1))

bof_mbof <- bof_mbof + labs(title = "A") 
bof_mbof_exp <- bof_mbof_exp + labs(title = "B")

plot_grid(bof_mbof,bof_mbof_exp)
ggsave(filename = "predicted growth rate side by side.pdf",
       device = "pdf",
       height = 5, 
       width = 8)


# We want to know if the values of the iBsu1147 BOF
# could have been optained with the values that we have measured. 

# One sample t-test is used to compare a measured mean to 
# a theoretical mean. In this case, the BOF coefficients are the 
# theoretical means, while we have measured means

prot <- read_excel("biomass_data_for_anova.xlsx", sheet = "prot")

# x is the vector of our data 
# mu is the theoretical mean
# we choose two sided, because the new mean could be higher or lower
# (one sided would mean that it could only be higher or lower)

# Test for iBsu1147 prot vs. WTG prot
t.test(x=subset(prot, prot$biomass_sample =="WTG" & prot$variable=="n")$prot_content, 
       mu = 0.528, alternative = "two.sided")  # not significant

# Test for iBsu1147 prot vs. WTM prot
t.test(x=subset(prot, prot$biomass_sample =="WTM" & prot$variable=="n")$prot_content, 
       mu = 0.528, alternative = "two.sided")  # not significant

# Test for iBsu1147 prot vs. MDHG prot
t.test(x=subset(prot, prot$biomass_sample =="MDHG" & prot$variable=="n")$prot_content, 
       mu = 0.528, alternative = "two.sided")  # not significant

# Test for iBsu1147 prot vs. MDHM prot
t.test(x=subset(prot, prot$biomass_sample =="MDHM" & prot$variable=="n")$prot_content, 
       mu = 0.528, alternative = "two.sided")  # p-value = 0.05312

# Now we test RNA content: 

rna <- read_excel("biomass_data_for_anova.xlsx", sheet = "rna")

rna_wtg <- t.test(x=subset(rna, rna$biomass_sample =="WTG" & rna$variable=="n")$rna_content, 
       mu = 0.0655, alternative = "two.sided")  # significant

rna_wtg$p.value

t.test(x=subset(rna, rna$biomass_sample =="WTM" & rna$variable=="n")$rna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(rna, rna$biomass_sample =="MDHG" & rna$variable=="n")$rna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(rna, rna$biomass_sample =="MDHM" & rna$variable=="n")$rna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

# Now we test DNA content: 

dna <- read_excel("biomass_data_for_anova.xlsx", sheet = "dna")

t.test(x=subset(dna, dna$biomass_sample =="WTG" & dna$variable=="n")$dna_content, 
       mu = 0.0655, alternative = "two.sided")  # significant

t.test(x=subset(dna, dna$biomass_sample =="WTM" & dna$variable=="n")$dna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(dna, dna$biomass_sample =="MDHG" & dna$variable=="n")$dna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(dna, dna$biomass_sample =="MDHM" & dna$variable=="n")$dna_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

# Now we test lipid content: 

lipid <- read_excel("biomass_data_for_anova.xlsx", sheet = "lipid")

t.test(x=subset(lipid, lipid$biomass_sample =="WTG" & lipid$variable=="n")$lipid_content, 
       mu = 0.0655, alternative = "two.sided")  # significant

t.test(x=subset(lipid, lipid$biomass_sample =="WTM" & lipid$variable=="n")$lipid_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(lipid, lipid$biomass_sample =="MDHG" & lipid$variable=="n")$lipid_content, 
       mu = 0.0655, alternative = "two.sided")  # sig

t.test(x=subset(lipid, lipid$biomass_sample =="MDHM" & lipid$variable=="n")$lipid_content, 
       mu = 0.0655, alternative = "two.sided")  # sig


# we try to visualize the values that have actually been used for 
# one-sample t-test: 

normalized_values <- read_excel("biomass_data_for_anova.xlsx", sheet="avg_normalized")

normalized_values$biomass_sample <- factor(normalized_values$biomass_sample, 
                            levels = c("iBsu1147", "WTG", "WTM", "MDHG", "MDHM"))

normalized_values$component <- factor(normalized_values$component, 
                                           levels = c("prot" , "lipid","rna", "dna"))


ggplot(data=normalized_values, aes(x=biomass_sample, y = avg, fill = component)) + 
  geom_col(position=position_dodge(), col="black")+ theme_minimal() + 
  scale_fill_manual(values = cbPalette) + 
  scale_y_break(c(0.1,0.45)) + 
  geom_errorbar(aes(ymin=avg - sd, ymax = avg + sd), 
                width = .3, position = position_dodge(.9)) +
  xlab("Biomass sample") + ylab("g/gDW") + labs(fill = "Biomass \n component") 
  

ggsave(filename = "biomass_all_scaled.pdf",
       device = "pdf",
       height = 5, 
       width = 8)


# Visualizing the measured biomass composition


