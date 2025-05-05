# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`

install.packages("foreign")
install.packages("data.table")
library(data.table)
library(foreign)
hts <- data.table(read.spss("~/2025_MethodsII/07-lab-assignment-2025-Elliott-Co/datasets/HTS.household.10regions.sav",to.data.frame = T))

# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results

library(ggplot2)

hts[, singlefamily := ifelse(htype == "single family detached", 1, 0)]

hts[, singlefamily := factor(singlefamily, labels = c("Other", "Single Family Detached"))]

ggplot(data = hts, aes(x = hhincome)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ singlefamily) +
  labs(title = "Household Income by Housing Type",
       x = "Household Income",
       y = "Count",
       subtitle = "Faceted by Single Family vs. Other") +
  theme_minimal()



ttest_hhincome <- hts[, t.test(hhincome ~ singlefamily)]
print(ttest_hhincome)

hts[, .(
  mean_hhincome = mean(hhincome, na.rm = TRUE),
  sd_hhincome = sd(hhincome, na.rm = TRUE),
  N = .N
), by = singlefamily]


df <- ttest_hhincome$parameter
tstat <- ttest_hhincome$statistic



# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

unique(hts$region)
sa <- hts[hts$region == "San Antonio, TX", ]


median_jobpop <- median(sa$jobpop, na.rm = TRUE)


sa[, jobpop_group := ifelse(jobpop >= median_jobpop, "High", "Low")]


two_tailed_t_test <- sa[, t.test(lnvmt ~ jobpop_group)]
two_tailed_t_test


# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.

# Step 1: Boxplot to check for outliers in the lnvmt variable
ggplot(data = sa, aes(x = income_cat, y = lnvmt)) +
  geom_boxplot() +
  ggtitle("Boxplot of lnvmt by Income Category - Checking for Outliers") +
  theme_minimal()

# Step 2: Removing outliers
boxplot_sa <- boxplot(sa$lnvmt ~ sa$income_cat)

# Extract outliers
outliers <- boxplot_sa$out

# Create a new data.table without outliers
sa_clean <- sa[!lnvmt %in% outliers]

# Boxplot again after removing outliers
boxplot(sa_clean$lnvmt ~ sa_clean$income_cat)

# Step 3: Checking normality assumption for lnvmt
hist(sa_clean$lnvmt)  # A histogram to check normality

# Step 4: Check for homogeneity of variances
bartlett.test(lnvmt ~ income_cat, data = sa_clean)

# Step 5: Run one-way ANOVA
fit <- aov(lnvmt ~ income_cat, data = sa_clean)
summary(fit)

# Step 6: Tukey's Post-Hoc Test
TukeyHSD(fit)

# Plot the Tukey HSD results
plot(TukeyHSD(fit))


# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---------------------------------------------------------------------------------------------------------
# ---- Libraries ----
library(sf)
library(raster)
library(tidyverse)
library(exactextractr)
library(ggplot2)
library(broom)

# ---- Load and Prepare Data ----
waterways <- st_read("C:/Users/witht/Documents/2025_MethodsII/Final/StormChannels/StormChannels.shp")
heat_raster <- raster("C:/Users/witht/Documents/2025_MethodsII/Final/Heat/HeatC.tif")

# Reproject waterways to match raster CRS
waterways <- st_transform(waterways, crs = crs(heat_raster))

# ---- Filter and Buffer by Channel Type ----
buffers <- list(
  Concrete = st_buffer(waterways %>% filter(ChannelTyp == "Concrete"), 500),
  Earthen  = st_buffer(waterways %>% filter(ChannelTyp == "Earthen"),  500),
  Natural  = st_buffer(waterways %>% filter(ChannelTyp == "Natural"),  500)
)

# ---- Extract Raster Values within Buffers ----
extract_heat_df <- function(buffer, label) {
  buffer <- st_transform(buffer, crs = crs(heat_raster))  # ensure match
  values <- exact_extract(heat_raster, buffer)
  values <- values[sapply(values, function(x) is.data.frame(x) && "value" %in% names(x))]
  df <- bind_rows(lapply(values, function(x) data.frame(value = x$value)))
  df$Type <- label
  return(df)
}

heat_all <- bind_rows(
  extract_heat_df(buffers$Concrete, "Concrete"),
  extract_heat_df(buffers$Earthen,  "Earthen"),
  extract_heat_df(buffers$Natural,  "Natural")
)

# ---- Clean and Summarize ----
heat_all$Type <- factor(heat_all$Type)
heat_all <- heat_all %>% drop_na()

summary_stats <- heat_all %>%
  group_by(Type) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )
print(summary_stats)

#---------------------------------------------------------------------------------------------------------



heat_subset <- heat_all %>% filter(Type %in% c("Concrete", "Natural"))
t_test_result <- t.test(value ~ Type, data = heat_subset)
print(t_test_result)


anova_model <- aov(value ~ Type, data = heat_all)
summary(anova_model)


# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.



