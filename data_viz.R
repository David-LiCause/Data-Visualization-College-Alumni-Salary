
library(tidyverse)
library(stringr)
library(ggthemes)
library(Cairo)
library(scales)
library(ggridges)

deg <- read.csv("degrees-that-pay-back.csv", header=T, stringsAsFactors = F)
reg <- read.csv("salaries-by-region.csv", header=T, stringsAsFactors = F)
typ <- read.csv("salaries-by-college-type.csv", header = T, stringsAsFactors = F)

convert_dollars <- function(dollars_str) {
  dollars_str <- str_replace_all(dollars_str, fixed("$"), "")
  dollars_str <- str_replace_all(dollars_str, ",", "")
  dollars_str <- str_replace_all(dollars_str, fixed(".00"), "")
  dollars_str <- as.numeric(dollars_str)
  return(dollars_str)
}

deg <- deg %>%
  mutate(med_salary_start = convert_dollars(Starting.Median.Salary), 
         med_salary_mid = convert_dollars(Mid.Career.Median.Salary)) %>%
  select(Undergraduate.Major, med_salary_start, med_salary_mid) %>%
  arrange(desc(med_salary_mid)) 
deg$Undergraduate.Major <- factor(deg$Undergraduate.Major, levels = c(deg$Undergraduate.Major))

deg_top_10 <- deg %>%
  slice(1:10)

options(repr.plot.width=5, repr.plot.height=4)

ggplot(deg_top_10, aes(x=Undergraduate.Major, y=med_salary_mid, fill=med_salary_mid)) + 
  geom_bar(stat="identity") +
  ggtitle("Median Mid-Career Salary by Degree (Top 10)") +
  labs(x="", y="Median Mid-Career Salary ($)") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low="#ebf442",high="#00FF00") + 
  guides(fill=FALSE) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90)) 

deg_bottom_10 <- deg %>%
  slice((nrow(deg)-10):nrow(deg))

ggplot(deg_bottom_10, aes(x=Undergraduate.Major, y=med_salary_mid, fill=med_salary_mid)) + 
  geom_bar(stat="identity") +
  ggtitle("Median Mid-Career Salary by Degree (Bottom 10)") +
  labs(x="", y="Median Mid-Career Salary ($)") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low="#f44242",high="#ebf442") + 
  guides(fill=FALSE) +
  expand_limits(y = max(deg$med_salary_mid)) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90))

deg <- deg %>%
  mutate(salary_growth = med_salary_mid - med_salary_start) %>%
  arrange(desc(salary_growth))
deg$Undergraduate.Major <- factor(deg$Undergraduate.Major, levels = as.character(deg$Undergraduate.Major))

salary_growth_top_10 <- deg %>%
  slice(1:10)

ggplot(salary_growth_top_10, aes(x=Undergraduate.Major, y=salary_growth, fill=salary_growth)) + 
  geom_bar(stat="identity") +
  ggtitle("Salary Growth by Degree (Top 10)") +
  labs(x="", y="Salary Growth (Starting vs. Mid-Career)") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low="#ebf442",high="#00FF00") + 
  guides(fill=FALSE) +
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11), 
        axis.text.x = element_text(angle = 90))

salary_growth_bottom_10 <- deg %>%
  slice((nrow(deg)-10):nrow(deg))

ggplot(salary_growth_bottom_10, aes(x=Undergraduate.Major, y=salary_growth, fill=salary_growth)) + 
  geom_bar(stat="identity") +
  ggtitle("Salary Growth by Degree (Bottom 10)") +
  labs(x="", y="Salary Growth (Starting vs. Mid-Career)") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low="#f44242",high="#ebf442") + 
  guides(fill=FALSE) +
  expand_limits(y = max(deg$salary_growth)) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90))

# Compare salaries across schools
typ <- typ %>%
  mutate(med_salary_start = convert_dollars(Starting.Median.Salary), 
         med_salary_mid = convert_dollars(Mid.Career.Median.Salary)) %>%
  select(School.Name, School.Type, med_salary_start, med_salary_mid)

ggplot(typ, aes(x=med_salary_start, y=med_salary_mid, color=School.Type)) +
  geom_point(size=2) +
  ggtitle("Salary by School Type") +
  labs(x="Starting Median Salary ($)", y="Mid-Career Median Salary ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_minimal() + 
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11))

ggplot(typ, aes(x=School.Type, y=med_salary_mid)) + 
  geom_violin(aes(fill=School.Type, color=School.Type)) +
  ggtitle("Median Salaries by School Type") +
  labs(x="", y="Mid-Career Median Salary ($)") +
  scale_y_continuous(labels = comma) +
  theme_minimal() + 
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11))

ggplot(typ, aes(x=med_salary_mid, fill=School.Type, color=School.Type)) + 
  geom_density(alpha=.55) +
  ggtitle("Median Salary Distributions by School Type") +
  labs(x="Mid-Career Median Salary ($)", y="Density") +
  scale_x_continuous(labels = comma) +
  theme_minimal() + 
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11), 
        axis.text.y = element_blank())

reg <- reg %>%
  mutate(med_salary_start = convert_dollars(Starting.Median.Salary), 
         med_salary_mid = convert_dollars(Mid.Career.Median.Salary)) %>%
  select(School.Name, Region, med_salary_start, med_salary_mid)

ggplot(reg, aes(x=Region, y=med_salary_mid)) + 
  geom_violin(aes(fill=Region, color=Region)) +
  ggtitle("Median Salaries by Region") +
  labs(x="", y="Mid-Career Median Salary ($)") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11)) 

ggplot(reg, aes(x=med_salary_start, y=med_salary_mid, color=Region)) +
  geom_point(size=2) +
  ggtitle("Salary by Region") +
  labs(x="Starting Median Salary ($)", y="Mid-Career Median Salary ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~ Region) +
  theme(plot.title = element_text(size=18, hjust=.5), 
        axis.title = element_text(size=11), 
        axis.text = element_text(size=11))