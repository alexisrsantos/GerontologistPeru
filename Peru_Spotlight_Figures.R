library(wpp2019)
library(ggplot2)
library(dplyr)
library(tidyr)

data(popM)
data(popF)
data(popMprojMed)
data(popFprojMed)

# Historical years (1950-2020) from popM/popF
hist_years <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020")
proj_years <- c("2030")

# Get Puerto Rico code
data(UNlocations)
puerto_rico_code <- UNlocations %>% filter(name == "Peru") %>% pull(country_code)

# Historical data
male_hist <- popM %>%
  filter(country_code == puerto_rico_code) %>%
  pivot_longer(cols = all_of(hist_years), names_to = "Year", values_to = "Population") %>%
  mutate(Sex = "Male")

female_hist <- popF %>%
  filter(country_code == puerto_rico_code) %>%
  pivot_longer(cols = all_of(hist_years), names_to = "Year", values_to = "Population") %>%
  mutate(Sex = "Female")

# Projected data (median projections)
male_proj <- popMprojMed %>%
  filter(country_code == puerto_rico_code) %>%
  pivot_longer(cols = all_of(proj_years), names_to = "Year", values_to = "Population") %>%
  mutate(Sex = "Male")

female_proj <- popFprojMed %>%
  filter(country_code == puerto_rico_code) %>%
  pivot_longer(cols = all_of(proj_years), names_to = "Year", values_to = "Population") %>%
  mutate(Sex = "Female")

# Combine all
male_hist$Sex="Male"
female_hist$Sex="Female"
population_data2 <- bind_rows(male_hist, female_hist)

population_data <- bind_rows(male_hist, female_hist, male_proj, female_proj) %>%
  mutate(Age = factor(age, levels = unique(age)),
         Population = Population * 1000)  # Convert to actual count

years <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020", "2030")

# Install and load required packages
#install.packages(c("wpp2019", "ggplot2", "dplyr", "tidyr"))
library(wpp2019)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load WPP data
data(popM)
data(popF)
data(popMprojMed)
data(popFprojMed)
data(UNlocations)

# Country code for Puerto Rico
puerto_rico_code <- UNlocations %>%
  filter(name == "Peru") %>%
  pull(country_code)

# Years to include
hist_years <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020")
proj_years <- c("2030")
all_years <- c(hist_years, proj_years)

# Helper function to process population data
process_data <- function(df, years, sex_label) {
  df %>%
    filter(country_code == puerto_rico_code) %>%
    pivot_longer(cols = all_of(years), names_to = "Year", values_to = "Population") %>%
    mutate(Sex = sex_label)
}

# Process historical and projected data
male_hist <- process_data(popM, hist_years, "Male")
female_hist <- process_data(popF, hist_years, "Female")
male_proj <- process_data(popMprojMed, proj_years, "Male")
female_proj <- process_data(popFprojMed, proj_years, "Female")

# Combine all data
population_data <- bind_rows(male_hist, female_hist, male_proj, female_proj) %>%
  mutate(
    Age = factor(age, levels = unique(age)),
    Population = Population * 1000  # Convert from thousands
  )

population_data$Sex2<-ifelse(population_data$Sex=="Male","Hombres","Mujeres")

# Plot: Faceted population pyramid by year
ggplot(population_data, aes(x = Age,
                            y = ifelse(Sex2 == "Hombres", -Population/1000, Population/1000),
                            fill = Sex)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  labs(x = "Edad", y = "Poblacion (en miles)",
    fill = "Sexo"
  ) +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_manual(values = c("Hombres" = "#1f77b4", "Mujeres" = "#ff7f0e")) +
  theme_minimal(base_size = 12) +
  theme(legend.position="bottom",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 7))

population_data2<-subset(population_data,
                  Year %in% c(1950,2000,2020,2030))

F1<-ggplot(population_data2, aes(x = Age,
                            y = ifelse(Sex == "Male", -Population/1000, Population/1000),
                            fill = Sex)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  scale_x_discrete(breaks = levels(population_data2$Age)[seq(1, length(levels(population_data2$Age)), by = 4)]) +
  labs(x = "Age", y = "Population (in thousands)",
       fill = "Sex"
  ) +
  facet_wrap(~Year, ncol = 2) +
  #scale_fill_manual(values = c("Male" = "steelblue", "Female" = "firebrick")) +
  scale_fill_discrete(breaks=c("Male","Female"),type=c("steelblue","firebrick"))+
  theme_minimal(base_size = 12) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 7))

F1

ggsave(
  "figure1.png",
  plot = F1,
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

library(readxl)
dataper <- read_excel("Peru_Data.xlsx")

pop_long <- dataper %>%
  select(Year, Pop0_19, Pop_2064, Pop_65plus) %>%
  pivot_longer(cols = c(Pop0_19, Pop_2064, Pop_65plus),
               names_to = "AgeGroup",
               values_to = "Population")

pop_long$AgeGroup2<-ifelse(pop_long$AgeGroup=="Pop0_19","3",
                    ifelse(pop_long$AgeGroup=="Pop_2064","2","1"))


#pop_long$AgeGroup2<-reorder(pop_long$AgeGroup,new.order=c("Pop0_19","20-64","65 +"))

A<-ggplot(pop_long, aes(x = Year, y = Population, fill = factor(AgeGroup2))) +
  geom_area(alpha = 0.8, size = 0.2, colour = "white") +
  scale_fill_discrete(breaks=c("3","2","1"),
                      labels = c("0-19 +", "20-64", "65 +"),
                      type=c("steelblue","#abd9e9","firebrick")) +
  labs(title = "Population by age groups",
       x = element_blank(), y = "Population (in thousands)") +
  theme_minimal()+
  theme(legend.position="bottom",legend.title=element_blank())

A

B<-ggplot(dataper, aes(x = Year)) +
  geom_line(aes(y = CBR_per1000, colour = "CBR"), size = 1.2) +
  geom_line(aes(y = CDR_per1000, colour = "CDR"), size = 1.2) +
  scale_colour_manual(values = c("CBR" = "steelblue", "CDR" = "firebrick")) +
  labs(title = "Crude Birth and Death Rates",
       x = element_blank(), y = "Per 1,000 Population", colour = "") +
  theme_minimal()+ylim(0,50)+theme(legend.position = "bottom")

B

C<-ggplot(dataper, aes(x = Year)) +
  geom_line(aes(y = ChildDepRatio, colour = "Children Dependency Ratio"), size = 1.2) +
  geom_line(aes(y = OldDepRatio, colour = "Old-Age Dependency Ratio"), size = 1.2) +
  scale_y_continuous(breaks =c(0,25,50,75,100,125,150)) +
  scale_x_continuous(breaks =c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  scale_colour_manual(values = c("Children Dependency Ratio" = "steelblue", "Old-Age Dependency Ratio" = "firebrick")) +
  labs(title = "Dependency Ratios",
       x = "Year", y = "Per 100 age 20-64", colour = "") +
  theme_minimal()+
  theme(legend.position="bottom")

C

D<-ggplot(dataper, aes(x = Year)) +
  geom_line(aes(y = AgingIndex, colour = "Aging Index (65 + per 100 0-19 years)"), size = 1.2) +
  scale_y_continuous(breaks =c(0,5,10,15,20,25,30)) +
  scale_x_continuous(breaks =c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  scale_colour_manual(values = c("Aging Index (65 + per 100 0-19 years)" = "steelblue", "OldDepRatio" = "firebrick")) +
  labs(title = "Aging Index",
       x = "Year", y = "65 + Per 100 age 0-19", colour = "") +
  theme_minimal()+
  theme(legend.position="bottom")

D

library(ggpubr)

fina<-ggarrange(A,B,C,D,ncol=2,nrow=2)
fina

print(fina)

ggsave(
  "figure2.png",
  plot = fina,
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

