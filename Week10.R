library(tidyverse)
library(skimr)
library(ggfortify)

setwd("/Users/taliagoldenberg/Desktop/DATA152")
ds=read_csv("ncbirths.csv")

skim(ds)
head(ds,10)
unique(ds$habit)

ds=ds %>%
  filter(! is.na(habit))

ds %>%
  ggplot(aes(x=weight)) +
  geom_histogram(fill="darkred") +
  facet_grid(habit~.)+
  theme_bw()

ds%>%
  group_by(habit) %>%
  summarize(
    mean_bw=mean(weight),
    sd_bw=sd(weight),
    n=n()
  )

t.test(weight~habit,data=ds,alternative="greater")
7.144-6.83

smokers=ds%>%
  filter(habit=="smoker") %>%
  pull(weight)

nonsmokers=ds%>%
  filter(habit=="nonsmoker") %>%
  pull(weight)

t.test(smokers,nonsmokers, alternative="less")


ds%>%
  group_by(habit) %>%
  summarize(
    mean_bw=mean(weight),
    sd_bw=sd(weight),
    n=n()
  ) %>%
  summarise(
    diff_means=6.83-7.14,
    sd_diff_meaans=sqrt(1.39^2/126+1.52^2/873),
    df=171,
    t_score=(diff_means-0) / sd_diff_meaans
    )

pt(-2.31,df=171)

##---------------------------------------------------------
ds24=read_csv("EPA_fueleco_2024.csv")

ds16=read_csv("EPA_fueleco_2016.csv")

# 2024 dataset
unique(ds24$`Mfr Name`)

Nissan24=ds24 %>%
  select(`Model Year`:`Comb Unrd Adj FE - Conventional Fuel`) %>%
  filter(`Mfr Name`== "Nissan") %>%
  slice_sample(n=20)


# 2016 dataset
unique(ds16$`Vehicle Manufacturer Name`)

Nissan16=ds16 %>%
   select(`Model Year`:`# of Gears`,`Test Category`: "FE_UNIT") %>%
   filter(`Vehicle Manufacturer Name`=="Nissan") %>%
   slice_sample(n=20)

# kia2016 %>% pull(RND_ADJ_FE)



##--------------------------THURSDAY-------------------------------

# null is something = something, we are assuming something 
# alt is based on wording of question
# continuous t, proportion z

library(car)



