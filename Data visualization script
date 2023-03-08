## Header ####
## who:KK
## what:Data visualization and analytics
## when:2023.03.08

## contents ####
## setup
## Forest _area
## Forest
## brazil_loss
## soybean_use
## vegetable_oil

#Load the dataset
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


#Load the necessary libraries
library(tidyverse)
library(scales)
theme_set(theme_light())
library(ggplot2)
library(tidytext)

#Forest area
forest_area_country <- forest_area%>%
  filter(str_length(code) ==3,
         year >=1992)%>%
  rename(country = entity)%>%
  mutate(forest_area = forest_area/100)


forest_area_country %>%
  filter(country %in% c("Russia", "Brazil", "Canada","United States", "China", "Australia"))%>%
  mutate(country = fct_reorder(country, -forest_area))%>%
  ggplot(aes(year, forest_area, color = country))+
  geom_line()+
  scale_y_continuous(labels = percent)+
  expand_limits(y = 0)+
  labs(x = "year", y = "percent of global forest")


#Forest conversion from 1992 to 2020
forest_area_country %>%
  filter(year %in% c(1992, 2020))%>%
  mutate(year= paste0("forest_area_", year)) %>%
  spread(year,forest_area) %>%
  arrange(desc(forest_area_1992))

#Forest
forest %>%
  group_by(year)%>%
  summarize(net_forest_conversion = sum(net_forest_conversion))

forest %>%
  filter(year==2015)%>%
  arrange((net_forest_conversion)) %>%
  slice_max(abs(net_forest_conversion), n = 10)%>%
  mutate(entity = fct_reorder(entity, net_forest_conversion))%>%
  ggplot(aes(net_forest_conversion, entity, fill = net_forest_conversion > 0))+
  geom_col()+
  scale_x_continuous(labels = comma)+
  theme(legend.position = "none")+
  labs(x = "Net change in forest conversion in 2015(Hectares)",
       y = "")


forest %>%
  group_by(year)%>%
  slice_max(abs(net_forest_conversion), n = 10)%>%
  ungroup()%>%
  mutate(entity = reorder_within(entity, net_forest_conversion, year))%>%
  ggplot(aes(net_forest_conversion, entity, fill = net_forest_conversion > 0))+
  geom_col()+
  facet_wrap(~ year, scales = "free_y")+
  scale_x_continuous(labels = comma)+
  scale_y_reordered()+
  theme(legend.position = "none")+
  labs(x = "Net change in forest conversion in Hectares(Top ten countries)",
       y = "")


forest %>%
  mutate(entity=fct_lump(entity,5, w = abs(net_forest_conversion)))%>%
  group_by(entity, year)%>%
  summarize(net_forest_conversion= sum(net_forest_conversion),
            .groups = "drop")%>%
  mutate(entity=fct_reorder(entity, -net_forest_conversion))%>%
  ggplot(aes(year,net_forest_conversion, color = entity))+
  geom_line()+
  scale_y_continuous(labels = comma)+
  labs(y = "Net change in forest (Hectares)")


#Brazil forest loss
brazil_loss <- brazil_loss %>%
  pivot_longer(commercial_crops:small_scale_clearing,
               names_to = "cause",
               values_to = "loss")%>%
  mutate(cause = str_to_sentence(str_replace_all(cause, "_", " ")))

#Brazil forest loss in 2013
brazil_loss%>%
  filter(year==max(year))%>%
  arrange(desc(loss))%>%
  mutate(cause=fct_reorder(cause, loss))%>%
  ggplot(aes(loss, cause))+
  geom_col()+
  scale_x_continuous(labels = comma)+
  labs(x = "loss of forest in 2013", y = "")


#Brazil forest loss by year wise
brazil_loss%>%
  arrange(desc(loss))%>%
  mutate(cause=fct_reorder(cause, loss))%>%
  ggplot(aes(loss, cause))+
  geom_col()+
  facet_wrap(~ year)+
  scale_x_continuous(labels = comma)+
  labs(y = "loss of forest in Brazil ", x = "")


#soybean use
soybean_use %>%
  filter(year == max(year)) %>%
  arrange(desc(total))

soybean_use <- soybean_use %>%
  filter(str_length(code) ==3) %>%
  mutate(total = human_food + animal_feed + processed) %>%
  pivot_longer(human_food:processed,
               names_to = "use",
               values_to = "count")%>%
  filter(!is.na(total))%>%
  arrange(desc(total))%>%
  mutate(use = str_to_sentence(str_replace_all(use, "-", " "))) 

soybean_use %>%
  filter(entity %in% c("Brazil","United States", "China"))%>%
  ggplot(aes(year, count, fill = use))+
  geom_area()+
  scale_y_continuous(labels = comma)+
  labs(y = "soybean use",
       fill = "")+
  facet_wrap(~ entity)



#Vegetable oil production
vegetable_oil %>%
  filter(!is.na(production))%>%
  mutate(crop_oil = fct_lump(crop_oil, 5, w = production))%>%
  group_by(entity, crop_oil, year) %>%
  summarize(production = sum(production))%>%
  filter(entity %in% c("United States", "India", "China", "Indonesia"))%>%
  ggplot(aes(year,production, fill = crop_oil))+
  geom_area()+
  scale_y_continuous(labels = comma)+
  facet_wrap(~ entity)

