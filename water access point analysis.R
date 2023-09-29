library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())

library(lubridate)

tt <- tidytuesdayR::tt_load('2021-05-04')

water <- tt$water 


water %>% 
  count(water_source , sort = TRUE)

water %>% 
  count(water_tech , sort = TRUE)

water %>% 
  count(water_tech ,water_source,
        sort = TRUE)

water %>% 
  count(installer,
        sort = TRUE)

water %>% 
  count(pay,
        sort = TRUE)

water %>% 
  count(status_id, status,
        sort = TRUE)


#cleaning the data
water <- water %>% 
  mutate(report_date = mdy(report_date) ) %>% 
  rename(lat = lat_deg,
         lon = lon_deg,
          country = country_name) %>% 
  separate(water_tech , c("water_tech", "brand"), sep = "_", fill = "right") %>% 
  mutate(install_year = ifelse(install_year > 2020, NA_real_, install_year)) %>% 
  filter(!country %in% c("Peru", "Dominican Republic", "Timor-Leste"), 
         !is.na(country)) %>% 
  filter(between(lat,  -35, 37), 
         between(lon, -40, 60))

 water %>% 
  filter(install_year > 1980) %>% 
  count(install_year) %>% 
  ggplot(aes(install_year, n ))+
  geom_col()

 water %>% 
   count(country, sort = TRUE) %>% 
   view()
 
#creating a maps
 
water %>% 
  sample_n(50000) %>% 
  filter(country != "Timor-Leste") %>% 
  ggplot(aes(lon, lat, color = country ))+
  geom_point()


water %>% 
  group_by(country) %>% 
  summarise(lat = mean(lat), 
            lon = mean(lon)) %>% 
  ggplot(aes(lon, lat))+
  geom_point()+
  geom_text(aes(label = country), vjust = 1, hjust = 1) 

water %>% 
  sample_n(50000) %>% 
  ggplot(aes(lon, lat, color = country ))+
  geom_point()

#getting the ountries that hve our data
countries <- unique(water$country)



water %>% 
  sample_n(50000) %>% 
  ggplot(aes(lon, lat))+
  borders("world", regions = "countries")+
  geom_point()

install.packages("countrycode")
library(countrycode)

africa_map_data <-  map_data("world") %>% 
  as_tibble() %>% 
  mutate(continent = countrycode(region, "country.name", "continent")) %>% 
  filter(continent == "Africa")


water %>% 
  sample_n(50000) %>% 
  ggplot(aes(lon, lat))+
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               data = africa_map_data)+
  geom_point()

library(ggthemes)

water %>% 
  sample_n(10000) %>% 
  ggplot(aes(lon, lat ))+
  geom_polygon(aes(long, lat, group = group),
               color = "gray",
               fill = "white",
               size = .25,
               data = africa_map_data)+
  geom_point(size = .1, alpha = .25)+
  theme_map()


# looking at one country per time 
water %>% 
  count(country, sort = TRUE)

#seeing that uganda has the most acces to water lets start with uganda

water%>% 
  filter(country == "Uganda") %>% 
  sample_n(10000) %>% 
  ggplot(aes(lon, lat ))+
  geom_polygon(aes(long, lat, group = group),
               color = "gray",
               fill = "white",
               size = .25,
               data = africa_map_data)+
  geom_point(size = .1, alpha = .25)+
  theme_map()


water %>%
  filter(country == "Uganda") %>% 
  sample_n(10000) %>% 
  ggplot(aes(lon, lat))+
  borders("world", regions = "Uganda")+
  geom_point(size = .1, alpha = .25)+
  theme_map()

#removing the data points thats outside its region 

#start with where are recorded water wells in Uganda 
water_uganda <- water %>%
  filter(country == "Uganda",
         between(lat, -2, 4), 
         between(lon, 29, 40)) 


water_uganda %>%
  filter(country == "Uganda",
         between(lat, -2, 4), 
         between(lon, 28, 40)) %>%  
  sample_n(10000) %>% 
  ggplot(aes(lon, lat))+
  borders("world", regions = "Uganda")+
  geom_point(size = .1, alpha = .25)+
  theme_map()

#open up uganda map on gooogle and see why point dont have water points 

water_uganda %>%
  sample_n(20000) %>% 
  ggplot(aes(lon, lat,  color = status_id))+
  borders("world", regions = "Uganda")+
  geom_point(size = .1, alpha = .25)+
  theme_map()+
  scale_color_discrete(guide = guide_legend(override.aes = list(size = 2, alpha = 1))) # to make the dot on the legends bgger 


water_uganda %>%
  ggplot(aes(lon, lat,  color = status_id))+
  borders("world", regions = "Uganda")+
  geom_point(size = .1, alpha = .25)+
  theme_map()+
  scale_color_discrete(guide = guide_legend(override.aes = list(size = 2, alpha = 1))) # to make the dot on the legends bgger 

#getting the map of uganda 

install.packages("ggmap")

library(ggmap)

bbox <- c(left = 29.2, bottom = -2, right = 35, top = 4.2)

uganda_map <- get_stamenmap(bbox, zoom = 8)  

ggmap(uganda_map)

#chhoosing your own data to pass
ggmap(uganda_map)+
  geom_point(aes(lon, lat),
             data = water_uganda,  size = .1, alpha = .1)

#fxing poperr coloring 
water_uganda %>% 
  count(pay, sort = TRUE)

#lumping through water uganda to get others and color 

water_uganda_lump <- water_uganda %>% 
  mutate(water_source =  fct_lump(water_source, 5)) %>% 
  replace_na(list(water_source = "Other")) %>% 
  mutate(water_source = fct_reorder(water_source, water_source, length, .desc = TRUE))

water_uganda_lump %>% 
  count(water_source)

ggmap(uganda_map)+
  geom_point(aes(lon, lat),
             data = water_uganda_lump,  size = .1, alpha = .1)+
  facet_wrap(~water_source)


water_uganda_lump %>% 
  mutate(report_year = year(report_date)) %>% 
  count(report_year, water_source) %>% 
  complete(report_year, water_source, fill = list(n = 0)) %>% 
  group_by(report_year) %>% 
  mutate(year_total = sum(n)) %>% 
  filter(year_total >= 500) %>% 
  ggplot(aes(report_year, n/year_total, fill = water_source))+
  geom_area()
#this shows that the data was collected from different perods 


#loking at the water teech 

water_uganda_lump %>% 
  mutate(water_tech = fct_lump(water_tech, 5) ) %>% 
  mutate(report_year = year(report_date)) %>% 
  count(report_year, water_tech) %>% 
  complete(report_year, water_tech , fill = list(n = 0)) %>% 
  group_by(report_year) %>% 
  mutate(year_total = sum(n)) %>% 
  filter(year_total >= 500) %>% 
  ggplot(aes(report_year, n, fill = water_tech))+
  geom_area()


#install year

water_uganda %>% 
  ggplot(aes(report_date, install_year))+
  geom_point()



ggmap(uganda_map)+
  geom_point(aes(lon, lat, color = install_year),
             data = water_uganda %>% sample_n(10000) ,  size = .33)+
  scale_color_gradient2(low = "red", high = "blue", midpoint = 1990)


#making an animation 
install.packages("gganimate")
library(gganimate)

install.packages("gifski")

library(gifski)

animation <-  water_uganda %>%
  filter(!is.na(install_year)) %>% 
  sample_n(10000) %>% 
  mutate(install_year = pmax(1990, install_year) ) %>% 
  mutate(year = map(install_year, ~seq(., 2021))) %>% 
  unnest(year) %>% 
  ggplot(aes(lon, lat))+
  borders("world", regions = "Uganda")+
  geom_point(size = .1, alpha = .25)+
  theme_map()+
  transition_manual(year)+
  labs(title = "Water sources in Ugander in year {current_frame}")

animate(animation, renderer = gifski_renderer())


# adding layouts to the maps


point_data <-  water_uganda %>%
  filter(!is.na(install_year)) %>% 
  sample_n(20000) %>% 
  mutate(install_year = pmax(1990, install_year) ) %>% 
  mutate(year = map(install_year, ~seq(., 2021))) %>% 
  unnest(year)

animate_data <- 
  ggmap(uganda_map)+
  
  geom_point()+
  geom_point(aes(lon, lat), data = point_data, size = .1, alpha = .25)+
  transition_manual(year)+
  labs(title = "Water sources in Ugander in year {current_frame}")

animate(animate_data, renderer = gifski_renderer())

