#### setup ####
# libraries
library(tidyverse)
library(mapview)
library(conflicted)
library(sf)
library(tigris)

tigris_use_cache = TRUE

#### basemap ####
zcta_base <- zctas(year = 2021) %>%
  select(geometry) %>%
  st_transform(2804) %>%
  st_crop(xmin= 423360.8, ymin= 168494, xmax= 442119.5, ymax= 190929) %>%
  erase_water()

baltimore_bound <- places(state = "MD", year = 2021) %>%
  filter(NAMELSAD == "Baltimore city") %>%
  select(geometry) %>%
  st_transform(2804) 

baltimore_no_water <- baltimore_bound %>%
  erase_water()

zcta_balt <- zcta_base %>%
  st_intersection(baltimore_no_water)

streets_base <- primary_secondary_roads(state = "MD", year = 2021) %>%
  select(geometry) %>%
  st_transform(2804) %>%
  st_crop(xmin= 423360.8, ymin= 168494, xmax= 442119.5, ymax= 190929) %>%
  st_as_sf()

streets_balt <- streets_base %>%
  st_simplify() %>%
  st_union() %>%
  st_intersection(baltimore_bound)

#### parks #### 
parks <- st_read("Parks.geojson") %>%
  st_transform(2804) %>%
  erase_water() %>%
  filter(acres > 90)

#### create locations ####
wire_poi <- data.frame(poi = c("pits",
                               "baltimore police department",
                               "circuit court for baltimore city",
                               "orlando's",
                               "university hospital",
                               "northeastern market",
                               "failed buy-bust",
                               "east-west basketball game",
                               "greenmount cemetary"),
                       coords = c("39.29988371984722, -76.62641785158569",#"pits",
                                  "39.29065445487585, -76.6179939499356",#"baltimore police department",
                                  "39.29094528466687, -76.61114069446187",# "city hall",
                                  "39.2855285533914, -76.59379188779478",# "orlando's",
                                  "39.2975181953095, -76.59270832377906",# "university hospital",
                                  "39.2981237592223, -76.58739919226154",# "northeastern market",
                                  "39.30754080510629, -76.66215578975655",# "failed buy-bust",
                                  "39.30627810336138, -76.58748243208466",# "east-west basketball game",
                                  "39.30934887460668, -76.60681096251554"# "greenmount cemetary"
                       )) %>%
  mutate(lon = word(coords, 2),
         lat = substr(word(coords, 1), start = 1, stop = 13),
         num = row_number()) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


#### map ####
water_df <- data.frame(xmin= 423360.8, ymin= 168494, xmax= 442119.5, ymax= 190929)

ggplot() +
  geom_rect(data = water_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            color = "transparent", fill = "dodgerblue4") +
  geom_sf(data = zcta_base, fill = "indianred3", color = "indianred4", alpha = 0.85) +
  geom_sf(data = zcta_balt, fill = "indianred2", color = "indianred3", size = 2) +
  geom_sf(data = st_union(parks), fill = "chartreuse4", color = NA, alpha = 0.55) +
  geom_sf(data = streets_base, color = "lightgoldenrod2", alpha = 0.2) +
  geom_sf(data = streets_balt, color = "lightgoldenrod1") +
  theme_void()
