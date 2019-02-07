library(tidyverse)
library(RSocrata)
library(cwi)
library(jsonlite)

# bind rows to trick other cities to add town column matching Hartford
weights <- list(
  "New Haven" = nhv_tracts,
  Hartford = hartford_tracts,
  Bridgeport = bridgeport_tracts,
  Stamford = stamford_tracts
) %>%
  bind_rows(.id = "city") %>%
  rename(neighborhood = name) %>%
  rename(name = geoid) %>%
  select(-tract) %>%
  bind_rows(
    tibble(
      city = c("New Haven", "Hartford", "Bridgeport", "Stamford"),
      name = c("New Haven", "Hartford", "Bridgeport", "Stamford"),
      weight = 1
    ) %>%
      mutate(neighborhood = name)
  )

# cdc from socrata
cdc_keep <- c("High blood pressure", "Current asthma", "Health insurance", "Diabetes", "Dental visit", "Current smoking", "Annual checkup", "Coronary heart disease", "Sleep <7 hours")

cdc_query <- list(
  stateabbr = "CT",
  data_value_type = "Crude prevalence",
  "$select" = "cityname,tractfips,year,category,short_question_text,data_value,geographiclevel"
) %>%
  imap(~paste(.y, .x, sep = "=")) %>%
  str_flatten(collapse = "&")
cdc_url <- "https://chronicdata.cdc.gov/resource/csmm-fdhi.json"

cdc_df <- read.socrata(paste(cdc_url, cdc_query, sep = "?"), Sys.getenv("SOCRATA_KEY")) %>%
  as_tibble() %>%
  select(city = cityname, year, tract = tractfips, level = geographiclevel, question = short_question_text, value = data_value, topic = category) %>%
  mutate_at(vars(year, value), as.numeric) %>%
  mutate(value = value / 100) %>%
  mutate_at(vars(question, topic), camiller::cap_first) %>%
  filter(question %in% cdc_keep) %>%
  mutate(level = as_factor(level) %>%
           fct_recode("1_neighborhood" = "Census Tract", "2_city" = "City")) %>%
  semi_join(weights, by = "city") %>%
  mutate(type = "map", format = ".0%")



# life expectancy from csv download
life_df <- read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CT_A.CSV") %>%
  select(1, 5) %>%
  set_names(c("tract", "value")) %>%
  mutate(indicator = "Life expectancy", topic = "Life expectancy", 
         level = factor("1_neighborhood") %>% fct_expand("2_city")) %>%
  inner_join(weights %>% distinct(city, name), by = c("tract" = "name")) %>%
  mutate(type = "map", format = ".3g")


# both 500 cities and life expectancy
health <- cdc_df %>% 
  select(tract, city, level, topic, indicator = question, value, type, format) %>%
  bind_rows(life_df) %>%
  mutate(name = coalesce(tract, city))

# get rid of clutter
# rm(cdc_keep, cdc_query, cdc_url, cdc_df, life_df)



all_city_health <- weights %>%
  left_join(health, by = c("name", "city")) %>% 
  group_by(city, level, topic, indicator, type, format, town, neighborhood) %>%
  summarise(value = weighted.mean(value, weight, na.rm = T) %>% round(digits = 3)) %>% 
  filter(!is.na(value)) %>%
  ungroup() %>%
  rename(geoType = level)


########### partly copied over from 2017acs

json <- list.files(path = "./to_viz", pattern = "data_\\d+\\.json", full.names = T) %>%
  set_names(str_extract(., "\\w+(?=_data)")) %>%
  map(fromJSON) %>%
  map(as_tibble) %>%
  map(select, -ends_with("order"))

headings <- list("indicator_headings.txt", "cdc_indicators.txt") %>%
  map_dfr(read_csv) %>%
  rename(displayIndicator = display)

# don't actually need order columns if writing to json
meta <- bind_rows(
  json[[1]] %>% rename(displayTopic = topic) %>% mutate(topic = displayTopic),
  all_city_health %>% rename(displayTopic = topic) %>% mutate(topic = str_extract(displayTopic, "\\w+$"))
) %>%
  distinct(topic, displayTopic, indicator, type, format) %>%
  rename(displayIndicator = indicator) %>%
  mutate(topic = as_factor(topic) %>%
           fct_relabel(str_to_lower) %>%
           fct_relabel(str_remove, " by age:") %>%
           fct_relabel(str_replace_all, "\\s", "_") %>%
           fct_relabel(str_replace, "children", "kids") %>%
           fct_recode(race = "race_and_ethnicity")) %>%
  inner_join(headings, by = c("topic", "displayIndicator")) %>%
  mutate(indicator = indicator %>%
           str_replace_all("\\s", "_") %>%
           str_replace("estimate", "num")) %>%
  # mutate(displayIndicator = coalesce(new_display, displayIndicator)) %>%
  select(topic, displayTopic, indicator, displayIndicator, type, format, new_display)


wide <- json %>%
  bind_rows(.id = "name") %>%
  bind_rows(all_city_health %>% mutate(name = city %>% str_replace_all("\\s", "_") %>% str_to_lower())) %>%
  select(city = name, town, name = neighborhood, geoType, topic, indicator, value) %>%
  rename(displayIndicator = indicator, displayTopic = topic) %>%
  left_join(meta, by = c("displayTopic", "displayIndicator")) %>%
  select(-matches("display"), -type, -format) %>%
  mutate_at(vars(indicator, topic), as_factor) %>%
  group_by(city, topic) %>%
  nest() %>%
  mutate(data = map(data, ~spread(., key = indicator, value = value) %>% arrange(desc(geoType)))) %>%
  split(.$city) %>%
  map(select, -city)


write_json(wide, "./to_viz/nhood_acs_health_wide.json")

meta %>%
  mutate(displayIndicator = coalesce(new_display, displayIndicator)) %>%
  select(-new_display) %>%
  mutate_at(vars(topic, displayTopic), as_factor) %>%
  arrange(topic, displayTopic) %>%
  write_json("./to_viz/nhood_meta.json")