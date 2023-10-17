rm(list=ls())
library("eurostat")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(plm)
library(tidyr)
library(broom)

#################### DOWNLOAD E PULIZIA DATI ##########################
Home=get_eurostat("isoc_r_broad_h")
View(Home)
ict= subset(Home,unit=="PC_HH_IACC")
ict= drop_na(ict)
ict= unique(ict)
View(ict)

# GDP
GDP=get_eurostat("nama_10r_2gdp",filters = list(unit="MIO_EUR"))
GDP=drop_na(GDP);GDP=unique(GDP)
View(GDP)

# Impiegati
emp=get_eurostat("lfst_r_lfe2emp", filters = list(sex="T"))
View(emp)
emp= subset(emp,age=="Y15-64")
emp=drop_na(emp);emp=unique(emp)
View(emp)

# Produttività
prod= inner_join(GDP, emp, by=c('geo'='geo', 'time'='time'))
prod$prod= prod$values.x/prod$values.y
prod$prod= prod$prod * 100
View(prod)

##################### STATISTICHE DESCRITTIVE ####################################
X= ict[,-1]
Y= prod[,-8][,-7][,-6][,-5][,-4][,-1]
View(Y) # produttività geo tempo
View(X) # ict geo tempo
XX= merge(X,Y, by=c("geo","time"))
statdesc<- XX %>%
  select(
    `Prod` = prod,
    `ICT` = values,
      ) %>%
  summarise_all(funs(mean,median, sd, min, max), na.rm = TRUE) %>%
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  select(variable, mean,median, sd, min, max) %>%
  mutate_each(funs(round(., 1)), -variable)
View(statdesc)

################### MAPPA ICT 2018 ######################
geo= get_eurostat("isoc_r_broad_h",filters = list(unit="PC_HH_IACC"),time_format="num")
geo= drop_na(geo);geo= unique(geo)

geo <- geo %>%
  filter(time == 2018) %>%
  dplyr::mutate(cat = cut_to_classes(values, n = 7, decimals = 1))

sf <- get_eurostat_geospatial(output_class = "sf", resolution = "60",
                              nuts_level = "2")

df3 <- left_join(sf, geo, by = c("id" = "geo"))
df3b <- right_join(geo, sf, by = c("geo" = "id"))

df3 %>%
  count(cat)

my_theme <- function() {
  theme_bw() %+replace%
    theme(
      text = element_text(family = "Ubuntu Mono"),
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightgrey"),
      panel.border = element_rect(color = "lightgrey", fill = NA),
      title = element_text(color = "#2E3436"),
      axis.line = element_line(color = "lightgrey"),
      axis.ticks = element_line(color = "lightgrey"),
      axis.text = element_text(color = "#005577"),
      axis.title = element_text(face = "bold", size = 12, color = "#005577"),
      strip.background = element_rect(color = "#005577", fill = "#005577"),
      strip.text.x = element_text(size = 11, color = "white")
    )
}

p1 <- df3 %>%
  filter(is.na(cat) == FALSE) %>%
  ggplot(aes(fill = cat)) +
  scale_fill_brewer(palette = "PuBu") +
  geom_sf(color = "dim grey", size = .1) +
  xlim(c(-12, 44)) +
  ylim(c(35, 70)) +
  guides(fill = guide_legend(reverse = TRUE, title = "ICT (%)")) +
  labs(title = "ICT rate in NUTS 2 regions, 2018",
       caption = "Data source: Eurostat") +
  my_theme() +
  theme(
    plot.title = element_text(family = "ETBembo"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.5)
  )
p1
################## MAPPA ICT 2008 ##############################
geo= get_eurostat("isoc_r_broad_h",filters = list(unit="PC_HH_IACC"),time_format="num")
geo= drop_na(geo);geo= unique(geo)

geo <- geo %>%
  filter(time == 2008) %>%
  dplyr::mutate(cat = cut_to_classes(values, n = 7, decimals = 1))

sf <- get_eurostat_geospatial(output_class = "sf", resolution = "60",
                              nuts_level = "2")

df3 <- left_join(sf, geo, by = c("id" = "geo"))
df3b <- right_join(geo, sf, by = c("geo" = "id"))

df3 %>%
  count(cat)

my_theme <- function() {
  theme_bw() %+replace%
    theme(
      text = element_text(family = "Ubuntu Mono"),
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightgrey"),
      panel.border = element_rect(color = "lightgrey", fill = NA),
      title = element_text(color = "#2E3436"),
      axis.line = element_line(color = "lightgrey"),
      axis.ticks = element_line(color = "lightgrey"),
      axis.text = element_text(color = "#005577"),
      axis.title = element_text(face = "bold", size = 12, color = "#005577"),
      strip.background = element_rect(color = "#005577", fill = "#005577"),
      strip.text.x = element_text(size = 11, color = "white")
    )
}

p2 <- df3 %>%
  filter(is.na(cat) == FALSE) %>%
  ggplot(aes(fill = cat)) +
  scale_fill_brewer(palette = "PuBu") +
  geom_sf(color = "dim grey", size = .1) +
  xlim(c(-12, 44)) +
  ylim(c(35, 70)) +
  guides(fill = guide_legend(reverse = TRUE, title = "ICT (%)")) +
  labs(title = "ICT rate in NUTS 2 regions, 2008",
       caption = "Data source: Eurostat") +
  my_theme() +
  theme(
    plot.title = element_text(family = "ETBembo"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.5)
  )
p2

################## MAPPE PRODUTTIVITA' 2018 #####################
GDP=get_eurostat("nama_10r_2gdp",filters = list(unit="MIO_EUR"),time_format = "num")
emp=get_eurostat("lfst_r_lfe2emp", filters = list(sex="T"),time_format = "num")
emp= subset(emp,age=="Y15-64")
prod= inner_join(GDP, emp, by=c('geo'='geo', 'time'='time'))
prod$prod= prod$values.x/prod$values.y
prod$prod= prod$prod * 100
prod=drop_na(prod);prod=unique(prod)

geoo <- prod %>%
  filter(time == 2018) %>%
  dplyr::mutate(cat = cut_to_classes(prod, n = 7, decimals = 1))

sf <- get_eurostat_geospatial(output_class = "sf", resolution = "60",
                              nuts_level = "2")

df3 <- left_join(sf, geoo, by = c("id" = "geo"))
df3b <- right_join(geoo, sf, by = c("geo" = "id"))


df3 %>%
  count(cat)

my_theme <- function() {
  theme_bw() %+replace%
    theme(
      text = element_text(family = "Ubuntu Mono"),
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightgrey"),
      panel.border = element_rect(color = "lightgrey", fill = NA),
      title = element_text(color = "#2E3436"),
      axis.line = element_line(color = "lightgrey"),
      axis.ticks = element_line(color = "lightgrey"),
      axis.text = element_text(color = "#005577"),
      axis.title = element_text(face = "bold", size = 12, color = "#005577"),
      strip.background = element_rect(color = "#005577", fill = "#005577"),
      strip.text.x = element_text(size = 11, color = "white")
    )
}

p3 <- df3 %>%
  filter(is.na(cat) == FALSE) %>%
  ggplot(aes(fill = cat)) +
  scale_fill_brewer(palette = "PuBu") +
  geom_sf(color = "dim grey", size = .1) +
  xlim(c(-12, 44)) +
  ylim(c(35, 70)) +
  guides(fill = guide_legend(reverse = TRUE, title = "Productivity Rate (%)")) +
  labs(title = "Productivity rate in NUTS 2 regions, 2018",
       caption = "Data source: Eurostat") +
  my_theme() +
  theme(
    plot.title = element_text(family = "ETBembo"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.5)
  )
p3

################## MAPPE PRODUTTIVITA' 2008 #####################
GDP=get_eurostat("nama_10r_2gdp",filters = list(unit="MIO_EUR"),time_format = "num")
emp=get_eurostat("lfst_r_lfe2emp", filters = list(sex="T"),time_format = "num")
emp= subset(emp,age=="Y15-64")
prod= inner_join(GDP, emp, by=c('geo'='geo', 'time'='time'))
prod$prod= prod$values.x/prod$values.y
prod$prod= prod$prod * 100
prod=drop_na(prod);prod=unique(prod)

geoo <- prod %>%
  filter(time == 2008) %>%
  dplyr::mutate(cat = cut_to_classes(prod, n = 7, decimals = 1))

sf <- get_eurostat_geospatial(output_class = "sf", resolution = "60",
                              nuts_level = "2")

df3 <- left_join(sf, geoo, by = c("id" = "geo"))
df3b <- right_join(geoo, sf, by = c("geo" = "id"))


df3 %>%
  count(cat)

my_theme <- function() {
  theme_bw() %+replace%
    theme(
      text = element_text(family = "Ubuntu Mono"),
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightgrey"),
      panel.border = element_rect(color = "lightgrey", fill = NA),
      title = element_text(color = "#2E3436"),
      axis.line = element_line(color = "lightgrey"),
      axis.ticks = element_line(color = "lightgrey"),
      axis.text = element_text(color = "#005577"),
      axis.title = element_text(face = "bold", size = 12, color = "#005577"),
      strip.background = element_rect(color = "#005577", fill = "#005577"),
      strip.text.x = element_text(size = 11, color = "white")
    )
}

p4 <- df3 %>%
  filter(is.na(cat) == FALSE) %>%
  ggplot(aes(fill = cat)) +
  scale_fill_brewer(palette = "PuBu") +
  geom_sf(color = "dim grey", size = .1) +
  xlim(c(-12, 44)) +
  ylim(c(35, 70)) +
  guides(fill = guide_legend(reverse = TRUE, title = "Productivity Rate (%)")) +
  labs(title = "Productivity rate in NUTS 2 regions, 2008",
       caption = "Data source: Eurostat") +
  my_theme() +
  theme(
    plot.title = element_text(family = "ETBembo"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.5)
  )
p4

#################### REGRESSIONE LINEARE #####################
#model=plm(prod~values, model="pooling" ,data=XX,index=c("geo","time"))
model=plm(prod~lag(values,n=1),data=XX, model="pooling")
summary(model)
