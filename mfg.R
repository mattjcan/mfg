# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(htmltools)
library(sp)
library(rgdal)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) 

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Dropbox/01a. Resources/data/mfg/" # parent directory for the data

al <- read_csv(paste0(d,"data/al.csv"), skip = 0)

sa2_emp <- read_csv(paste0(d,"data/ABS 2016 - ind_empl_sa2.csv"), skip = 9)

sa2_emp <- sa2_emp[2:2311, ]

asgs <- read_csv(paste0(d,"data/SA1_2016_AUST.csv"), skip = 0)

sa2_map_org <- readOGR(paste0(d,"data/SA2_2016_AUST.shx"))

sa3_map_org <- readOGR(paste0(d,"data/SA3_2016_AUST.shx"))

sa4_map_org <- readOGR(paste0(d,"data/SA4_2016_AUST.shx"))

# ind gva

path2 <- "C:/Users/matt/Dropbox/01a. Resources/data/mfg/data/5206006_industry_gva.xlsx"

sheets2 <- excel_sheets(path2)

xl_list2 <-
  lapply(excel_sheets(path2), read_excel, path = path2)

xl_list2 <- lapply(seq_along(sheets2), function(i) {
  data.frame(sheet = I(sheets2[i]), xl_list2[[i]])
})

# emp

path <- "C:/Users/matt/Dropbox/01a. Resources/data/mfg/data/6291005.xlsx"

sheets <- excel_sheets(path)

xl_list <-
  lapply(excel_sheets(path), read_excel, path = path)

xl_list <- lapply(seq_along(sheets), function(i) {
  data.frame(sheet = I(sheets[i]), xl_list[[i]])
})

# TIDY

sa2_emp <- sa2_emp %>% 
  rename(region = "INDP - 4 Digit Level")

sa2_emp <- sa2_emp %>% 
  gather(key = ind, value = emp, -c("region", "SA2_5DIGITCODE_2016"))

sa2_emp <- sa2_emp %>% 
  group_by(SA2_5DIGITCODE_2016) %>% 
  mutate(p_emp = (emp / sum(emp, na.rm = T)) * 100) %>% 
  ungroup()

asgs_sa3 <- asgs %>% 
  select(SA2_5DIGITCODE_2016, SA3_CODE_2016) %>% 
  distinct()

asgs_sa4 <- asgs %>% 
  select(SA2_5DIGITCODE_2016, SA4_CODE_2016) %>% 
  distinct()

sa2_emp <- left_join(sa2_emp, asgs_sa3, by = "SA2_5DIGITCODE_2016")

sa2_emp <- left_join(sa2_emp, asgs_sa4, by = "SA2_5DIGITCODE_2016")

sa2_emp <- sa2_emp %>% 
  rename(SA2_5DIG16 = "SA2_5DIGITCODE_2016")

sa2_emp_all <- sa2_emp %>% 
  ungroup() %>% 
  group_by(region, SA2_5DIG16) %>% 
  summarise(emp_tot = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  select(SA2_5DIG16, emp_tot)

list_ag <- unique(sa2_emp$ind)[1:64]

list_min <- unique(sa2_emp$ind)[65:86]

list_mfg <- unique(sa2_emp$ind)[87:285]

list_food <- unique(sa2_emp$ind)[88:121]

list_metal <- unique(sa2_emp$ind)[161:236]

sa2_emp$sect <- ifelse(sa2_emp$ind %in% list_ag, "ag",
       ifelse(sa2_emp$ind %in% list_min, "min",
              ifelse(sa2_emp$ind %in% list_mfg, "mfg",
               "other")))

sa2_emp$sect_d <- ifelse(sa2_emp$ind %in% list_ag, "ag",
                         ifelse(sa2_emp$ind %in% list_min, "min",
                                ifelse(sa2_emp$ind %in% list_food, "food",
                                ifelse(sa2_emp$ind %in% list_metal, "metal",
                                "other"))))

sa2_emp <- sa2_emp %>% 
  select(region, SA2_5DIG16, SA3_CODE_2016, SA4_CODE_2016, ind, emp, p_emp, sect, sect_d)

sa2_sect <- sa2_emp %>% 
  ungroup() %>% 
  group_by(region, SA2_5DIG16, SA3_CODE_2016, SA4_CODE_2016, sect) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA2_5DIG16) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)
  
sa2_sect_d <- sa2_emp %>% 
  ungroup() %>% 
  group_by(region, SA2_5DIG16, SA3_CODE_2016, SA4_CODE_2016, sect_d) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA2_5DIG16) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)

sect_t <- sa2_emp %>% 
  ungroup() %>% 
  group_by(sect) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)

sa3_sect <- sa2_sect %>% 
  ungroup() %>% 
  group_by(SA3_CODE_2016, SA4_CODE_2016, sect) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA3_CODE_2016) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)

sa4_sect <- sa2_sect %>% 
  ungroup() %>% 
  group_by(SA4_CODE_2016, sect) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA4_CODE_2016) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)

sa3_sect_d <- sa2_sect_d %>% 
  ungroup() %>% 
  group_by(SA3_CODE_2016, SA4_CODE_2016, sect_d) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA3_CODE_2016) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)

sa4_sect_d <- sa2_sect_d %>% 
  ungroup() %>% 
  group_by(SA4_CODE_2016, sect_d) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(SA4_CODE_2016) %>% 
  mutate(p_emp = emp / sum(emp, na.rm = T) * 100)


sa4_sect <- sa4_sect %>% 
  rename(SA4_CODE16 = SA4_CODE_2016)

sa4_sect_d <- sa4_sect_d %>% 
  rename(SA4_CODE16 = SA4_CODE_2016)

sa3_sect <- sa3_sect %>% 
  rename(SA3_CODE16 = SA3_CODE_2016)

sa3_sect_d <- sa3_sect_d %>% 
  rename(SA3_CODE16 = SA3_CODE_2016)


# gva

gva <- do.call(cbind, xl_list2[c(2:3)])

gva <- as_tibble(gva[,2:length(names(gva))])

gva <- gva[c(1:2, 10:249), c(1, 111:219, 277:353) ]

list_ind <- names(gva)[c(4, 11, 17, 21, 25, 26, 27, 28, 29, 36, 39, 42, 45, 46, 47, 48,  49, 50, 51, 52, 54, 55, 56)]

gva_ind <- gva %>% 
  select(...1, list_ind)

gva_ind <- gva_ind[3:nrow(gva_ind), ]

gva_ind <- gva_ind %>% 
  gather(key = "ind", value = "gva", 2:length(names(gva_ind)))

names(gva_ind) <- c("qtr", "ind", "gva")

gva_ind <- gva_ind %>% 
  filter(!is.na(gva))

gva_ind$qtr <- as.Date(as.numeric(gva_ind$qtr), origin = "1899-12-30")

gva_ind$gva <- as.numeric(gva_ind$gva)

# mfg

list_mfg <- names(gva)[c(12:17, 56)]

gva_mfg <- gva %>% 
  select(...1, list_mfg)

gva_mfg <- gva_mfg[3:nrow(gva_mfg), ]

gva_mfg <- gva_mfg %>% 
  gather(key = "ind", value = "gva", 2:length(names(gva_mfg)))

names(gva_mfg) <- c("qtr", "ind", "gva")

gva_mfg <- gva_mfg %>% 
  filter(!is.na(gva))

gva_mfg$qtr <- as.Date(as.numeric(gva_mfg$qtr), origin = "1899-12-30")

gva_mfg$gva <- as.numeric(gva_mfg$gva)

# emp

emp <- do.call(cbind, xl_list[c(2:4)])

emp <- as_tibble(emp[,2:length(names(emp))])

emp <- emp[c(10:nrow(emp)),  ]

emp_mfg <- emp %>% 
  select(...1, contains("Manufacturing....Employed.total"))


emp_mfg <- emp_mfg %>% 
  gather(key = "state", value = "emp", 2:length(names(emp_mfg)))

names(emp_mfg) <- c("qtr", "state", "emp")

emp_mfg <- emp_mfg %>% 
  filter(!is.na(emp))

emp_mfg$qtr <- as.Date(as.numeric(emp_mfg$qtr), origin = "1899-12-30")

emp_mfg$emp <- as.numeric(emp_mfg$emp)

# al

al$year <- paste0(al$year, "-06-30")

al$year <- as_date(al$year)

al_g <- gather(al, key = comm, value = ex, -year)


# TRANSFORM ---- 

# gva ind

gva_ind <- gva_ind %>% 
  group_by(qtr) %>% 
  mutate(p_gva = gva / gva[ind == "GROSS.DOMESTIC.PRODUCT.....165"] * 100) 

gva_ind <- gva_ind %>% 
  ungroup() %>% 
  group_by(ind) %>% 
  mutate(index = gva / gva[qtr == "1999-12-01"] * 100)

ind_labels <- tibble(ind = unique(gva_ind$ind), ind_lab = c("Agriculture", "Mining", "Manufacturing", "Electricity", "Construction", "Wholesale trade", "Retail trade", "Accommodation", "Transport", "ICT", "Finance", "Real estate", "Professional service", "Admin", "Public admin", "Education", "Health", "Arts", "Other services", "Ownership of dwellings", "Taxes less subsidies", "Statistical discrepancy", "GDP"))

gva_ind <- left_join(gva_ind, ind_labels, by = "ind")

# gva mfg

gva_mfg <- gva_mfg %>% 
  group_by(qtr) %>% 
  mutate(p_gva = gva / gva[ind == "GROSS.DOMESTIC.PRODUCT.....165"] * 100)

gva_mfg <- gva_mfg %>% 
  ungroup() %>% 
  group_by(ind) %>% 
  mutate(index = gva / gva[qtr == "2007-12-01"] * 100)

mfg_labels <- tibble(ind = unique(gva_mfg$ind), ind_lab = c("Food", "Petroleum", "Metals", "Machinery", "Other", "Manufacturing", "GDP"))

gva_mfg <- left_join(gva_mfg, mfg_labels, by = "ind")

mfg_delta <- gva_mfg %>% 
  ungroup() %>% 
  filter(qtr == "2007-12-01" | qtr == "2019-06-01") %>% 
  group_by(ind) %>% 
  mutate(delta = (gva / lag(gva, 1) - 1) * 100) %>% 
  filter(qtr == "2019-06-01")

mfg_delta_2000 <- gva_mfg %>% 
  ungroup() %>% 
  filter(qtr == "1999-12-01" | qtr == "2019-06-01") %>% 
  group_by(ind) %>% 
  mutate(delta = (gva / lag(gva, 1) - 1) * 100) %>% 
  filter(qtr == "2019-06-01")

mfg_delta_2000_2007 <- gva_mfg %>% 
  ungroup() %>% 
  filter(qtr == "1999-12-01" | qtr == "2007-12-01") %>% 
  group_by(ind) %>% 
  mutate(delta = (gva / lag(gva, 1) - 1) * 100) %>% 
  filter(qtr == "2007-12-01")

mfg_delta_2007_2019 <- gva_mfg %>% 
  ungroup() %>% 
  filter(qtr == "2007-12-01" | qtr == "2019-06-01") %>% 
  group_by(ind) %>% 
  mutate(delta = (gva / lag(gva, 1) - 1) * 100) %>% 
  filter(qtr == "2019-06-01")

al_g <- al_g %>% 
  ungroup() %>% 
  group_by(comm) %>% 
  mutate(index = ex / ex[year == "2007-06-30"] * 100)

# PLOTS ----

labs_al <- al_g %>% 
  filter(year == "2018-06-30")

p_al_in <- al_g %>% 
  ggplot(aes(x = year, y = index, color = comm)) + 
  geom_line(size = stroke_size) + 
  theme_mc +
  theme(legend.position = "bottom", legend.text = element_text(size=10), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank()) +
  scale_color_manual(values = c(Bauxite = line_color, Alumina = "red", Aluminium = "dark green")) +
  labs(title = paste0("Exports of Aluminium related commodities"), subtitle = "2007 = 100", x ="", y = "", caption = "Source: Resource and Energy Quarterly") +
  theme(plot.title = element_text(size = 14)) +
  ylim(0, 600) +
  geom_text(data = labs_al, aes(x = year, y = index, label = round(index,1), vjust = ifelse(comm == "Aluminium", 1.5, -1.5)), hjust = 0.5, size = 4) +
  geom_point(data = labs_al, size = 2.5) 
  


list_ind_plot <- c("Agriculture", "Mining", "Manufacturing")

p_gva <- gva_ind %>% 
  filter(ind_lab %in% list_ind_plot) %>% 
  ggplot(aes(x = qtr, y = index, color = ind_lab)) + 
  geom_line(size = stroke_size) +
  theme_mc +
  labs(title = "Industry output", subtitle = "index, 2007 = 100", x ="", y = "") +
  theme(legend.position = "bottom", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) +
  scale_color_manual(values = c(Agriculture = "dark green", Mining = "red", Manufacturing = "blue")) +
  ylim(0, 200)

# mfg gva

list_ind_plot <- c("")

p_gva_mfg <- gva_mfg %>% 
  ggplot(aes(x = qtr, y = index, color = ind_lab)) + 
  geom_line(size = stroke_size) +
  theme_mc +
  labs(title = "Industry output", subtitle = "index, 2007 = 100", x ="", y = "") +
  theme(legend.position = "bottom", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) 

p_mfg_delta <- mfg_delta %>% 
  ggplot(aes(x = reorder(ind_lab, -delta), y = delta))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Change in output since 2007"), subtitle = "%", caption = "Source: ABS 5206.0 Australian National Accounts: National Income, Expenditure and Product", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste0(round(delta,1), "%"), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-70, 100)

p_mfg_delta_2000 <- mfg_delta_2000 %>% 
  ggplot(aes(x = reorder(ind_lab, -delta), y = delta))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Change in output since 2000"), subtitle = "%", caption = "Source: ABS 5206.0 Australian National Accounts: National Income, Expenditure and Product", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste0(round(delta,1), "%"), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-70, 100)

p_mfg_delta_2000_2007 <- mfg_delta_2000_2007 %>% 
  ggplot(aes(x = reorder(ind_lab, -delta), y = delta))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Change in output from 2000 to 2007"), subtitle = "%", caption = "Source: ABS 5206.0 Australian National Accounts: National Income, Expenditure and Product", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste0(round(delta,1), "%"), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-70, 100)



# MAPS ---- 

# sa4

sa4_sect_mfg <- sa4_sect %>% 
  filter(sect == "mfg")

sa4_mfg_map <- sp::merge(sa4_map_org, sa4_sect_mfg, by = "SA4_CODE16", all=F)

pal_quant <- colorQuantile(palette="Blues", domain = sa4_mfg_map@data$p_emp, na.color="transparent")

labs_m <- sprintf(
  "<strong>%s</strong><br/>Mfg %%: %g %%<br/>Mfg employment: %g",
  sa4_mfg_map@data$SA4_NAME16, round(sa4_mfg_map@data$p_emp,1), round(sa4_mfg_map@data$emp,0)
) %>% lapply(htmltools::HTML)

m_sa4_mfg <- leaflet(data = sa4_mfg_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_quant(p_emp), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labs_m,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa4_mfg, file="m_sa4_mfg.html")

# sa3 mfg

sa3_sect_mfg <- sa3_sect %>% 
  filter(sect == "mfg")

sa3_mfg_map <- sp::merge(sa3_map_org, sa3_sect_mfg, by = "SA3_CODE16", all=F)

pal_quant <- colorQuantile(palette="Blues", domain = sa3_mfg_map@data$p_emp, na.color="transparent")

labs_m <- sprintf(
  "<strong>%s</strong><br/>Mfg %%: %g %%<br/>Mfg employment: %g",
  sa3_mfg_map@data$SA3_NAME16, round(sa3_mfg_map@data$p_emp,1), round(sa3_mfg_map@data$emp,0)
) %>% lapply(htmltools::HTML)

m_sa3_mfg <- leaflet(data = sa3_mfg_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_quant(p_emp), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labs_m,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa3_mfg, file="m_sa3_mfg.html")

# sa3 mfg

sa3_sect_min <- sa3_sect %>% 
  filter(sect == "min")

sa3_min_map <- sp::merge(sa3_map_org, sa3_sect_min, by = "SA3_CODE16", all=F)

pal_quant <- colorQuantile(palette="Blues", domain = sa3_min_map@data$p_emp, na.color="transparent")

labs_m <- sprintf(
  "<strong>%s</strong><br/>Mining %%: %g %%<br/>Mfg employment: %g",
  sa3_min_map@data$SA3_NAME16, round(sa3_min_map@data$p_emp,1), round(sa3_min_map@data$emp,0)
) %>% lapply(htmltools::HTML)

m_sa3_min <- leaflet(data = sa3_min_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_quant(p_emp), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labs_m,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa3_min, file="m_sa3_min.html")

# sa3 ag

sa3_sect_ag <- sa3_sect %>% 
  filter(sect == "ag")

sa3_ag_map <- sp::merge(sa3_map_org, sa3_sect_ag, by = "SA3_CODE16", all=F)

pal_quant <- colorQuantile(palette="Greens", domain = sa3_ag_map@data$p_emp, na.color="transparent")

labs_m <- sprintf(
  "<strong>%s</strong><br/>Ag %%: %g %%<br/>Mfg employment: %g",
  sa3_ag_map@data$SA3_NAME16, round(sa3_ag_map@data$p_emp,1), round(sa3_ag_map@data$emp,0)
) %>% lapply(htmltools::HTML)

m_sa3_ag <- leaflet(data = sa3_ag_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_quant(p_emp), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labs_m,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa3_ag, file="m_sa3_ag.html")


# sa2

sa2_sect_mfg <- sa2_sect %>% 
  filter(sect == "mfg")

sa2_mfg_map <- sp::merge(sa2_map_org, sa2_sect_mfg, by = "SA2_5DIG16", all=F)

pal_quant <- colorQuantile(palette="Blues", domain = sa2_mfg_map@data$p_emp, na.color="transparent")

labs_m <- sprintf(
  "<strong>%s</strong><br/>Mfg %%: %g %%<br/>Mfg employment: %g",
  sa2_mfg_map@data$SA2_NAME16, round(sa2_mfg_map@data$p_emp,1), round(sa2_mfg_map@data$emp,0)
) %>% lapply(htmltools::HTML)

m_sa2_mfg <- leaflet(data = sa2_mfg_map) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_quant(p_emp), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labs_m,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa2_mfg, file="m_sa2_mfg.html")

# EXPORT ---- 

png("img/p_mfg_delta.png", width = 8, height = 5, units = "in", res = 300)
p_mfg_delta
dev.off() 



