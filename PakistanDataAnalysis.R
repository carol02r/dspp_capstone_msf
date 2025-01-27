#PAKISTAN CASE STUDY - DATA ANALYSIS 

library(tidyverse)
library(sf) 
library(patchwork) 
library(RColorBrewer)
library(showtext)


#0. Defining style ========================================

#downloading roboto font
font_add_google("Roboto", "roboto")
showtext_auto()

#defining capstone theme
theme_dspp <- function() {
  theme_void() + 
    theme(
      legend.position = 'top',
      legend.title = element_text(size = 12, family = "roboto"),
      legend.text = element_text(size = 10, family = "roboto"),
      legend.key.width = unit(2, 'cm'),
      legend.key.height = unit(0.5, 'cm'),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.box.margin = margin(t = 0, r = 10, b = 0, l = 0),
      legend.margin = margin(t = 5, r = 10, b = 5, l = 10)
    )
}

apply_dspp <- function(plot) {
  plot +
    guides(fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,  # Center-align the title
      barwidth = 20,      # Adjust the width of the color bar
      barheight = 0.5     # Adjust the height of the color bar
    )) +
    theme_dspp()
}



##################################################################################
################## 1. Pakistan map - estimates 2022 ##############################
#################################################################################

#1. Data transformation ========================================

#Reading the dataset
PAK <- read_csv("PAKISTAN_cleaned_data_reports_papers/district_level/district_case_notification_estimates_2022.csv")
PAKshp <- st_read("PAKISTAN_cleaned_data_reports_papers/shapefiles_pakistan/estimates2022.shp")
PAKreg <- st_read("shp/PAKreg.shp") # Regional level

#Merging shapefile with PAK variables 
PAKshp <- PAKshp %>%
  select(ADM2_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(district_id = ADM2_PCODE) %>% 
  inner_join(PAK, by = "district_id")

#Creating categories for CDR and CDR_reg
PAKshp$cdrcat <- cut(PAKshp$cdr, breaks=c(0, 0.25, 0.7, 1, Inf), 
                     labels=c("<25%","25-70%","70-100%",">100%"))
PAKshp$cdrregcat <- cut(PAKshp$cdr_reg, breaks=c(0, 0.25, 0.7, 1, Inf), 
                        labels=c("<25%","25-70%","70-100%",">100%"))

# calculating tb notification per 100k
PAKshp$pop_size <- PAKshp$pop_1k * 1000  # Convert pop_1k to actual population size
PAKshp$notified_cases_per_100k <- (PAKshp$tbnot_2022 / PAKshp$pop_size) * 100000

#Merging again at the regional level
PAKreg <- PAKreg %>%
  select(ADM1_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(region_id = ADM1_PCODE) %>% 
  inner_join(PAK, by = "region_id")

#2. Plots ==================================================

#Figure x: Number of cases notified per 100k in 2022

map1 <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = pmin(notified_cases_per_100k, 300)), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#495057', fill = NA, linewidth = 0.2) +
  scale_fill_gradientn(
    name = "TB cases notified per 100k in 2022", 
    colours = c("#FFF7EC", "#FEE8C8", "#FDBB84", "#FC8D59", "#E34A33", "#990000"), 
    limits = c(0, 300), 
    breaks = c(0, 50, 100, 150, 200, 250, 300), 
    labels = c("0", "50", "100", "150", "200", "250", "300+")
  )

apply_dspp (map1)

ggsave("figures/PAK_map.png", width = 10, height = 10, units = "in", dpi = 300)


