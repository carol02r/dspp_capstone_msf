#PAKISTAN CASE STUDY - DATA ANALYSIS 

library(tidyverse)
library(sf) 
library(patchwork) 
library(RColorBrewer)
library(readr)
library(showtext)


#0. Defining style ========================================

#downloading roboto font
font_add_google("Roboto", "roboto")
showtext_auto()

#defining capstone theme
theme_dspp <- function() {
  theme_void() + 
    theme(
      panel.background = element_rect(fill = "white", colour = NA), 
      plot.background = element_rect(fill = "white", colour = NA),
      legend.title = element_text(size = 18, family = "roboto"),
      legend.text = element_text(size = 16, family = "roboto"),
      legend.key = element_rect(fill = "white", colour = NA), # Proper key background
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
      barwidth = 10,      # Adjust the width of the color bar
      barheight = 0.5     # Adjust the height of the color bar
    )) +
    theme_dspp()
}

theme_dspp_cat <- function() {
  theme_void() + 
    theme(
      # Set background to white
      panel.background = element_rect(fill = "white", colour = NA), 
      plot.background = element_rect(fill = "white", colour = NA),
      # Legend settings for vertical layout
      legend.position = c(0.2, 0.85), # Top-left corner inside the plot
      legend.direction = "vertical", # Arrange legend items vertically
      legend.title = element_text(size = 16, face = "bold", family = "roboto"),
      legend.text = element_text(size = 12, family = "roboto"),
      legend.key = element_rect(fill = "white", colour = "black"), # Proper key background
      legend.key.height = unit(0.4, "cm"), # Increase height for better visibility
      legend.key.width = unit(0.4, "cm"), # Keep width balanced
      legend.spacing.y = unit(0.5, "cm"), # Add vertical spacing between keys
      legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5), # Margin around the legend box
      legend.margin = margin(t = 10, r = 10, b = 10, l = 10) # Inner margin of legend
    )
}



##################################################################################
################## 1. Pakistan map - estimates 2022 ##############################
#################################################################################

#1. Data transformation ========================================

#reading directly from url
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/district_level/district_case_notification_estimates_2022.csv"
PAK <- read_csv(url)

#reading from cloned folder (shapefiles cant be read through url)
PAKshp <- st_read("PAKISTAN_cleaned_data_reports_papers/shapefiles_pakistan/estimates2022.shp")
PAKreg <- st_read("PAKISTAN_cleaned_data_reports_papers/shapefiles_pakistan/PAKreg.shp")

#Merging shapefile with PAK variables 
PAKshp <- PAKshp %>%
  select(ADM2_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(district_id = ADM2_PCODE) %>% 
  inner_join(PAK, by = "district_id")

PAKreg <- PAKreg %>%
  select(ADM1_PCODE, geometry, Shape_Leng, Shape_Area) %>% 
  rename(region_id = ADM1_PCODE) %>% 
  inner_join(PAK, by = "region_id")

#Creating categories for CDR and CDR_reg
PAKshp$cdrcat <- cut(PAKshp$cdr, breaks=c(0, 0.25, 0.7, 1, Inf), 
                     labels=c("<25%","25-70%","70-100%",">100%"))
PAKshp$cdrregcat <- cut(PAKshp$cdr_reg, breaks=c(0, 0.25, 0.7, 1, Inf), 
                        labels=c("<25%","25-70%","70-100%",">100%"))

# calculating tb notification per 100k
PAKshp$pop_size <- PAKshp$pop_1k * 1000  # Convert pop_1k to actual population size
PAKshp$notified_cases_per_100k <- (PAKshp$tbnot_2022 / PAKshp$pop_size) * 100000

#2. Plots ==================================================

#Figure 1: Number of cases notified per 100k in 2022

map1 <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = pmin(notified_cases_per_100k, 300)), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_gradientn(
    name = "TB notifications per 100k in 2022", 
    colours = c("#FFF7EC", "#FEE8C8", "#FDBB84", "#FC8D59", "#E34A33", "#990000"), 
    limits = c(0, 300), 
    breaks = c(0, 50, 100, 150, 200, 250, 300), 
    labels = c("0", "50", "100", "150", "200", "250", "300+")
  ) + theme_dspp()

apply_dspp (map1)

ggsave("figures/map1_notifications_2022.png", width = 10, height = 10, units = "cm", dpi = 300)

#Figure 2: Detection Rate in 2022 (based on 2022 paper estimates)
map2 <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = cdrcat), colour = "white", linewidth = 0.1) +
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "Case detection rate", 
    values = c("<25%"="#ebebff", "25-70%"="#bac9fe", 
               "70-100%"="#5974d6", ">100%"="#002a80")
  ) + 
  theme_dspp_cat()
map2

ggsave("figures/map2_cdr_2022.png", width = 10, height = 10, units = "cm", dpi = 300)

#Figure 3: Health Facilities per 100k in 2022
PAKshp$hc_fac_cat <- cut(
  PAKshp$hc_fac_100k,
  breaks = c(0, 1, 2, 3, Inf), # Adjusted breaks
  labels = c("0-1", "1-2", "2-3", "3+"), # Corresponding labels
  include.lowest = TRUE
)

# Plot the map
map3 <- ggplot(data = PAKshp) +
  geom_sf(aes(fill = hc_fac_cat), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "Health Facilities per 100k in 2022", 
    values = c("0-1" = "#e9f5db", "1-2" = "#b5c99a", "2-3" = "#87986a", "3+" = "#34623f"),
    labels = c("0-1", "1-2", "2-3", "3+")
  ) +
  theme_dspp_cat()
map3

ggsave("figures/map3_hfac_per100k_2022.png", width = 10, height = 10, units = "cm", dpi = 300)
