#PAKISTAN CASE STUDY - DATA ANALYSIS 

library(tidyverse)
library(sf) 
library(patchwork) 
library(RColorBrewer)
library(readr)
library(showtext)
library(stringdist)


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
      legend.position = "top",  # Place legend at the top
      legend.title = element_text(size = 16, family = "roboto"),
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
      legend.position = c(0.3, 0.80), # Top-left corner inside the plot
      legend.direction = "vertical", # Arrange legend items vertically
      legend.title = element_text(size = 26,  family = "roboto"),
      legend.text = element_text(size = 16, family = "roboto"),
      legend.key = element_rect(fill = "white", colour = "black"), # Proper key background
      legend.key.height = unit(0.5, "cm"), # Increase height for better visibility
      legend.key.width = unit(0.5, "cm"), # Keep width balanced
      legend.spacing.y = unit(0.5, "cm"), # Add vertical spacing between keys
      legend.box.margin = margin(t = 0, r = 10, b = 0, l = 0),
      legend.margin = margin(t = 5, r = 10, b = 5, l = 10)
    )
}



###############################################################################################################
################## 1. Pakistan map - estimates, cases and health facilities 2022 ##############################
###############################################################################################################


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

#ggsave("PAKISTAN_case_study_analysis/figures/map1_notifications_2022.png", width = 10, height = 10, units = "cm", dpi = 300)

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

#ggsave("PAKISTAN_case_study_analysis/figures/map2_cdr_2022.png", width = 10, height = 10, units = "cm", dpi = 300)

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

#ggsave("figures/map3_hfac_per100k_2022.png", width = 10, height = 10, units = "cm", dpi = 300)


##################################################################################
################## 2. genX per district wrangling ###############################
#################################################################################


#read from gitbub: https://github.com/carol02r/dspp_capstone_msf/blob/main/PAKISTAN_cleaned_data_reports_papers/district_level/genx_per_district.xlsx
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/district_level/genx_per_district.csv"
genx_dis <- read_csv(url)

genx_dis <- genx_dis %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(!is.na(district)) %>%    
  mutate(district = case_when(
    district %in% c("Abbotabad", "Abbottabad") ~ "Abbottabad",
    district %in% c("Batagram", "Battagram") ~ "Battagram",
    district %in% c("Charsada", "Charsadda") ~ "Charsadda",
    TRUE ~ district
  ))


#matching names from genx_dis to PAKreg ----------------

#turn everything tolower genx_dis and PAKreg
genx_dis$district <- tolower(genx_dis$district)
PAKreg$district <- tolower(PAKreg$district)

#remove - or any special characters from genx_dis and PAKreg
genx_dis$district <- gsub("-", " ", genx_dis$district)
PAKreg$district <- gsub("-", " ", PAKreg$district)

#using fuzzy matching to match the names using stringdist
# Function to find the best match from PAKreg for each district in genx_dis
#match_districts <- function(districts_to_match, reference_districts) {
  #sapply(districts_to_match, function(district) {
    #distances <- stringdist(district, reference_districts, method = "lv")  # Levenshtein distance
    #best_match <- reference_districts[which.min(distances)]  # Pick closest match
    #return(best_match)})}

# Apply fuzzy matching
#genx_dis <- genx_dis %>%
  #mutate(matched_district = match_districts(district, PAKreg$district))
#matchlist <- genx_dis %>% select(district, matched_district)

#after some manual adjustments and qual assessment done to match special cases, given changes in boundaries
#lets merged back and make a few changes -----------------------------------------------------------------

#read matchlist https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/district_level/matchlist_district_names.csv
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/district_level/matchlist_district_names.csv"
matchlistclean <- read_csv(url)

#merge back to genx_dis (using district_genxfile columns) 
genx_dis <- genx_dis %>%
  left_join(matchlistclean, by = c("district" = "district_genxfile"))

#1. tank rows should be merged into one in genx_dis (fr tank, district_genxfile column)
#2. bannu rows should be merged into one in  genx_dis (fr bannu and bannu, district_genxfile column)
#3. kohat rows should be merged into one in genx_dis (fr kohat and kohat, district_genxfile column)

genx_dis <- genx_dis %>%
  mutate(across(c(
    dots_population,
    grand_total_new_relapsed_cases,
    total_new_opd_quarter,
    presumptive_tb_cases_identified,
    presumptive_tb_patients_tested_afb_or_genx,
    confirmed_tb_cases_detected,
    new_relapse_tb_cases_tested_xpert
  ), as.numeric)) %>% 
  group_by(district_pakregfile) %>%
  summarise(
    dots_population = first(dots_population), # Keep the first value of dots_population
    across(where(is.numeric), sum, na.rm = TRUE), # Sum all numeric columns
    .groups = "drop"
  ) %>%
  filter(district_pakregfile %in% c("tank", "bannu", "kohat") | !duplicated(district_pakregfile))

#4. karachi (west, south etc) should be merged into one in PAKshp -------------------------------------------
#5. hunza and nagar should be merged into one in PAKshp
#6. shigar and kharmang should be merged into one in PAKshp

PAKshp$district <- gsub("-", " ", PAKshp$district)
PAKshp$district <- tolower(PAKshp$district)

#filter PAKSHP to contain the following rows:district_id, district, region, tbnot_2022, pop_size, inc_100k, inc_100k_reg, hc_facilities, hc_fac_100k, hc_fac_cat, cdr, cdr_reg, cdrcat, cdrregcat, notified_cases_per_100k
PAKshp_simple <- PAKshp %>%
  select(district, region, tbnot_2022, pop_size, inc_100k, inc_100k_reg, hc_facilities, hc_fac_100k, cdr, cdr_reg, notified_cases_per_100k)

# Assuming your shapefile is named `genx_shapefile`
karachi_districts <- c("central karachi", "east karachi", "korangi karachi", 
                       "malir karachi", "south karachi", "west karachi")
hunza_districts <- c("nagar", "hunza")

# Merge Karachi districts and rename
karachi_merged <- PAKshp_simple %>%
  filter(district %in% karachi_districts) %>%
  summarise(
    district = "karachi total", # Rename merged district
    # Sum numeric variables (default)
    across(
      where(is.numeric) & !c("inc_100k", "inc_100k_reg", "hc_fac_100k", "cdr", "cdr_reg", "notified_cases_per_100k"), 
      sum, na.rm = TRUE
    ),
    # Take the average for specific columns
    across(
      c(inc_100k, inc_100k_reg, hc_fac_100k, cdr, cdr_reg, notified_cases_per_100k), 
      mean, na.rm = TRUE
    ),
    geometry = st_union(geometry) # Merge geometries into one
  )


# Merge Hunza districts
hunza_merged <- PAKshp_simple %>%
  filter(district %in% hunza_districts) %>%
  summarise(
    district = "hunza", # New district name
    across(
      where(is.numeric) & !c("inc_100k", "inc_100k_reg", "hc_fac_100k", "cdr", "cdr_reg", "notified_cases_per_100k"),
      sum, na.rm = TRUE
    ),
    across(
      c(inc_100k, inc_100k_reg, hc_fac_100k, cdr, cdr_reg, notified_cases_per_100k),
      mean, na.rm = TRUE
    ),
    geometry = st_union(geometry) # Merge geometries
  )

#add rows 
PAKshp_simple <- PAKshp_simple %>%
  filter(!district %in% c(karachi_districts, hunza_districts)) %>% # Remove merged districts
  bind_rows(karachi_merged,hunza_merged) # Add merged rows

# Merging shapefie with genx_dis --------------------------------------------------------

#how many genx_dis$district_pakregfile are in PAKshp_simple$district
sum(genx_dis$district_pakregfile %in% PAKshp_simple$district)

#how many unique values are in genx_dis$district_pakregfile
length(unique(genx_dis$district_pakregfile))
#how many unique values are in PAKshp_simple$district
length(unique(PAKshp_simple$district))

#which repeated value PAKshp_simple$district
table(PAKshp_simple$district)

# Find the districts in PAKshp_simple that are not in genx_dis
non_matching_districts <- setdiff(PAKshp_simple$district, genx_dis$district_pakregfile)
non_matching_districts

#merge genx_dis to PAKshp_simple using district_pakregfile and district
PAKshp_genx <- PAKshp_simple %>%
  left_join(genx_dis, by = c("district" = "district_pakregfile"))

#save the shapefile
#st_write(PAKshp_genx, "PAKISTAN_cleaned_data_reports_papers/shapefiles_pakistan/PAKshp_genx.shp")



##################################################################################
################## 3. genX per district plot ####################################
#################################################################################

#Figure 4: new_relapse_tb_cases_tested_xpert2 ------------------------------------------- 

map4 <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = pmin(new_relapse_tb_cases_tested_xpert, 5000)), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_gradientn(
    name = "TB cases tested with GenX (2018)", # Updated title
    colours = c("black", "#f7e1f6", "#d4b2d8", "#b484c8", "#9457b7", "#7329a6", "#4e007e"), # Black for 0
    values = scales::rescale(c(0, 1, 1000, 2000, 3000, 4000, 5000)), # Adjust spacing for black
    limits = c(0, 5000),
    breaks = c(0, 1000, 2000, 3000, 4000),
    labels = c("0 (no genX)","1000", "2000", "3000", "4000+")
  ) + theme_dspp()

apply_dspp (map4)

#ggsave("PAKISTAN_case_study_analysis/figures/map4_genxtesting_absolute.png", width = 10, height = 10, units = "cm", dpi = 300)



#figure 5: % tested with genX --------------------------------------------------------

#new variable: percentage new_relapse_tb_cases_tested_xpert of confirmed_tb_cases_detected 
PAKshp_genx$perc_confirmed_genx <- (PAKshp_genx$new_relapse_tb_cases_tested_xpert / PAKshp_genx$confirmed_tb_cases_detected) * 100

#turn nan those > 100
PAKshp_genx$perc_confirmed_genx[PAKshp_genx$perc_confirmed_genx > 100] <- 100
map5 <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = cut(perc_confirmed_genx, 
                         breaks = c(-Inf, 0, 20, 40, 60, 80, Inf), 
                         labels = c("GenX not available", "1-20%", "21-40%", "41-60%", "61-80%", "80%+"))), 
          colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "Percentage of confirmed TB cases tested with GenX (2018)",
    values = c(
      "GenX not available" = "black",
      "1-20%" = "#ffccd5",
      "21-40%" = "#ff8fa3",
      "41-60%" = "#ff4d6d",
      "61-80%" = "#c9184a",
      "80%+" = "#800f2f"
    )) +  theme_dspp_cat()

map5

ggsave("PAKISTAN_case_study_analysis/figures/map5_genxtesting_perc.png", width = 15, height = 15, units = "cm", dpi = 300)

#6. genX presence vs outcomes  --------------------------------------------------

# Create the Case Detection Rate (CDR) categories
PAKshp_genx <- PAKshp_genx %>%
  mutate(cdrcat = cut(
    cdr, 
    breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),  # Adjusted for simplified ranges
    labels = c("0-20%", "20-40%", "40-60%", "80%+") # Fixed labels
  ))

# Extract centroids for GenX bubbles
PAKshp_genx_centroids <- PAKshp_genx %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(
    district = PAKshp_genx$district_pakregfile,  # Keep district name
    perc_confirmed_genx = PAKshp_genx$perc_confirmed_genx # Associate GenX data
  ) %>%
  filter(perc_confirmed_genx > 0) # Remove areas with 0% GenX (no bubbles)

# Create the map with simplified case detection categories and GenX bubbles
map6 <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = cdrcat), colour = "white", linewidth = 0.1) +
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.1) +
  geom_point(
    data = PAKshp_genx_centroids,
    aes(x = X, y = Y, size = cut(perc_confirmed_genx, 
                                 breaks = c(0, 20, 40, 60, 100, Inf), # 5 breaks
                                 labels = c("20%", "40%", "60%", "80%+", "100%+"))), # 5 labels
    color = "red", alpha = 0.7
  ) +
  scale_fill_manual(
    name = "Case detection rate", 
    values = c(
      "0-20%" = "#ebebff", 
      "20-40%" = "#bac9fe", 
      "40-60%" = "#5974d6", 
      "80%+" = "#002a80"
    )
  ) + 
  scale_size_manual(
    name = "GenX Testing (%)", 
    values = c("20%" = 2, "40%" = 4, "60%" = 6, "100%+" = 8) # Set fixed bubble sizes
  ) +
  guides(size = guide_legend(override.aes = list(color = "red"))) + 
  theme_dspp_cat()

# Display the final simplified map
map6
