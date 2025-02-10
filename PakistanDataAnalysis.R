#PAKISTAN CASE STUDY - DATA ANALYSIS 

library(tidyverse)
library(sf) 
library(patchwork) 
library(RColorBrewer)
library(readr)
library(showtext)
library(stringdist)
library(ggrepel)


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

#for categorical maps
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
      legend.text = element_text(size = 20, family = "roboto"),
      legend.key = element_rect(fill = "white", colour = "black"), # Proper key background
      legend.key.height = unit(0.5, "cm"), # Increase height for better visibility
      legend.key.width = unit(0.5, "cm"), # Keep width balanced
      legend.spacing.y = unit(0.5, "cm"), # Add vertical spacing between keys
      legend.box.margin = margin(t = 0, r = 10, b = 0, l = 0),
      legend.margin = margin(t = 5, r = 10, b = 5, l = 10)
    )
}

#for time-series

theme_dspp_ts <- function() {
  theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.1),  # Keep horizontal grid lines
      panel.grid.major.x = element_line(color = "grey90", size = 0.1),  # Keep horizontal grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", colour = NA), 
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "top",  # Place legend at the top
      legend.title = element_text(size = 16, family = "roboto"),
      legend.text = element_text(size = 16, family = "roboto"),
      legend.key.width = unit(1, 'cm'),
      legend.key.height = unit(0.5, 'cm'),
      legend.spacing.x = unit(0.5, 'cm'),
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
PAKshp$cdrcat <- cut( PAKshp$cdr, 
                      breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),  # Adjusted for simplified ranges
                      labels = c("0-20%", "20-40%", "40-60%", "80%+"))
PAKshp$cdrregcat <- cut(PAKshp$cdr_reg, 
                        breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),  # Adjusted for simplified ranges
                        labels = c("0-20%", "20-40%", "40-60%", "80%+"))

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
    name = "Estimated case detection rate (2022)", 
    values = c("0-20%"="#ebebff", "20-40%"="#bac9fe", 
               "40-60%"="#5974d6", "80%+"="#002a80")
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
################## 2. genX per district wrangling ################################
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
################## 4. Single indicators maps ####################################
#################################################################################


#FIGURE 4:  new_relapse_tb_cases_tested_xpert2 (2018) -----------------------------------------------------------------------

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

#FIGURE 4B:  presumptive_tb_patients_tested_afb_or_genx (2018) -----------------------------------------------------------------------
# Create categories for presumptive TB cases tested
PAKshp_genx$tb_tested_category <- cut(
  PAKshp_genx$presumptive_tb_patients_tested_afb_or_genx,
  breaks = c(0, 1000, 5000, 10000, 20000, Inf), # Categories based on your distribution
  labels = c("<1k", "1k–5k", "5k–10k", "10k–20k", "20k+"),
  right = FALSE # Include lower boundary in the interval
)

# Plot map with categories
map4b <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = tb_tested_category), colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "TB cases tested with GenX (2018)", 
    values = c(
      "<1k" = "#f7e1f6", 
      "1k–5k" = "#d4b2d8", 
      "5k–10k" = "#b484c8", 
      "10k–20k" = "#9457b7", 
      "20k+" = "#4e007e"
    )
  ) + 
  theme_dspp_cat()

map4b

#ggsave("PAKISTAN_case_study_analysis/figures/map4_genxtesting_absolute.png", width = 10, height = 10, units = "cm", dpi = 300)


##################################################################################
################## 3. genX per district plot ####################################
#################################################################################
 
#new variable: percentage new_relapse_tb_cases_tested_xpert of confirmed_tb_cases_detected 
PAKshp_genx$perc_confirmed_genx <- (PAKshp_genx$new_relapse_tb_cases_tested_xpert / PAKshp_genx$confirmed_tb_cases_detected) * 100
PAKshp_genx$perc_total_genx <- (PAKshp_genx$new_relapse_tb_cases_tested_xpert / PAKshp_genx$grand_total_new_relapsed_cases) * 100

#perc_confirmed_genx shows the proportion of bacteriologically confirmed TB cases tested with GenX, focusing on diagnostic confirmation efficiency.
#perc_total_genx shows the proportion of all new and relapsed TB cases (both confirmed and clinically diagnosed) tested with GenX, focusing on overall testing coverage.

#FIGURE 5: % perc_confirmed_genx (2018) -----------------------------------------------------------------------------------------
hist(pmin(PAKshp_genx$perc_confirmed_genx, 100), breaks = 20, col = "lightblue", border = "black",main = "Histogram of perc_confirmed_genx")

#turn those above 100 to 100
PAKshp_genx$perc_confirmed_genx[PAKshp_genx$perc_confirmed_genx > 100] <- 100

map5 <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = cut(perc_confirmed_genx, 
                         breaks = c(-Inf, 0, 20, 40, 60, 80, Inf), 
                         labels = c("GenX not available", "1-20%", "21-40%", "41-60%", "61-80%", "80%+"))), 
          colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "Percentage of Confirmed TB Cases Tested Using GenX (2018)",
    values = c(
      "GenX not available" = "black",
      "1-20%" = "#ffccd5",
      "21-40%" = "#ff8fa3",
      "41-60%" = "#ff4d6d",
      "61-80%" = "#c9184a",
      "80%+" = "#800f2f"
    )) +  theme_dspp_cat()

map5

#Figure 5b: % perc_total_genx (2018) -----------------------------------------------------------------------------------------
hist(PAKshp_genx$perc_total_genx, breaks = 20, col = "lightblue", border = "black", main = "Histogram of perc_total_genx")


map5b <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = cut(perc_total_genx, 
                         breaks = c(-Inf, 0, 20, 40, 60, 80, Inf), 
                         labels = c("GenX not available", "1-20%", "21-40%", "41-60%", "61-80%", "80%+"))), 
          colour = "white", linewidth = 0.1) + 
  geom_sf(data = PAKreg, colour = '#432818', fill = NA, linewidth = 0.1) +
  scale_fill_manual(
    name = "Percentage of All New & Relapsed TB Cases Tested Using GenX (2018)",
    values = c(
      "GenX not available" = "black",
      "1-20%" = "#ffccd5",
      "21-40%" = "#ff8fa3",
      "41-60%" = "#ff4d6d",
      "61-80%" = "#c9184a",
      "80%+" = "#800f2f"
    )) +  theme_dspp_cat()

map5b

#ggsave("PAKISTAN_case_study_analysis/figures/map5b_genxtestingtotal_perc.png", width = 20, height = 20, units = "cm", dpi = 300)


#FIGURE 6: genX presence vs case detection rate  ----------------------------------------------------------

# Create the Case Detection Rate (CDR) categories
PAKshp_genx <- PAKshp_genx %>%
  mutate(cdrcat = cut(
    cdr, 
    breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),  # Adjusted for simplified ranges
    labels = c("0-20%", "20-40%", "40-60%", "80%+")  # Fixed labels
  ))

# Extract centroids for GenX bubbles
PAKshp_genx_centroids <- PAKshp_genx %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(
    district = PAKshp_genx$district_pakregfile,  # Keep district name
    perc_confirmed_genx = PAKshp_genx$perc_confirmed_genx, # Associate GenX data
    cdrcat = PAKshp_genx$cdrcat # Associate CDR data
  ) %>%
  filter(perc_confirmed_genx > 0) # Remove areas with 0% GenX (no bubbles)

#plot map
map6 <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = cdrcat), colour = "white", linewidth = 0.1) +
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.1) +
  geom_point(
    data = PAKshp_genx_centroids %>% 
      filter(perc_confirmed_genx >= 40), # Exclude values less than 40%
    aes(x = X, y = Y, size = cut(perc_confirmed_genx, 
                                 breaks = c(40, 60, 80, Inf), # New breaks
                                 labels = c("40-60%", "60-80%", "80%+"))), # New labels
    color = "red", alpha = 0.5
  ) +
  scale_fill_manual(
    name = "Estimated case detection rate", 
    values = c(
      "0-20%" = "#ebebff", 
      "20-40%" = "#bac9fe", 
      "40-60%" = "#5974d6", 
      "80%+" = "#002a80"
    )
  ) + 
  scale_size_manual(
    name = "Percentage of confirmed TB cases tested with GenX", 
    values = c("40-60%" = 2, "60-80%" = 4, "80%+" = 8) # Adjust bubble sizes
  ) +
  guides(size = guide_legend(override.aes = list(color = "red"))) + 
  theme_dspp_cat()
map6

#ggsave("PAKISTAN_case_study_analysis/figures/map6_genx_cdr.png", width = 20, height = 20, units = "cm", dpi = 300)

#FIGURE 6b: GenX dummy vs case detection (inverted) -------------------------------------------------------------

# Add a dummy for GenX presence (yes/no)
PAKshp_genx <- PAKshp_genx %>%
  mutate(genx_presence = ifelse(perc_confirmed_genx >= 10, "Gen X Available", "GenX not Available"))
  #mutate(genx_presence = ifelse(perc_confirmed_genx >= 50, "Above 50%", "Below 50% or no Genx presence"))
  

# Base map with GenX presence
map6b <- ggplot(data = PAKshp_genx) +
  geom_sf(aes(fill = genx_presence), colour = "white", linewidth = 0.1) +
  geom_sf(data = PAKreg, colour = '#3B3B3B', fill = NA, linewidth = 0.1) +
  geom_point(
    data = PAKshp_genx_centroids %>% 
      filter(cdrcat != "0-20%"), # Exclude "0-20%" category
    aes(x = X, y = Y, size = cdrcat, color = cdrcat), # Use cdrcat directly
    color = "#ff9b5c", alpha = 0.75
  ) +
  # Define the palette for GenX presence
  scale_fill_manual(
    name = "Confirmed TB cases tested with GenX",
    values = c("Gen X Available" = "#5974d6", "GenX not Available" = "#edede9")
  ) +
  # Define bubble sizes for CDR categories
  scale_size_manual(
    name = "Case Detection Rate (CDR)",
    values = c("20-40%" = 2, "40-60%" =4 , "80%+" = 8)
  ) + 
  # Adjust theme
  theme_dspp_cat() 
  
# Display the inverted map
map6b
#ggsave("PAKISTAN_case_study_analysis/figures/map8_genx_cdr_inverted_yesno.png", width = 20, height =20, units = "cm", dpi = 300)


#FIGURE 7 (SCATTER PLOT) - genX presence vs case detection rate ------------------------------------------------
#turn those above 100 to 100
scatter1 <- ggplot(PAKshp_genx, aes(x = cdr, y = perc_confirmed_genx)) +
  geom_point(color = "#002a80", size = 3, alpha = 0.6) +  # Fixed color & size 
  geom_smooth(method = "lm", color = "black", linetype = "dashed", size = 0.5) + # Add trend line
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), name = "Case Detection Rate (CDR)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), name = "GenX Testing (%)") +
  labs(
    title = "Case Detection Rate (CDR) vs. GenX Testing (%)",
    x = "Case Detection Rate (CDR)",
    y = "Percentage of Confirmed TB Cases Tested with GenX"
  ) + 
  theme_minimal() +
  theme(plot.title = element_text(size = 30, family = "roboto"),
        axis.title = element_text(size = 30, family = "roboto"),
        axis.text = element_text(size = 30, family = "roboto"),
        panel.background = element_rect(fill = "white", colour = NA))

scatter1
#ggsave("PAKISTAN_case_study_analysis/figures/scatter1_genx_cdr.png", width = 15, height = 10, units = "cm", dpi = 300)

#INTERATING TO HIGHLIGHT EACH PROVINCE -------------------------------------------------------------------

# Get unique regions (excluding NA and Islamabad)
regions <- PAKshp_genx %>%
  filter(!is.na(region) & region != "Islamabad") %>%
  pull(region) %>%
  unique()

# Function to create a scatter plot for each region
create_region_plot <- function(region_name) {
  
  PAKshp_genx <- PAKshp_genx %>%
    mutate(region_highlight = ifelse(region == region_name, "Highlight", "Other"))  # Mark selected region
  
  ggplot(PAKshp_genx, aes(x = cdr, y = perc_confirmed_genx)) +
    geom_point(aes(color = region_highlight), size = 3, alpha = 1) +  # Highlighted region in orange
    geom_smooth(method = "lm", color = "#ffb67e", linetype = "dashed", size = 0.5, se = FALSE) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), name = "Case Detection Rate (CDR)") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), name = "GenX Testing (%)") +
    scale_color_manual(values = c("Highlight" = "#ffb67e", "Other" = "#adb5bd")) +  # Color scheme
    labs(
      title = paste("CDR vs. GenX Testing -", region_name),
      x = "Case Detection Rate (CDR)",
      y = "Percentage of Confirmed TB Cases Tested with GenX"
    ) + 
    theme_minimal()+
    theme(plot.title = element_text(size = 25, family = "roboto"),
          axis.title = element_text(size = 25, family = "roboto"),
          axis.text = element_text(size = 20, family = "roboto"),
          axis.title.y = element_text(angle = 90),  # Ensure y-axis title is vertical
          legend.position = "none",
          panel.background = element_rect(fill = "white", colour = NA)) 
}

# Create and save each plot
plots <- map(regions, create_region_plot)

# Display the first plot as an example
plots[[1]]
 
walk2(plots, regions, ~ggsave(filename = paste0("PAKISTAN_case_study_analysis/figures/scatter_", .y, ".png"),
                              plot = .x, width = 15, height = 10, units = "cm", dpi = 300))


    
##################################################################################
################## 4. genX per province #########################################
#################################################################################

#Creating a province shapefile based on PAKshp (district)
PAKshp_region <- PAKshp %>%
  group_by(region) %>%  # Group by region
  summarize(geometry = st_union(geometry), .groups = "drop") 

#reading URL with province level data for 2022: https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_diagnosis_2022.csv
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_diagnosis_2022.csv"
PAKprov <- read_csv(url)

#drop region - pakistan; and rename Khyber Pakhtunkhwa to Khyber Pakhtun Khwa; Azad Jammu & Kashmir to Azad Jammu and Kashmir
PAKprov <- PAKprov %>%
  filter(province != "Pakistan") %>%  # Remove the Pakistan region
  mutate(province = recode(province,
                         "Khyber Pakhtunkhwa" = "Khyber Pakhtun Khwa",
                         "Azad Jammu & Kashmir" = "Azad Jammu and Kashmir",
                         "Islamabad & CT" = "Islamabad"))  

# and merge with PAKshp_region shapefile
PAKshp_region <- PAKshp_region %>%
  left_join(PAKprov, by = c("region" = "province"))

#MAP 9 - genexpert_sites_connected ---------------------------------------------------
#other variables:

#genexpert_sites_connected
#total_modules_connected
#yearly_utilization_percent
#avg_tests_per_day
#yearly_utilization_percent
#mtb_positivity_rate_percent
#avg_population_per_centre
#population_coverage_per_module_sourcelabreport
#percent_gxalert_connected_sourcelabreport

# Calculate centroids of each region for bubble placement
#colors: #889eec #002a80 (blue)

PAKshp_region_centroids <- PAKshp_region %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(region = PAKshp_region$region, 
         genexpert_sites_connected = PAKshp_region$genexpert_sites_connected,
         yearly_utilization_percent = PAKshp_region$yearly_utilization_percent,
         population_coverage_per_module_sourcelabreport = PAKshp_region$population_coverage_per_module_sourcelabreport,
         avg_population_per_centre = PAKshp_region$avg_population_per_centre) 

#merge with PAKprov_estimates
PAKshp_region_centroids <- PAKshp_region_centroids %>%
  left_join(PAKprov_estimates, by = c("region" = "province"))

#remove % from missed_cases_percent:
PAKshp_region_centroids$missed_cases_percent <- as.numeric(gsub("%", "", PAKshp_region_centroids$missed_cases_percent))


# Plot with bubbles, labels, and region names
map9c <- ggplot(PAKshp_region) +
  geom_sf(fill = "#ede0d4", color = "#432818", linewidth = 0.1) +  # Base map
  # Bubbles for GenXpert sites
  geom_point(data = PAKshp_region_centroids, aes(X, Y, size = missed_cases_percent), 
             color = "#889eec", alpha = 0.5) +  
  # Labels for GenXpert site counts
  geom_text(data = PAKshp_region_centroids, aes(X, Y, label = missed_cases_percent), 
            size = 4, color = "black", fontface = "bold") +  
  # Labels for region names (non-overlapping)
  geom_text_repel(data = PAKshp_region_centroids, aes(X, Y, label = region),  
                  size = 5, color = "#002a80", fontface = "bold",
                  box.padding = 0.5, point.padding = 3, force = 0.5) +  
  # Remove legend
  scale_size(range = c(5, 12), guide = "none") +
  #add title
  labs(title = "missed_cases_percent 2022 by Province") +
  theme_dspp() +
  theme(
    plot.title = element_text(size = 20, family = "roboto"))

map9c

ggsave("PAKISTAN_case_study_analysis/figures/map9c_missed_cases_percent.png", width = 10, height = 10, units = "cm", dpi = 300)


##### FIGURE 10: Bubble plot: Case Detection Rate vs Utilisation Rate (sized by number of GenX)  ---------------------------------------------------

#reading estimates data 2022 (lab report): https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_estimates_notifications_2022.csv
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_estimates_notifications_2022.csv"
PAKprov_estimates <- read_csv(url)


#Drop pakistan, and rename: Azad Jammu & Kashmir to Azad Jammu and Kashmir, Khyber Pakhtunkhwa to Khyber Pakhtun Khwa; 	Islamabad & CT to Islamabad
PAKprov_estimates <- PAKprov_estimates %>%
  select(-year) %>%
  filter(province != "Pakistan") %>% 
  mutate(province = recode(province,
                         "Khyber Pakhtunkhwa" = "Khyber Pakhtun Khwa",
                         "Azad Jammu & Kashmir" = "Azad Jammu and Kashmir",
                         "Islamabad & CT" = "Islamabad"))

#merge with PAKshp_region shapefile
PAKshp_region <- PAKshp_region %>%
  left_join(PAKprov_estimates, by = c("region" = "province"))

#trasnform case_detection_rate from 47% to 47
PAKshp_region$case_detection_rate <- as.numeric(gsub("%", "", PAKshp_region$case_detection_rate))
PAKshp_region$ppm_contribution <- as.numeric(gsub("%", "", PAKshp_region$ppm_contribution))

#bubble plot with case_detection_rate vs yearly_utilization_percent sized by genexpert_sites_connected
#bubble plot with case_detection_rate vs ppm_contribution sized by genexpert_sites_connected

fig10 <- ggplot(PAKshp_region, aes(x = case_detection_rate, y = ppm_contribution)) +
  geom_point(aes(size = genexpert_sites_connected), color = "#002a80", alpha = 0.6) +  # Bubbles sized by GenX sites
  geom_text_repel(aes(label = region), size = 3, color = "#002a80", fontface = "bold", 
                  box.padding = 0.5, point.padding = 0.5, force = 2) +  # Labels for provinces
  scale_size_continuous(range = c(3, 10), name = "GenXpert Sites Connected") +  # Adjust bubble sizes
  scale_x_continuous(name = "Case Detection Rate (CDR)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous(name = "ppm_contribution (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Case Detection Rate vs. ppm_contribution",
       subtitle = "Bubble size represents the number of GenXpert sites connected") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, family = "roboto"),
        plot.subtitle = element_text(size = 16, family = "roboto"),
        axis.title = element_text(size = 18, family = "roboto"),
        axis.text = element_text(size = 14, family = "roboto"),
        legend.title = element_text(size = 16, family = "roboto"),
        legend.text = element_text(size = 14, family = "roboto"),
        panel.background = element_rect(fill = "white", colour = NA))

fig10

ggsave("PAKISTAN_case_study_analysis/figures/fig10b_bubble_ppm_cdr_utilisation.png", width = 12, height = 8, units = "cm", dpi = 300)

##################################################################################
################## 5. Provinces TimeSeries ######################################
#################################################################################

#read url: https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_diagnosis_ts.csv
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/province_level/province_diagnosis_ts.csv"
PAKprov_ts <- read_csv(url)

# Create new variable: Total Tests per GenXpert Site
PAKprov_ts <- PAKprov_ts %>%
  mutate(tests_per_site = `Total Tests Done` / `Gxpert Sites`)

# Scatter plot without size scaling
fig11 <- ggplot(PAKprov_ts,aes(x = pmin(tests_per_site, 5000), y = `MTB Positive Rate (%)`, color = province)) +
  geom_point(size = 3, alpha = 0.7) +  # Fixed size, colored by province
  scale_x_continuous(name = "Total Tests per GenXpert Site") +
  scale_y_continuous(name = "MTB Positive Rate (%)", limits = c(0, 60), breaks = seq(0, 60, 10)) +
  labs(title = "Tests per GenXpert Site vs. MTB Positive Rate",
       subtitle = "Each point represents a province-year combination",
       caption = "Data from PAKprov_ts (2011-2022)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, family = "roboto"),
        plot.subtitle = element_text(size = 16, family = "roboto"),
        axis.title = element_text(size = 18, family = "roboto"),
        axis.text = element_text(size = 14, family = "roboto"),
        legend.title = element_text(size = 16, family = "roboto"),
        legend.text = element_text(size = 14, family = "roboto"))

fig11

ggsave("PAKISTAN_case_study_analysis/figures/fig11_scatter_tests_per_site.png", width = 12, height = 8, units = "cm", dpi = 300)


##################################################################################
################## 6. Other descriptive figures #################################
#################################################################################


#Figure 12: Time Series of MTB positity rate (private vs public) ---------------------------------------------------
url <- "https://raw.githubusercontent.com/carol02r/dspp_capstone_msf/refs/heads/main/PAKISTAN_cleaned_data_reports_papers/country_level/country_diagnosis_labreport_ts.csv"
PAKts <- read_csv(url)

#summary
summary(PAKts)

#remove % and turn numeric: mtb_positive_percent_public and mtb_positive_percent_private
PAKts$mtb_positive_percent_public <- as.numeric(gsub("%", "", PAKts$mtb_positive_percent_public))
PAKts$mtb_positive_percent_private <- as.numeric(gsub("%", "", PAKts$mtb_positive_percent_private))
PAKts$yearly_utilization_percent <- as.numeric(gsub("%", "", PAKts$yearly_utilization_percent))

#plot time series of both public and private mtb positivity rate
fig12 <- ggplot(PAKts, aes(x = year)) +
  geom_line(aes(y = mtb_positive_percent_public, color = "Public"), size = 0.5) +
  geom_line(aes(y = mtb_positive_percent_private, color = "Private"), size = 0.5) +
  
  # Add markers at data points
  geom_point(aes(y = mtb_positive_percent_public, color = "Public"), size = 1) +
  geom_point(aes(y = mtb_positive_percent_private, color = "Private"), size = 1) +
  
  # Custom colors: Blue for public, Red for private
  scale_color_manual(name = NULL, values = c("Public" = "#adc178", "Private" = "#ff9b5c")) +
  
  # Ensure x-axis is rounded & shows all years
  scale_x_continuous(name = NULL, 
                   limits = c(2014, max(PAKts$year)),  # Start at first data year
                   breaks = seq(2014, max(PAKts$year), 1),  # Show all years
                   expand = c(0, 0)) +  # Remove extra padding
  
  # Y-axis adjustments
  scale_y_continuous(name = "MTB Positive Rate (%)", limits = c(0, 60), breaks = seq(0, 60, 10)) +
  
  # Titles and styling
  labs(title = "Time Series of MTB Positive Rate (Public vs. Private)")+ 
  theme_dspp_ts() +
  theme(plot.title = element_text(size = 20, family = "roboto"),
        axis.title = element_text(size = 20, family = "roboto"),
        axis.text = element_text(size = 18, family = "roboto"))

fig12

ggsave("PAKISTAN_case_study_analysis/figures/fig12_ts_mtb_positivity_rate.png", width = 8, height = 6, units = "cm", dpi = 300)


#Figure 13: Time Series of genX available (private vs public) ---------------------------------------------------
#time series: total_genex_results, xpert_total_tests_public, xpert_total_tests_private

#plot time series of both public and private mtb positivity rate
fig13 <- ggplot(PAKts, aes(x = year)) +
  geom_line(aes(y = xpert_total_tests_public, color = "Public"), size = 0.5) +
  geom_line(aes(y = xpert_total_tests_private, color = "Private"), size = 0.5) +
  
  
  # Add markers at data points
  geom_point(aes(y = xpert_total_tests_public, color = "Public"), size = 1) +
  geom_point(aes(y = xpert_total_tests_private, color = "Private"), size = 1) +
  
  # Custom colors: Blue for public, Red for private
  scale_color_manual(name = NULL, values = c("Public" = "#adc178", "Private" = "#ff9b5c")) +
  
  # Ensure x-axis is rounded & shows all years
  scale_x_continuous(name = NULL, 
                     limits = c(2014, max(PAKts$year)),  # Start at first data year
                     breaks = seq(2014, max(PAKts$year), 1),  # Show all years
                     expand = c(0, 0)) +  # Remove extra padding
  
  # Y-axis adjustments
  scale_y_continuous(name = "Xpert Total Tests (in thousands)", 
                     limits = c(0, 600000),  # Adjust based on your max value
                     breaks = seq(0, 600000, 100000),  # Set intervals in thousands
                     labels = scales::label_comma(scale = 0.001)) + # Convert to thousands
  
  # Titles and styling
  labs(title = "Time Series of GenX Tests Performed (Public vs. Private)")+ 
  theme_dspp_ts() +
  theme(plot.title = element_text(size = 20, family = "roboto"),
        axis.title = element_text(size = 20, family = "roboto"),
        axis.text = element_text(size = 18, family = "roboto"))

fig13

ggsave("PAKISTAN_case_study_analysis/figures/fig13_ts_mtb_positivity_rate.png", width = 8, height = 6, units = "cm", dpi = 300)


#Figure 14: Time Series of genX machines ---------------------------------------------------
#time series: xpert_machines

#plot time series of both public and private mtb positivity rate
fig14 <- ggplot(PAKts, aes(x = year)) +
  geom_line(aes(y = xpert_machines), color ="#2c4fab", size = 0.5) +
  geom_point(aes(y = xpert_machines), color = "#2c4fab", size = 1) +
  
  # Ensure x-axis is rounded & shows all years
  scale_x_continuous(name = NULL, 
                     limits = c(2011, max(PAKts$year)),  # Start at first data year
                     breaks = seq(2011, max(PAKts$year), 1),  # Show all years
                     expand = c(0, 0)) +  # Remove extra padding

  # Y-axis adjustments
  scale_y_continuous(name = "GenX Machines", 
                     limits = c(0, 500),  # Adjust based on your max value
                     breaks = seq(0, 500, 100)) +  # Set intervals in thousands
  
  # Titles and styling
  labs(title = "Time Series of GenX Machines Available in Pakistan")+ 
  theme_dspp_ts() +
  theme(plot.title = element_text(size = 20, family = "roboto"),
        axis.title = element_text(size = 20, family = "roboto"),
        axis.text = element_text(size = 18, family = "roboto"),
        axis.text.x = element_text(angle = 45, hjust = 1))

fig14

ggsave("PAKISTAN_case_study_analysis/figures/fig14_ts_genxmachines.png", width = 8, height = 6, units = "cm", dpi = 300)


#Figure 15: Time Series of genX utilisation rate ---------------------------------------------------
  
fig15 <- ggplot(PAKprov_ts, aes(x = year, y = tests_per_site, color = province)) +
    geom_line(size = 0.5) +  # One line per province
    geom_point(size = 1) +  # Add markers for each point
  
    scale_color_brewer(palette = "Pastel1", name = "Province") +
    
    # Ensure x-axis is rounded & shows all years
    scale_x_continuous(name = NULL, 
                       limits = c(2016, max(PAKprov_ts$year)),  # Start at first data year
                       breaks = seq(2016, max(PAKprov_ts$year), 1),  # Show all years
                       expand = c(0, 0)) +
    
    # Y-axis adjustments
  scale_y_continuous(name = "Average Total Tests By GenX Site",
                     limits = c(0, 1500),  # Set a limit slightly above the 3rd quartile for better visualization
                     breaks = seq(0, 1500, 300),  # Breaks every 300 tests
                     labels = scales::label_comma())  + # No scaling; display as raw numbers
    
    
    # Titles and styling
    labs(title = " Yearly Average Total Tests By GenX Site (Thousands)") + 
    theme_dspp_ts() +
    theme(plot.title = element_text(size = 20, family = "roboto"),
          axis.title = element_text(size = 20, family = "roboto"),
          axis.text = element_text(size = 18, family = "roboto"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),  # Remove legend title
          legend.text = element_text(size = 16))
  
fig15

ggsave("PAKISTAN_case_study_analysis/figures/fig15_ts_genxmachines_prov.png", width = 12, height = 8, units = "cm", dpi = 300)

