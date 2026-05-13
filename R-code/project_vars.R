proj_name <- "Middletown And Cobb Community Evacuation Routes - Phase 2"
project_ID <- "25-WP-LNU-79252605"


### Define which treatments are site prep, biomass utilization, or other treatment types that do not require FVS inputs.
noFVS <- c()

## Define which treatments get combined for FVS modeling
## If you only have one treatment polygon, leave this as-is
combine_tcns <- c("5new", "6new", "7new", "8new") #example: c("1new","2new","5new")
combine_tcns_2 <- c()

### Note: Do not include reforestation treatments in `noFVS`. Even though FVS is not run on them, the FVS input files are used to determine species of regenerating conifers.

### Define which year of TreeMap you want to use
tm <- 2022 # Options = 2016, 2020, 2022

## If there's post-fire reforestation, define the year of the fire 
fire_year <- 2018
## Define the name of the reforestation shapefile (without the full path)
## This is only needed for reforestation runs
rf_shp_name <- "71625010_1_1_20250114.shp"

## Define directory with treatment shapefiles in it
## This can be located anywhere on your computer. It doesn't need to be in the same folder as this code
trt_dir <- "C:/Users/ctubbesi/OneDrive - California Air Resources Board/Documents/CCI/QMs/Forest Health QM/QM Runs 2026/WPGP/Shapefiles/25_WP_LNU_79252605"


### Define "data" directory 
data_dir <- "../../data"

### Define input and output directories
temp_dir <- "../../temp"
triaads_output_dir <- "../../triaads_input"
output_dir_randig <- "../../randig_shapefiles"
fvs_input_dir <- "../../FVS_Input"
fvs_output_dir = "../../FVS_Output/"


# Define things within data directory (you shouldn't have to mess with this section)
ca_dir <- paste0(data_dir, "/ca-state-boundary")
treemap_dir <- paste0(data_dir, "/TreeMap")
fvs_varloc_shp_path <- paste0(data_dir, "/FVSVariantMap20210525/FVS_Variants_and_Locations.shp")
blank_db <- file.path(data_dir, "/BlankDatabase.xlsx")
wf_path <- file.path(data_dir, "/FSIM/fsim_mean.tif")
rf_shp_path <- file.path(trt_dir, rf_shp_name)
ps_path <- paste0(data_dir, "/PostCRPT/conifer_regen_prob_", fire_year, ".tif")
species_path <- file.path(data_dir, "TreeMap/species_codes.xlsx")
  
### Define impact area buffer distance, in meters (don't change this)
buffer_dist <- 500