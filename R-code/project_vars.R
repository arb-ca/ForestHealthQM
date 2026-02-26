# HI JASON

proj_name <- "Cole and Evans"
project_ID <- "8GG25305"

### Define which treatments are site prep, biomass utilization, or other treatment types that do not require FVS inputs.
noFVS <- c()
### Note: Do not include reforestation treatments in `noFVS`. Even though FVS is not run on them, the FVS input files are used to determine species of regenerating conifers.

### Define which year of TreeMap you want to use
tm <- 2022 # Options = 2016, 2020, 2022

## Define directory with treatment shapefiles in it
## This can be located anywhere on your computer. It doesn't need to be in the same folder as this code
trt_dir <- "C:/Users/ctubbesi/OneDrive - California Air Resources Board/Documents/CCI/QMs/Forest Health QM/QM Runs 2025/CFIP/8GG25305 Cole and Evans/shapefile"

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

### Define impact area buffer distance, in meters (don't change this)
buffer_dist <- 500