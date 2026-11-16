proj_name <- "Example Project"
project_ID <- "12345"


## Define directory with treatment shapefiles in it
### This can be located anywhere on your computer. It doesn't need to be in the same folder as this code
### This folder should ONLY include shapefiles for the project you want to run right now. All shapefiles in the folder will be analyzed.
trt_dir <- r"(C:/Path/To/Your/Project/Shapefiles/Folder)"

### Define which treatments are site prep, biomass utilization, or other treatment types that do not require FVS inputs.
### Note: Do not include reforestation treatments in `noFVS`. Even though FVS is not run on them, the FVS input files are used to determine species of regenerating conifers.
noFVS <- c()

## Define whether you want to split up disparate polygons (default is yes)
separate_tcns <- "no" # options = "yes", "no"

## Define which treatments get combined for FVS modeling
## If you only have one treatment polygon, leave as empty sets: `c()` 
combine_tcns <- c() #example: c("1new","2new","5new")
combine_tcns_2 <- c() #example: c("3new","4new")

### The next two parameters only need to be edited if the project includes reforestation
###################################################################

## If there's post-fire reforestation, define the year of the fire 
### e.g. fire_year <- 2018
fire_year <- 9999

## Define the name of the reforestation shapefile (without the full path)
## This is only needed for reforestation runs. It should end in ".shp"
rf_shp_name <- "Reforestation shapefile.shp"

### In most cases, you can leave the below lines as they are
###################################################################

### Define which year of TreeMap you want to use (only use older years for retroactive QM runs)
tm <- 2022 # Options = 2016, 2020, 2022

### Define "data" directory 
### This is where important reference files are, like TreeMap rasters, FVS variant maps, etc.
data_dir <- "../../data"

### Define input and output directories
temp_dir <- "../../areas"
final_polygons_dir <- "../../final_polygons"
triaads_output_dir <- "../../triaads_input"
output_dir_randig <- "../../randig_shapefiles"
fvs_input_dir <- "../../FVS_Input"
fvs_output_dir = "../../FVS_Output/"

# Define paths and file locations within data directory 
## If you got the "/data" folder from Carmen in its entirety, you shouldn't have to mess with this section)
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