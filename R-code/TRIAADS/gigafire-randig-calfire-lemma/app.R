required_packages <- c("shiny", "sf","readxl", "leaflet", "DT", "terra", "dplyr", "data.table","zip")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  installed <- rownames(installed.packages())
  for (pkg in packages) {
    if (!(pkg %in% installed)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

# Install missing packages
install_missing_packages(required_packages)

library(shiny)
library(sf)
library(leaflet)
library(DT)
library(terra)
library(dplyr)
library(data.table)
library(readxl)
sf_use_s2(FALSE)

# Define function for calculating mode
Mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ui <- fluidPage(
  titlePanel("TRIAADS: Treatment Impact Area Analysis and Decision Support"),
  
  tabsetPanel(
   tabPanel("Setup & Data Preparation",
  h4("Setup Parameters"),
  fileInput("treatment_file", "Upload Treatment Shapefile (zip containing .shp, .shx, .dbf)", 
            accept = c(".zip")),
  fileInput("ia_file", "Upload Impact Area Shapefile (zip containing .shp, .shx, .dbf)", 
            accept = c(".zip")),
  textOutput("file_status"),
  numericInput("extent_expansion", 
         label = "Extent Expansion (%)", 
         value = 25, 
         min = 0, 
         max = 200, 
         step = 5),
  actionButton("prepare_data", "Prepare Data"),
  br(),
  leafletOutput("map")
),
    
 tabPanel("Prepare Landscapes & Run RANDIG",
  h4("Prepare Landscapes and Run RANDIG"),
  fileInput("fvs_data_files", "Upload Treatment and IA Data Files (2 xlsx files)", 
    accept = c(".xlsx", ".xls"),
    multiple = TRUE),
  textOutput("file_identification_status"),
  uiOutput("year_selector"),
      # Buttons for preparing landscapes and running RANDIG
      actionButton("prep_landscape", "Prepare Landscape Inputs"),
      verbatimTextOutput("error_message"),
      
      # Display progress
      textOutput("landscape_progress"),
      textOutput("randig_status"),
      
      # Plot inputs and results
      h4("Control Landscape"),
      plotOutput("landscape_control_plot"),
      h4("Treatment Landscape"),
      plotOutput("landscape_treat_plot"),
downloadButton("download_control_inputs", "Download Control Inputs (.tif)"),
      downloadButton("download_treatment_inputs", "Downlo      ad Treatment Inputs (.tif)"),
      h4("Customize Fuel Moistures"),
      numericInput("fuel_moisture_1hr", "1-hr Dead Fuel Moisture Content (%)", value = 3, min = 2, max = 300),
      numericInput("fuel_moisture_10hr", "10-hr Dead Fuel Moisture Content (%)", value = 4, min = 2, max = 300),
      numericInput("fuel_moisture_100hr", "100-hr Dead Fuel Moisture Content (%)", value = 5, min = 2, max = 300),
      numericInput("fuel_moisture_live_herb", "Live Herbaceous Fuel Moisture Content (%)", value = 60, min = 2, max = 300),
      numericInput("fuel_moisture_live_woody", "Live Woody Fuel Moisture Content (%)", value = 90, min = 2, max = 300),
      h4("Customize Wind Settings"),
      numericInput("wind_speed", "Wind Speed (mph)", value = 20, min = 0, max = 100),
      numericInput("wind_direction", "Wind Direction (degrees)", value = 270, min = 0, max = 360),

      actionButton("run_randig_control", "Run RANDIG for Control"),
      h4("Control Randig Outputs"),
      plotOutput("randig_output_control_plot"),
      actionButton("run_randig_treatment", "Run RANDIG for Treatment"),
      h4("Treatment Randig Outputs"),
      plotOutput("randig_output_treatment_plot"),
       downloadButton("download_randig_control",   "Download RANDIG Control Output (.tif)"),
      downloadButton("download_randig_treatment", "Download RANDIG Treatment Output (.tif)")

    ),
    
    tabPanel("Analysis",
      h4("Output Analysis"),
      actionButton("run_analysis", "Run Analysis"),
      DTOutput("analysis_table"),
      downloadButton("download_csv", "Download CSV")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  options(shiny.maxRequestSize=200*1024^2) 
  
  # Reactive values for storing necessary data
 rv <- reactiveValues(progress_msg = "", treat = NULL, ia = NULL, 
                     landscape_ready = FALSE, landscape_control = NULL, 
                     landscape_treat = NULL, identified_files = NULL)
  
  # Observe file upload and read in treatment shapefile
  # Observe treatment file upload
# Observe treatment file upload
observeEvent(input$treatment_file, {
  req(input$treatment_file)
  
  tryCatch({
    # Create unique temp directory for treatment
    temp_dir <- file.path(tempdir(), "treatment_upload")
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir)
    
    unzip(input$treatment_file$datapath, exdir = temp_dir)
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) == 0) {
      stop("No shapefile (.shp) found in the treatment zip archive.")
    }
    
    rv$treat <- st_read(shp_files[1], quiet = TRUE)
    rv$treat <- st_zm(rv$treat, drop = TRUE, what = "ZM")
    rv$treat <- st_make_valid(rv$treat)
    
    # Transform to EPSG:4326 for leaflet
    if (st_crs(rv$treat) != 4326) {
      rv$treat <- st_transform(rv$treat, crs = 4326)
    }
    
    cat(file = stderr(), "✓ Treatment shapefile loaded: ", shp_files[1], "\n")
    cat(file = stderr(), "  Features: ", nrow(rv$treat), "\n")
    cat(file = stderr(), "  Bbox: ", paste(st_bbox(rv$treat), collapse = ", "), "\n")
    
  }, error = function(e) {
    cat(file = stderr(), "✗ Error loading treatment file: ", e$message, "\n")
    rv$treat <- NULL
  })
})

# Observe IA file upload
observeEvent(input$ia_file, {
  req(input$ia_file)
  
  tryCatch({
    # Create unique temp directory for IA
    temp_dir <- file.path(tempdir(), "ia_upload")
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir)
    
    unzip(input$ia_file$datapath, exdir = temp_dir)
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) == 0) {
      stop("No shapefile (.shp) found in the IA zip archive.")
    }
    
    rv$ia <- st_read(shp_files[1], quiet = TRUE)
    rv$ia <- st_zm(rv$ia, drop = TRUE, what = "ZM")
    rv$ia <- st_make_valid(rv$ia)
    
    # Transform to EPSG:4326 for leaflet
    if (st_crs(rv$ia) != 4326) {
      rv$ia <- st_transform(rv$ia, crs = 4326)
    }
    
    cat(file = stderr(), "✓ IA shapefile loaded: ", shp_files[1], "\n")
    cat(file = stderr(), "  Features: ", nrow(rv$ia), "\n")
    cat(file = stderr(), "  Bbox: ", paste(st_bbox(rv$ia), collapse = ", "), "\n")
    
  }, error = function(e) {
    cat(file = stderr(), "✗ Error loading IA file: ", e$message, "\n")
    rv$ia <- NULL
  })
})
# Prepare Data button - create extent and render map
# Prepare Data button - create extent and render map
# Prepare Data button - create extent and render map
# Prepare Data button - create extent and render map
# Prepare Data button - create extent and render map
# Prepare Data button - create extent and render map

observeEvent(input$prepare_data, {
  req(rv$treat, rv$ia)
  
  tryCatch({
    cat(file = stderr(), "Preparing data and calculating extent...\n")
    
    # Ensure both are in EPSG:4326
    if (st_crs(rv$treat)$epsg != 4326) {
      rv$treat <- st_transform(rv$treat, crs = 4326)
    }
    if (st_crs(rv$ia)$epsg != 4326) {
      rv$ia <- st_transform(rv$ia, crs = 4326)
    }
    
    # Create simplified versions using st_combine and extracting boundary
    treat_combined <- rv$treat %>% 
      st_make_valid() %>% 
      st_geometry() %>%
      st_combine() %>%
      st_union()  # Apply union to the combined geometry
    
    # Get the outer boundary and create a new polygon from it
    treat_plot <- treat_combined %>%
      st_boundary() %>%  # Extract boundary lines
      st_polygonize() %>%  # Convert back to polygon
      st_collection_extract("POLYGON") %>%
      st_sf()
    
    ia_combined <- rv$ia %>% 
      st_make_valid() %>% 
      st_geometry() %>%
      st_combine() %>%
      st_union()
    
    ia_plot <- ia_combined %>%
      st_boundary() %>%
      st_polygonize() %>%
      st_collection_extract("POLYGON") %>%
      st_sf()
    
    cat(file = stderr(), "Created simplified plotting polygons.\n")
    
    # Get extents separately and combine
    treat_vect <- vect(rv$treat)
    ia_vect <- vect(rv$ia)
    
    treat_extent <- ext(treat_vect)
    ia_extent <- ext(ia_vect)
    
    # Create combined extent
    combined_extent <- ext(
      min(treat_extent$xmin, ia_extent$xmin),
      max(treat_extent$xmax, ia_extent$xmax),
      min(treat_extent$ymin, ia_extent$ymin),
      max(treat_extent$ymax, ia_extent$ymax)
    )
    
    # Calculate expanded extent
    expansion_factor <- input$extent_expansion / 100 / 2
    width_buffer <- (combined_extent$xmax - combined_extent$xmin) * expansion_factor
    height_buffer <- (combined_extent$ymax - combined_extent$ymin) * expansion_factor
    
    expanded_extent <- ext(
      combined_extent$xmin - width_buffer,
      combined_extent$xmax + width_buffer,
      combined_extent$ymin - height_buffer,
      combined_extent$ymax + height_buffer
    )
    
    # Convert extent to sf object
    spat_poly <- as.polygons(expanded_extent)
    crs(spat_poly) <- "EPSG:4326"
    rv$ext <- st_as_sf(spat_poly)
    
    cat(file = stderr(), "Extent calculated successfully.\n")
    
    # Render the map using simplified plotting polygons
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addPolygons(data = treat_plot, color = "blue", weight = 2, 
                    fillOpacity = 0.3, fillColor = "blue",
                    group = "Treatment", label = "Treatment Area") %>%
        addPolygons(data = ia_plot, color = "red", weight = 2, 
                    fillOpacity = 0.3, fillColor = "red",
                    group = "IA", label = "Impact Area") %>%
        addPolygons(data = rv$ext, color = "pink", weight = 2, 
                    fillOpacity = 0.1, fillColor = "pink",
                    group = "Extent", label = "Analysis Extent")
    })
    
    output$file_status <- renderText("Data prepared successfully. Map displayed below.")
    cat(file = stderr(), "Map rendered successfully.\n")
    
  }, error = function(e) {
    output$file_status <- renderText(paste("Error preparing data:", e$message))
    cat(file = stderr(), "Error in prepare_data: ", e$message, "\n")
  })
})
  # Prepare Landscape Inputs

read_data <- function(file) {
  ext <- tools::file_ext(file$datapath)
  if (ext == "csv") data.table::fread(file$datapath)
  else              readxl::read_excel(file$datapath)
}

# Function to identify and organize uploaded files
identify_fvs_files <- function(uploaded_files) {
  if (is.null(uploaded_files) || nrow(uploaded_files) != 2) {
    return(list(error = "Please upload exactly 2 files (Treatment and IA data)"))
  }
  
  file_list <- list(
    treatment_data = NULL,
    ia_data = NULL
  )
  
  for (i in 1:nrow(uploaded_files)) {
    filename <- tolower(uploaded_files$name[i])
    
    # Identify file type based on filename
    if (grepl("_ia_", filename)) {
      file_list$ia_data <- uploaded_files[i, ]
    } else if (grepl("_treatment_", filename)) {
      file_list$treatment_data <- uploaded_files[i, ]
    } else {
      return(list(error = paste("Could not identify file:", uploaded_files$name[i], 
                                "(must contain '_IA_' or '_treatment_')")))
    }
  }
   if (any(sapply(file_list, is.null))) {
    missing <- names(file_list)[sapply(file_list, is.null)]
    return(list(error = paste("Missing files:", paste(missing, collapse = ", "))))
  }
  
  return(file_list)
}
# Observer to identify uploaded files
observeEvent(input$fvs_data_files, {
  cat(file = stderr(), "=== FILE UPLOAD TRIGGERED ===\n")
  req(input$fvs_data_files)
  
  cat(file = stderr(), "Number of files uploaded:", nrow(input$fvs_data_files), "\n")
  cat(file = stderr(), "File names:", paste(input$fvs_data_files$name, collapse = ", "), "\n")
  
  identified <- identify_fvs_files(input$fvs_data_files)
  
  if (!is.null(identified$error)) {
    cat(file = stderr(), "Error identifying files:", identified$error, "\n")
    output$file_identification_status <- renderText({
      paste("Error:", identified$error)
    })
    rv$identified_files <- NULL
  } else {
    cat(file = stderr(), "Files identified successfully!\n")
    rv$identified_files <- identified
    output$file_identification_status <- renderText({
      paste("✓ Files identified successfully:",
            "\n- Treatment Data:", identified$treatment_data$name,
            "\n- IA Data:", identified$ia_data$name)
    })
    cat(file = stderr(), "rv$identified_files set, should trigger year selector observer\n")
  }
})

# — 1) Dynamic Year Selector —
# Dynamic Year Selector
observe({
  req(rv$identified_files)
  
  tryCatch({
    # Load both files
    treatment_data <- read_data(rv$identified_files$treatment_data)
    ia_data <- read_data(rv$identified_files$ia_data)
    
    # Check if Year column exists
    if (!"Year" %in% names(treatment_data)) {
      stop("Treatment data does not have a 'Year' column")
    }
    if (!"Year" %in% names(ia_data)) {
      stop("IA data does not have a 'Year' column")
    }
    
    # Get years from both datasets
    treatment_years <- unique(treatment_data$Year)
    ia_years <- unique(ia_data$Year)
    
    # Get common years
    if (length(treatment_years) == 0 || length(ia_years) == 0) {
      common_years <- c()
    } else {
      common_years <- intersect(treatment_years, ia_years)
    }

    output$year_selector <- renderUI({
      if (length(common_years) == 0) {
        p(style = "color: red;", 
          "Error: No common years found between treatment and IA data files. Please check that both files have a 'Year' column with matching values.")
      } else {
        selectInput(
          inputId  = "year",
          label    = "Select Analysis Year:",
          choices  = sort(common_years),
          selected = max(common_years)
        )
      }
    })
    
  }, error = function(e) {
    output$year_selector <- renderUI({
      p(style = "color: red;", paste("Error loading data:", e$message))
    })
  })
})





# Prepare Landscape Inputs
observeEvent(input$prep_landscape, {

  req(rv$identified_files, rv$ia, rv$treat, rv$ext, input$year)
  
  output$landscape_progress <- renderText("Loading and processing input files...")

  tryCatch({
    # Load FVS data
    cat("Loading FVS data files...\n")
    treatment_data <- read_data(rv$identified_files$treatment_data)
    ia_data <- read_data(rv$identified_files$ia_data)
    
    cat("Treatment data rows:", nrow(treatment_data), "\n")
    cat("IA data rows:", nrow(ia_data), "\n")
    cat("Treatment data columns:", paste(names(treatment_data), collapse = ", "), "\n")
    cat("IA data columns:", paste(names(ia_data), collapse = ", "), "\n")
    
    columns_to_rasterize <- c("Fuel_Mod1", "Total_Cover", "Stratum_1_Nom_Ht", "Canopy_Ht", "Canopy_Density")

    # Check if selected year is available
    if (!(input$year %in% unique(treatment_data$Year)) || 
        !(input$year %in% unique(ia_data$Year))) {
      stop("Selected year is not available in both datasets.")
    }
    
    cat("Selected year:", input$year, "\n")
    
    # Load landfire raster stack
    cat("Loading landfire raster data...\n")
    landfire_stack <- terra::rast("./data/base_data/cal_landfire.tif", 
                                   win = ext(st_transform(rv$ext, crs = "EPSG:26911")))
    cat("Landfire raster loaded successfully!\n")

    # Check available MgmtIDs
    cat("Treatment data MgmtIDs:", paste(unique(treatment_data$MgmtID), collapse = ", "), "\n")
    cat("IA data MgmtIDs:", paste(unique(ia_data$MgmtID), collapse = ", "), "\n")

    # Prepare IA baseline data (IANF)
    cat("Preparing IA baseline data (IANF)...\n")
    ia_baseline <- ia_data %>%
      filter(MgmtID == "IANF", Removal_Code == 1, Year == input$year)
    
    cat("IA baseline rows after filter:", nrow(ia_baseline), "\n")
    
    if (nrow(ia_baseline) == 0) {
      stop("No IA baseline data found for IANF, Removal_Code==1, Year=", input$year)
    }
    
    ia_baseline <- ia_baseline %>%
      select(all_of(c("StandID", columns_to_rasterize))) %>%
      mutate(StandID = as.numeric(StandID),
             Canopy_Density = Canopy_Density * 100, 
             Canopy_Ht = Canopy_Ht * 0.304 * 10,
             Stratum_1_Nom_Ht = Stratum_1_Nom_Ht * 0.304 * 10)
    
    cat("IA baseline data prepared! Rows:", nrow(ia_baseline), "\n")
    print(head(ia_baseline))

    # Prepare treatment area baseline data (BSNF)
    cat("Preparing treatment baseline data (BSNF)...\n")
    treat_baseline <- treatment_data %>%
      filter(MgmtID == "BSNF", Removal_Code == 1, Year == input$year)
    
    cat("Treatment baseline rows after filter:", nrow(treat_baseline), "\n")
    
    if (nrow(treat_baseline) == 0) {
      stop("No treatment baseline data found for BSNF, Removal_Code==1, Year=", input$year)
    }
    
    treat_baseline <- treat_baseline %>%
      select(all_of(c("StandID", columns_to_rasterize))) %>%
      mutate(StandID = as.numeric(StandID),
             Canopy_Density = Canopy_Density * 100, 
             Canopy_Ht = Canopy_Ht * 0.304 * 10,
             Stratum_1_Nom_Ht = Stratum_1_Nom_Ht * 0.304 * 10)
    
    cat("Treatment baseline data prepared! Rows:", nrow(treat_baseline), "\n")
    print(head(treat_baseline))

    # Prepare treatment scenario data (TRNF)
    cat("Preparing treatment scenario data (TRNF)...\n")
    treat_scenario <- treatment_data %>%
      filter(MgmtID == "TRNF", Removal_Code == 1, Year == input$year)
    
    cat("Treatment scenario rows after filter:", nrow(treat_scenario), "\n")
    
    if (nrow(treat_scenario) == 0) {
      stop("No treatment scenario data found for TRNF, Removal_Code==1, Year=", input$year)
    }
    
    treat_scenario <- treat_scenario %>%
      select(all_of(c("StandID", columns_to_rasterize))) %>%
      mutate(StandID = as.numeric(StandID),
             Canopy_Density = Canopy_Density * 100,
             Canopy_Ht = Canopy_Ht * 0.304 * 10,
             Stratum_1_Nom_Ht = Stratum_1_Nom_Ht * 0.304 * 10)
    
    cat("Treatment scenario data prepared! Rows:", nrow(treat_scenario), "\n")
    print(head(treat_scenario))

    # Transform polygons to landfire CRS
    cat("Transforming polygons to landfire CRS...\n")
    treat_transformed <- st_transform(rv$treat, crs = crs(landfire_stack))
    ia_transformed <- st_transform(rv$ia, crs = crs(landfire_stack))

    # Rasterize the treatment and IA polygons using TM_ID
    cat("Rasterizing treatment polygon with TM_ID...\n")
    treat_tmid_raster <- rasterize(vect(treat_transformed), landfire_stack[[1]], 
                                    field = "TM_ID")
    
    cat("Rasterizing IA polygon with TM_ID...\n")
    ia_tmid_raster <- rasterize(vect(ia_transformed), landfire_stack[[1]], 
                                 field = "TM_ID")

    cat("TM_ID rasters created successfully!\n")

    # Create CONTROL stack (IANF for IA, BSNF for treatment area)
    cat("Creating control landscape stack...\n")
    control_stack <- landfire_stack
    
    # Rasterize IA baseline data
    reclass_rasters_ia_baseline <- list()
    for (i in seq_along(columns_to_rasterize)) {
      cat("Rasterizing IA baseline column:", columns_to_rasterize[i], "\n")
      reclass_matrix <- as.matrix(ia_baseline[, c("StandID", columns_to_rasterize[[i]])])
      reclass_rasters_ia_baseline[[i]] <- classify(ia_tmid_raster, reclass_matrix, others = NA)
    }
    reclass_stack_ia_baseline <- rast(reclass_rasters_ia_baseline)
    
    # Rasterize treatment baseline data
    reclass_rasters_treat_baseline <- list()
    for (i in seq_along(columns_to_rasterize)) {
      cat("Rasterizing treatment baseline column:", columns_to_rasterize[i], "\n")
      reclass_matrix <- as.matrix(treat_baseline[, c("StandID", columns_to_rasterize[[i]])])
      reclass_rasters_treat_baseline[[i]] <- classify(treat_tmid_raster, reclass_matrix, others = NA)
    }
    reclass_stack_treat_baseline <- rast(reclass_rasters_treat_baseline)
    
    # Combine: treatment baseline for treatment area, IA baseline for IA
    combined_control <- cover(reclass_stack_treat_baseline, reclass_stack_ia_baseline)
    
    # Create mask for any TM_ID
    has_tmid_control <- !is.na(cover(treat_tmid_raster, ia_tmid_raster))
    
    # Replace landfire values
    control_stack[[4:8]] <- ifel(has_tmid_control, combined_control, control_stack[[4:8]])
    names(control_stack) <- c("Elevation", "Slope", "Aspect", "Fuel model", "Canopy cover", 
                              "Canopy height", "Canopy base height", "Canopy bulk density")
    
    cat("Control landscape complete!\n")

    # Create TREATMENT stack (IANF for IA, TRNF for treatment area)
    cat("Creating treatment landscape stack...\n")
    treat_stack <- landfire_stack
    
    # Rasterize treatment scenario data
    reclass_rasters_treat_scenario <- list()
    for (i in seq_along(columns_to_rasterize)) {
      cat("Rasterizing treatment scenario column:", columns_to_rasterize[i], "\n")
      reclass_matrix <- as.matrix(treat_scenario[, c("StandID", columns_to_rasterize[[i]])])
      reclass_rasters_treat_scenario[[i]] <- classify(treat_tmid_raster, reclass_matrix, others = NA)
    }
    reclass_stack_treat_scenario <- rast(reclass_rasters_treat_scenario)
    
    # Combine: treatment scenario for treatment area, IA baseline for IA
    combined_treatment <- cover(reclass_stack_treat_scenario, reclass_stack_ia_baseline)
    
    # Create mask for any TM_ID
    has_tmid_treatment <- !is.na(cover(treat_tmid_raster, ia_tmid_raster))
    
    # Replace landfire values
    treat_stack[[4:8]] <- ifel(has_tmid_treatment, combined_treatment, treat_stack[[4:8]])
    names(treat_stack) <- c("Elevation", "Slope", "Aspect", "Fuel model", "Canopy cover", 
                            "Canopy height", "Canopy base height", "Canopy bulk density")
    
    cat("Treatment landscape complete!\n")

    # Save rasterized outputs
    writeRaster(control_stack, "data/randig_inputs/control_inputs.tif", overwrite = TRUE)
    writeRaster(treat_stack, paste0("data/randig_inputs/treatment", input$treatment_no, "_inputs.tif"), 
                overwrite = TRUE)

    cat("Raster files saved!\n")

    # Download handlers
    output$download_control_inputs <- downloadHandler(
      filename = function() {
        paste0("control_inputs.tif")
      },
      content = function(file) {
        src <- file.path("data", "randig_inputs", "control_inputs.tif")
        file.copy(src, file, overwrite = TRUE)
      }
    )

    output$download_treatment_inputs <- downloadHandler(
      filename = function() {
        paste0("treatment", input$treatment_no, "_inputs.tif")
      },
      content = function(file) {
        src <- file.path("data", "randig_inputs",
                         paste0("treatment", input$treatment_no, "_inputs.tif"))
        file.copy(src, file, overwrite = TRUE)
      }
    )

    # Display raster plots
    output$landscape_control_plot <- renderPlot({
      cat("Rendering control landscape plot...\n")
      plot(control_stack)
    })

    output$landscape_treat_plot <- renderPlot({
      cat("Rendering treatment landscape plot...\n")
      plot(treat_stack)
    })
    
    # Update progress message
    output$landscape_progress <- renderText("Landscape preparation complete.")
    cat("prep_landscape completed successfully!\n")
    
  }, error = function(e) {
    output$landscape_progress <- renderText(paste("Error:", e$message))
    output$error_message <- renderText(paste("Detailed error:", e$message))
    cat(file = stderr(), "Error in prep_landscape: ", e$message, "\n")
    print(traceback())
  })
})

  
  
  # Run RANDIG Control
observeEvent(input$run_randig_control, {
  output$randig_status <- renderText("Running RANDIG for Control...")

  # Validate inputs for fuel moistures
  validate(
    need(input$fuel_moisture_1hr >= 0 && input$fuel_moisture_1hr <= 300, "1-hr Dead Fuel Moisture must be between 2 and 300."),
    need(input$fuel_moisture_10hr >= 0 && input$fuel_moisture_10hr <= 300, "10-hr Dead Fuel Moisture must be between 2 and 300."),
    need(input$fuel_moisture_100hr >= 0 && input$fuel_moisture_100hr <= 300, "100-hr Dead Fuel Moisture must be between 2 and 300."),
    need(input$fuel_moisture_live_herb >= 0 && input$fuel_moisture_live_herb <= 300, "Live Herbaceous Fuel Moisture must be between 2 and 300."),
    need(input$fuel_moisture_live_woody >= 20 && input$fuel_moisture_live_woody <= 300, "Live Woody Fuel Moisture must be between 2 and 300.")
  )

  # Read the existing template.input file
  template_path <- "./data/template.input"
  file_content <- readLines(template_path)

  # Update landscape path
  file_content <- gsub("^Landscape:.*", "Landscape: ./data/randig_inputs/control_inputs.tif", file_content)

  # Generate the "Fuel Moistures" line
  fuel_moisture_line <- paste(
    0,
    input$fuel_moisture_1hr,
    input$fuel_moisture_10hr,
    input$fuel_moisture_100hr,
    input$fuel_moisture_live_herb,
    input$fuel_moisture_live_woody
  )

  # Replace the existing fuel moisture line
  moisture_index <- which(grepl("^0 \\d+ \\d+ \\d+ \\d+ \\d+", file_content))
  file_content[moisture_index] <- fuel_moisture_line

  # Update wind speed and direction
  file_content <- gsub("^WIND_SPEED:.*", paste("WIND_SPEED:", input$wind_speed), file_content)
  file_content <- gsub("^WIND_DIRECTION:.*", paste("WIND_DIRECTION:", input$wind_direction), file_content)

  # Write the updated content to the control input file
  output_path <- "./data/randig_inputs/control.input"
  writeLines(file_content, output_path)

  # Execute RANDIG for control
  executable <- "./FB_x64/bin/TestRandig.exe"
  input_file <- shQuote(output_path)
  output_dir <- shQuote(file.path("./data/randig_outputs/control"))
  system2(executable, args = c(input_file, output_dir, "2"))

  output$randig_status <- renderText("RANDIG Control run complete.")

  # Display control outputs
  control_outs <- rast("./data/randig_outputs/control_RandigOutputs.tif")
  output$randig_output_control_plot <- renderPlot({ plot(control_outs) })
})




  # Run RANDIG Treatment
observeEvent(input$run_randig_treatment, {
  output$randig_status <- renderText("Running RANDIG for Treatment...")

  # Read the updated control.input file as the base
  control_path <- "./data/randig_inputs/control.input"
  file_content <- readLines(control_path)

  # Modify the Landscape line for the treatment file
  file_content <- gsub("^Landscape:.*", paste0("Landscape: ./data/randig_inputs/treatment", input$treatment_no, "_inputs.tif"), file_content)

  # Add FireSizeList line
  firelist_path <- "./data/randig_outputs/control_FireSizeList.txt"
  firelist_line <- paste0("FireListFile: ", firelist_path)
  file_content <- append(file_content, firelist_line, after = which(grepl("^Landscape:", file_content)))

  # Comment out TargetBurnProportion line
  file_content <- gsub("^(TargetBurnProportion:.*)", "#\\1", file_content)

  # Write the updated content to the treatment.input file
  output_path <- paste0("./data/randig_inputs/treatment", input$treatment_no, ".input")
  writeLines(file_content, output_path)

  # Execute RANDIG for treatment
  executable <- "./FB_x64/bin/TestRandig.exe"
  input_file <- shQuote(output_path)
  output_dir <- shQuote(file.path(paste0("./data/randig_outputs/treatment", input$treatment_no)))
  system2(executable, args = c(input_file, output_dir, "2"))

  output$randig_status <- renderText("RANDIG Treatment run complete.")

  # Display treatment outputs
  treat_outs <- rast(paste0("./data/randig_outputs/treatment", input$treatment_no, "_RandigOutputs.tif"))
  output$randig_output_treatment_plot <- renderPlot({ plot(treat_outs) })
})

output$download_randig_control <- downloadHandler(
  filename = function() "control_RandigOutputs.tif",
  content = function(file)
    file.copy("data/randig_outputs/control_RandigOutputs.tif", file, overwrite = TRUE)
)
output$download_randig_treatment <- downloadHandler(
  filename = function() paste0("treatment", input$treatment_no, "_RandigOutputs.tif"),
  content = function(file)
    file.copy(paste0("data/randig_outputs/treatment", input$treatment_no, "_RandigOutputs.tif"),
              file, overwrite = TRUE)
)  
  # Analysis function
 observeEvent(input$run_analysis, {
  cat("\n==== RUN ANALYSIS STARTED ====\n")  # Confirm event is triggered
  rv$buffer <- rv$ia
  tryCatch({
    # Step 1: Check if raster files exist before loading
    control_raster_path <- "./data/randig_outputs/control_RandigOutputs.tif"
    treatment_raster_path <- paste0("./data/randig_outputs/treatment", input$treatment_no, "_RandigOutputs.tif")

    if (!file.exists(control_raster_path)) {
      stop("ERROR: Control raster file not found at: ", control_raster_path)
    }
    if (!file.exists(treatment_raster_path)) {
      stop("ERROR: Treatment raster file not found at: ", treatment_raster_path)
    }

    cat("Loading raster files...\n")
    control_outs <- mask(rast(control_raster_path),st_transform(rv$ia, crs = "EPSG:26911"))
    treat_outs <- mask(rast(treatment_raster_path),st_transform(rv$ia, crs = "EPSG:26911"))
    cat("Raster files loaded successfully!\n")

    # Step 2: Check if spatial data (rv$treat, rv$buffer) exists
    if (is.null(rv$treat)) stop("ERROR: rv$treat is NULL!")
    if (is.null(rv$buffer)) stop("ERROR: rv$buffer is NULL!")

    cat("Transforming spatial data...\n")
    rv$treat <- st_transform(rv$treat, crs(control_outs)) %>% st_union() %>% st_sf()
    rv$buffer <- st_transform(rv$buffer, crs(control_outs)) %>% st_union() %>% st_sf()
    cat("Spatial data transformation complete!\n")

    # Step 3: Check if raster layers exist
    if (nlyr(control_outs) < 8 || nlyr(treat_outs) < 8) {
      stop("ERROR: Expected at least 8 raster layers in each output, but found less.")
    }

    # Step 4: Define the areas with flame length > 8 feet
    cat("Filtering areas with flame length > 8ft...\n")
    baseline_8ft <- terra::ifel(control_outs[[8]] > 8, control_outs[[8]], NA)
    treat_8ft <- terra::ifel(treat_outs[[8]] > 8, treat_outs[[8]], NA)

   cat("Ensuring area calculations will work...\n")
    if (is.null(rv$buffer) || is.null(rv$treat)) stop("ERROR: Buffer or Treatment areas are NULL!")
    
    cat("Calculating area total...\n")
    area_total <- as.numeric(st_area(rv$buffer))  # Total area in square meters
    cat("Calculating area treat...\n")
    area_treatment <- as.numeric(st_area(rv$treat))
    #if (area_total <= 0 || area_treatment <= 0) stop("ERROR: Calculated area is zero or negative!")

    cat("Area calculations complete. Total Area:", area_total, "sq.m, Treatment Area:", area_treatment, "sq.m\n")

    # Step 6: Masking raster with buffer and treatment areas
    cat("Applying spatial masks...\n")
    baseline_8ft_buffmask <- mask(baseline_8ft, rv$buffer)
    treatment_8ft_buffmask <- mask(treat_8ft, rv$buffer)

    baseline_buffmask <- mask(control_outs[[8]], rv$buffer)
    treatment_buffmask <- mask(treat_outs[[8]], rv$buffer)

    # Step 7: Calculate areas of high flame length (>8ft)
    baseline_8ft_area <- sum(!is.na(baseline_8ft_buffmask[])) * 900
    treatment_8ft_area <- sum(!is.na(treatment_8ft_buffmask[])) * 900
    baseline_8ft_perc <- (baseline_8ft_area / area_total) * 100
    treatment_8ft_perc <- (treatment_8ft_area / area_total) * 100

    cat("Baseline Area (>8ft):", baseline_8ft_area, "sq.m\n")
    cat("Treatment Area (>8ft):", treatment_8ft_area, "sq.m\n")

    # Step 8: Compute canopy burn probability (CBP)
   baseline_buffmask<-mask(control_outs[[8]],rv$buffer)
  treatment_buffmask<-mask(treat_outs[[8]],rv$buffer)

  baseline_area <- sum(!is.na(baseline_buffmask[])) * 900
  treatment_area <- sum(!is.na(treatment_buffmask[])) * 900
  baseline_perc <- baseline_area / area_total * 100
  treatment_perc <- treatment_area / area_total * 100


  ia_baseline_CBP_8ft <- as.numeric(global(ifel(!is.na(baseline_8ft_buffmask), control_outs[[1]], NA), fun = "mean", na.rm = TRUE) * 100)
  ia_treatment_CBP_8ft <- as.numeric(global(ifel(!is.na(treatment_8ft_buffmask), treat_outs[[1]], NA), fun = "mean", na.rm = TRUE) * 100)
  ia_baseline_CBP <- as.numeric(global(ifel(!is.na(baseline_buffmask), control_outs[[1]], NA), fun = "mean", na.rm = TRUE) * 100)
  ia_treatment_CBP <- as.numeric(global(ifel(!is.na(treatment_buffmask), treat_outs[[1]], NA), fun = "mean", na.rm = TRUE) * 100)

  baseline_8ft_treatmask <- mask(baseline_8ft, rv$treat)
  treatment_8ft_treatmask <- mask(treat_8ft, rv$treat)

  baseline_8ft_area_ta <- sum(!is.na(baseline_8ft_treatmask[])) * 900
  treatment_8ft_area_ta <- sum(!is.na(treatment_8ft_treatmask[])) * 900
  baseline_8ft_perc_ta <- baseline_8ft_area_ta / area_treatment * 100
  treatment_8ft_perc_ta <- treatment_8ft_area_ta  / area_treatment * 100

  baseline_treatmask<-mask(control_outs[[8]],rv$treat)
  treatment_treatmask<-mask(treat_outs[[8]],rv$treat)

  baseline_area_ta <- sum(!is.na(baseline_treatmask[])) * 900
  treatment_area_ta <- sum(!is.na(treatment_treatmask[])) * 900
  baseline_perc_ta <- baseline_area_ta / area_treatment * 100
  treatment_perc_ta <- treatment_area_ta / area_treatment * 100

  ta_baseline_CBP_8ft <- as.numeric(global(mask(ifel(!is.na(baseline_8ft_treatmask), control_outs[[1]], NA), rv$treat), fun = "mean", na.rm = TRUE) * 100)
  ta_treatment_CBP_8ft <- as.numeric(global(mask(ifel(!is.na(treatment_8ft_treatmask), treat_outs[[1]], NA), rv$treat), fun = "mean", na.rm = TRUE) * 100)
   ta_baseline_CBP <- as.numeric(global(mask(ifel(!is.na(baseline_treatmask), control_outs[[1]], NA), rv$treat), fun = "mean", na.rm = TRUE) * 100)
  ta_treatment_CBP <- as.numeric(global(mask(ifel(!is.na(treatment_treatmask), treat_outs[[1]], NA), rv$treat), fun = "mean", na.rm = TRUE) * 100)
  

  cat("High severity BTA:", ta_baseline_CBP_8ft , "\n")
  cat("High severity TTA:", ta_treatment_CBP_8ft , "\n")
  # Create the analysis data frame
  Variables <- c(
    "Area (acres)",
    "Area Baseline Flame Length > 8 ft",
    "Area Treatment Flame Length > 8ft",
    "% Baseline Flame Length > 8 ft",
    "% Treatment Flame Length > 8 ft",
    "High Severity Area Baseline CBP",
    "High Severity Area Treatment CBP",
    "High Severity Area CBP ratio",
    "All Burned Area Baseline CBP",
    "All Burned Area Treatment CBP",
    "All Burned Area CBP ratio"
  )
  
Values_ia <- round(c(
    area_total / 4046.856422,  # Convert square meters to acres
    baseline_8ft_area/ 4046.856422,  # Convert square meters to acres
    treatment_8ft_area / 4046.856422,  # Convert square meters to acres
    baseline_8ft_perc,
    treatment_8ft_perc,
    ia_baseline_CBP_8ft,
    ia_treatment_CBP_8ft,
    round(ia_treatment_CBP_8ft/ia_baseline_CBP_8ft, 3),
    ia_baseline_CBP,
    ia_treatment_CBP,
    round(ia_treatment_CBP/ia_baseline_CBP, 3)),2)
  
  Values_t <- round(c(
    area_treatment / 4046.856422,  # Convert square meters to acres
    baseline_8ft_area_ta / 4046.856422,  # Convert square meters to acres
    treatment_8ft_area_ta / 4046.856422,  # Convert square meters to acres
    baseline_8ft_perc_ta,
    treatment_8ft_perc_ta,
    ta_baseline_CBP_8ft,
    ta_treatment_CBP_8ft,
    round(ta_treatment_CBP_8ft/ta_baseline_CBP_8ft, 3),
    ta_baseline_CBP,
    ta_treatment_CBP,
    round(ta_treatment_CBP/ta_baseline_CBP, 3)),2)
  
    rv$analysis_result <- data.frame(Variable = Variables, `Impact Area` = Values_ia, `Treated Area`=Values_t)  # Store in reactiveValues

    

    # Display the analysis table in the UI
    output$analysis_table <- renderDT({
      datatable(rv$analysis_result, options = list(pageLength = 14))
    })

    cat("==== RUN ANALYSIS COMPLETED SUCCESSFULLY ====\n")

  }, error = function(e) {
    # Print error messages if anything fails
    cat("ERROR OCCURRED: ", e$message, "\n")
  })
output$download_csv <- downloadHandler(
  filename = function() { "analysis_results.csv" },
  content = function(file) {
    req(rv$analysis_result)  # Ensure the analysis result exists
    write.csv(rv$analysis_result, file, row.names = FALSE)
  }
)
  })
}

# Helper functions
# Helper function to create the control input file
create_input_file <- function(input_landscape, output_path, template_file = NULL) {
  # Use uploaded template if available; otherwise, use default
  input_file <- if (!is.null(template_file)) template_file else "./data/template.input"
  
  # Read and modify template content
  file_content <- readLines(input_file)
  landscape_path <- paste0("Landscape: ", input_landscape)
  file_content[2] <- landscape_path
  writeLines(file_content, output_path)
}

# Helper function to create the treatment input file with FireList
create_input_file_firelist <- function(input_landscape, output_path, firelist, template_file = NULL) {
  # Use uploaded template if available; otherwise, use default
  input_file <- if (!is.null(template_file)) template_file else "./data/template.input"
  
  # Read and modify template content
  file_content <- readLines(input_file)
  landscape_path <- paste0("Landscape: ", input_landscape)
  fire_list_line <- paste0("FireListFile: ", firelist)
  target_burn_line <- "#TargetBurnProportion: 0.970"
  
  # Modify specific lines
  file_content[2] <- landscape_path
  file_content <- append(file_content, fire_list_line, after = 2)
  
  if (!any(grepl("#TargetBurnProportion:", file_content))) {
    file_content <- append(file_content, target_burn_line, after = length(file_content))
  }

  writeLines(file_content, output_path)
}


# Run the application
shinyApp(ui = ui, server = server)

