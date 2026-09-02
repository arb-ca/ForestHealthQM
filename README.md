README
================
2026-04-10

- [Downloading data and software](#downloading-data-and-software)
- [FVS Input values](#fvs-input-values)
- [Fuels Reduction QM Procedure](#fuels-reduction-qm-procedure)
- [Reforestation QM Procedure](#reforestation-qm-procedure)
- [Pest Management QM Procedure](#pest-management-qm-procedure)
- [Forest Conservation QM Procedure](#forest-conservation-qm-procedure)
- [Biomass Utilization QM Procedure](#biomass-utilization-qm-procedure)

For details on the methodological background and equations embedded
within the of the California Climate Investments (CCI) Forest
Restoration and Management (FRM) Quantification Methodology (QM), please
see <https://www.caclimateinvestments.ca.gov/tools>.

For video tutorials see the [CCI youtube
channel](https://youtu.be/6aqkJxXq_SI?si=E64VxyQeIX4GE0ES)

Please contact <ForestQM@arb.ca.gov> with questions.

# Downloading data and software

First, download the below items according to your project type.

| Data or Software | Restoration | Pest Management | Fuel Reduction | Conservation | Biomass Utilization |
|----|:--:|:--:|:--:|:--:|:--:|
| [Forest Restoration & Management Calculator Tool](https://gcc02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fww2.arb.ca.gov%2Fsites%2Fdefault%2Ffiles%2Fauction-proceeds%2FFRM_Calculator_Tool25-26%2520Final%2520Revised%2520Apr14.xlsx&data=05%7C02%7CCarmen.Tubbesing%40arb.ca.gov%7Cf79c4c053fa942e8780408de9b04b26a%7C9de5aaee778840b1a438c0ccc98c87cc%7C0%7C0%7C639118641672841471%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=HevUDQn9fXEbmzVA1pppavKzCw412bu1ywbUiaIMfnQ%3D&reserved=0) | X | X | X | X | X |
| [Code repository from GitHub](https://github.com/arb-ca/ForestHealthQM) | X | X | X |  |  |
| [FVS software](https://www.fs.usda.gov/fvs/software/complete.php) | X | X | X | X |  |
| [TreeMap files](https://research.fs.usda.gov/firelab/products/dataandtools/treemap-tree-level-model-united-states-forests) | X | X | X | X |  |
| [California State Boundary](https://data.ca.gov/dataset/ca-geographic-boundaries) (for visualization) | X | X | X | X |  |
| [FVS Variant Map shapefile](https://www.fs.usda.gov/managing-land/forest-management/fvs/documents) | X | X | X | X |  |
| [FSim rasters for 2011 and 2047](https://www.fs.usda.gov/rds/archive/catalog/RDS-2025-0006) ([RDS-2025-0006.zip](https://usfs-public.box.com/shared/static/h55qel755s97nagdu97ebd4z6fzpp3w1.zip)) |  |  | X |  |  |
| [LANDFIRE fuel raster](https://www.landfire.gov/viewer) for California (us_250 40 Fire Behavior fuel) |  |  | X |  |  |
| NIDRM Data from the [NIDRM website](https://www.fs.usda.gov/science-technology/data-tools-products/fhp-mapping-reporting/national-insect-disease-risk-and-hazard-mapping), including 2018 update layer for California and 2012 “Composite Hazard from all pests” |  | X |  |  |  |
| [PostCRPT tool](https://reforestationtools.org/postfire-conifer-reforestation-planning-tool) results from the wildfire preceding reforestation | X |  |  |  |  |
| REGIMPUTE .kcp files for the FVS variant(s) relevant to your project. Choose the kcp file(s) with “Regen_SpeciesMethod” in the name. See the [REGIMPUTE website](https://figshare.com/articles/dataset/REGIMPUTE_Directory/26876338?file=52868588) | X | X | X |  |  |

# FVS Input values

Use these selections in FVS when instructed in the procedures below.

| Location in FVS GUI | Parameter or Keyword | Value |
|----|----|----|
| Simulate<br> ⤷ Time | Common starting year | Year in which treatment, planting, or easement will take place |
| Simulate<br> ⤷ Time | Common ending year | End of Project (as shown in Calculator Tool) **+ 1** |
| Simulate<br> ⤷ Time | Growth and reporting interval (years) | 5 |
| Simulate<br> ⤷ Components<br> ⤷ Keywords<br> ⤷ Fire and Fuels Extension<br> ⤷ CarbCalc | Biomass predictions | 1 = Use Jenkins and others |
|  | Units | 2 = Combined |
| Simulate<br> ⤷ Components<br> ⤷ Keywords<br> ⤷ Fire and Fuels Extension<br> ⤷ FireCalc | The fire behavior calculations should use: | 1 = the new fuel model selection logic |
|  | Fuel model set for use with the new fuel model logic: | 2 = use all 53 fuel models |

# Fuels Reduction QM Procedure

1.  [Prepare files](#1-prepare-files)
2.  [Open the Forest Restoration & Management Calculator Tool Excel
    file](#2-open-the-forest-restoration--management-calculator-tool-excel-file)
3.  [Configure `project_vars.R`](#3-configure-project_varsr)
4.  [Run Scripts 1 and 2](#4-run-scripts-1-and-2)
5.  [Run FVS simulations for the Treatment
    Area](#5-run-fvs-simulations-for-the-treatment-area)
6.  [Run FVS simulations for the Impact
    Area](#6-run-fvs-simulations-for-the-impact-area)
7.  [Configure `treatment_vars.R`](#7-configure-treatment_varsr)
8.  [Run Scripts 3](#8-run-script-3)
9.  [Run TRIAADS](#9-run-triaads)
10. [Run Script 4](#10-run-script-4)
11. [Fill in the remainder of the Excel Calculator
    Tool](#11-fill-in-the-remainder-of-the-excel-calculator-tool)
12. [Check results](#12-check-results)

### 1. Prepare files

- Prepare project shapefiles
  - There should be one shapefile for each treatment activity. All the
    shapefiles for one project should be in the same folder, but
    individual shapefiles may be in subfolders within that. Shapefile
    names *must be unique* – e.g. if two shapefiles for different
    treatments are both named “projectID.shp”, one will be removed and
    the QM scripts will only run the other.
- Refer to the [Data and software downloads
  table](#downloading-data-and-software) above to identify the data and
  software required.
  - Download and install or unzip each item.
- Prepare raster data, if needed
  - Using the tool of your choice (e.g., ArcGIS Pro), perform raster
    math to average the 2011 and 2047 FSim rasters
  - For faster processing times, you may also want to clip the TreeMap
    rasters to a smaller size – e.g., the size of California or your
    county or park
  - Save the LANDFIRE fuel raster for California in the folder
    `ForestHealthQM/R-code/gigafire-randig-calfire-lemma/data/base_data`<br>

### 2. Open the [Forest Restoration & Management Calculator Tool](https://gcc02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fww2.arb.ca.gov%2Fsites%2Fdefault%2Ffiles%2Fauction-proceeds%2FFRM_Calculator_Tool25-26%2520Final%2520Revised%2520Apr14.xlsx&data=05%7C02%7CCarmen.Tubbesing%40arb.ca.gov%7Cf79c4c053fa942e8780408de9b04b26a%7C9de5aaee778840b1a438c0ccc98c87cc%7C0%7C0%7C639118641672841471%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=HevUDQn9fXEbmzVA1pppavKzCw412bu1ywbUiaIMfnQ%3D&reserved=0) Excel file

- populate the green cells with project information.

### 3. Configure `project_vars.R`

- Open `project_vars.R`
- Set the project name and project ID
- Set the TreeMap vintage (“tm”) based on the year of your treatments
  - Options are 2016, 2020, and 2022. Most projects will use 2022,
    unless the QM is being run retroactively for older treatments.
    Choose the year that is closest to your treatment year without being
    after it.
- Define the project name, treatment name, and folder where treatment
  shapefiles are located (“trt_dr”)<br>

### 4. Run Scripts 1 and 2

- These end in “.qmd”
- Open each script in Rstudio and then click “Render” at the top. Wait
  for Script 1 to finish rendering before rendering Script 2.
- You only need to run each of these scripts once per project, even if
  there are multiple treatment types.
- They read in all shapefiles in a specified folder, prepare them for
  FVS, and save files used as inputs for TRIAADS and FVS
- In cases where impact areas within a project overlap, Script 1 does
  not create impact area files for every treatment. Script 1 also cuts
  out portions of impact areas that overlap with the boundaries of other
  treatment activities. <br>
- After running each script, find the .html that was generated in the
  folder `R-code`
  - Scroll through the file and look for errors. These will show up in
    large red text. If you see one, fix the problem and re-run the
    script.
  - Add the Project ID to its name
    - e.g. rename `1-Intersect_TreeMap_treatments` to
      `1-Intersect_TreeMap_treatments_25-WP-NEU-79327857`
    - This way, when you run the script again for another project, you
      won’t overwrite this render

### 5. Run FVS simulations for the Treatment Area

- [ ] Open the FVS software and start a new project
  - To do this, go to `Manage Projects` → `Manage Project.` Type a new
    project title (such as your project_ID + TCN), click “Make new
    project”, then go above to “Open selected project” and open it.
- Under `Manage Project` –\> `Import input data`, upload the treatment
  area FVS input file into FVS by clicking “Browse” under “Step 1.”
  - These files will be located in the folder `FVS_Input` and will
    follow the naming convention “FVS_input\_” + project_ID + TCN
    - Example: `FVS_input_8GG24601_3.3.xlsx`
  - Then select `Install uploaded database`
- Under `Simulate`→ `Stands`, select `All_Stands` in the “Groups”
  section and click `Add stands in selected groups` toward the bottom of
  the screen.
  - This adds all stands in the input data to your simulation.
- Configure for all runs
  - Set `Time`, `CarbCalc`, and `FireCalc` variables according to the
    [FVS input variables table](#fvs-input-values), leaving other values
    as defaults
  - Add natural regeneration
    - Under Editor (`Simulate` → `Components` → `Editor`), upload the
      REGIMPUTE regeneration file for the project’s FVS variant that has
      “species” in the name
      - Example: `Regen_SpeciesMethod_NC.kcp`
    - Click `Save in run` below `Existing component collection`
- Select outputs:
  - ✅`Carbon and fuels`
  - ✅`Fire and mortality`
  - ✅`Stand structure`
- Rename the run BSNF (for baseline no fire)
- Under `Simulate`→ `*Run*`, title the MgmtID as **BSNF**, then select
  `Run in background`
  - Choose the number of cores you want to use. Consider using a third
    to a quarter of the cores on your computer so that you can run
    multiple FVS simulations in the background at the same time.
  - Hit `Save and Run`
    - For a slow computer, consider waiting for one run to finish before
      continuing to the following steps.
- Configure and run BSWF (Baseline With Fire)
  - Duplicate the BSNF run and rename it **BSWF**, then save
  - Add the **SimFire** keyword under the Fire and Fuels Extension
    (`Simulate` → `Components` → `Keywords` →
    `Fire and Fuels Extension`)
    - Year or cycle number: 5 years after treatment
      - *Note:* If treatment spans multiple years (e.g.,
        thin/pile/burn), SimFire occurs 5 years after the **first**
        component is completed
    - Wind speed → 20 mph
    - Moisture level → 1 (very dry)
    - Temperature → 90
    - Mortality code → 1
    - Percentage of stand area burned → 100
    - Season of fire → 4 (Fall)
  - Rename MgmtID to **BSWF** and Run
- Configure and run TRWF (Treatment With Fire)
  - Duplicate the BSWF run and rename as **TRWF**
  - Under `Simulate` → `Components` → `Management` → `Fuel Treatments`,
    select the appropriate fuel treatment type
    - Choose from: Thin from Below, Mastication, Prescribed burn, Thin
      with fuel piled and burned, Pile burn surface fuel
    - Enter treatment‑specific information from the project details.
    - Enter these defaults if needed:
      - If thinning: set `Proportion of small trees left` = 0.01
      - If mastication: set `Percent masticated` to the midpoint of the
        range (e.g. 95%)
      - For mastication: leave
        `Proportion of surface fuel composed of masticated material` =
        .7
  - Under `Event Monitor` → `Compute Stand Variables in Editor`,
    - Year or cycle number = year of treatment
    - In the Editor box, paste:  
      `LiveCRem = TreeBio(0,0,1,All,0.,200.,0.,500.)*0.5*0.907185`
  - Rename MgmtID to **TRWF** and Run
- Configure TRNF (Treatment No Fire)
  - Duplicate the TRWF run and rename as **TRNF**
  - Delete SimFire
    - (Select the SimFire from the “Simulation Contents” on the far
      left. Once selected, click the “Cut/Delete” button.)
  - Rename MgmtID to **TRNF** and Run
- Download FVS outputs in the `View Outputs` tab
  - Wait until all runs have completed and the box under “Background run
    status” is empty.
  - Under `Load`, “Runs to Consider,” select all runs (TRNF, TRWF, BSWF,
    and BSNF)
  - Under `Database tables to consider`, choose `FVS_Carbon` and
    `FVS_Compute`
  - Next, go to `Explore`
    - Select all stands and all years
    - Select variables: `MgmtID`, `StandID`, `Year`,
      `Aboveground_Total_Live`, `Belowground_Live`, `LIVECREM`
    - Save as: `FVS_Output/ProjectID` + TCN + `_treatment_carbon`
      - Example:
        `FVS_Output/8GG24601/8GG24601_1.4_treatment_carbon.xlsx`
  - Return to `Load`
  - For BSNF and TRNF, select `FVS_PotFire` and `FVS_StrClass`
    - In the `Years` section, select only the year 5 years after
      treatment
    - Under `Database variables to consider`, select all variables
    - Save as: `FVS_Output/Project ID/ProjectID` + TCN +
      `_treatment_TRIAADS`
      - Example:
        `FVS_Output/8GG24601/8GG24601_1.4_treatment_TRIAADS.xlsx`
- Save your key file for future reference
  - Under `Manage Projects` → `Downloads`, select
    `Keyword file for current run`
  - This creates a record of exactly what parameters your FVS run
    included, which can be used for later reference<br>

### 6. Run FVS simulations for the Impact Area

Note: This is only necessary for TCNs that Script 1 identified as
needing impact area analysis. To find out which TCNs those are, scroll
the the bottom of the Script 1 rendered html.

- Start and open a new project in FVS. Distinguish the title to show it
  is for Impact Area (e.g. your project ID + TCN + “IA”)
- Under `Manage Projects` → `Import input data`, upload the Impact Area
  FVS input file generated from Script 2, then click
  `Install uploaded database`.
  - This files will be located in the folder `FVS_Input` and will follow
    the naming convention “FVS_input_IA” + Project_ID + TCN
- Under `Simulate` → `Stands`, select “All_Stands” in the `Groups`
  section and click “Add stands in selected groups.” This adds all
  stands in the input data to your simulation.
- Configure for all runs
  - Set Time, CarbCalc, and FireCalc variables according to the [FVS
    input variables table](#fvs-input-values), leaving other values as
    defaults
  - Add natural regeneration
    - Under Editor, upload the REGIMPUTE regeneration file for the
      project’s FVS variant. This should have “Regen_SpeciesMethod” in
      the file name.
      - Example: `Regen_SpeciesMethod_NC.kcp`
    - Save in run
- Select outputs:
  - ✅`Carbon and fuels`
  - ✅`Fire and mortality`
  - ✅`Stand structure`
- Rename `Run title` as IANF (for Impact Area No Fire)
- Under *Run*, title the MgmtID as IANF, then select “Run in background”
- Configure and run for wildfire scenario
  - Duplicate the IANF run and rename it **IAWF** and save
  - Add the **SimFire** keyword under the Fire and Fuels Extension
    - Year or cycle number: 5 years after treatment
      - *Note:* If the treatment takes multiple years (e.g.,
        thin/pile/burn), SimFire should occur five years after the first
        component is completed
    - Wind speed → 20 mph
    - Moisture level → 1 (very dry)
    - Temperature → 90
    - Mortality code → 1
    - Percentage of stand area burned → 100
    - Season of fire → 4 (Fall)
- Under *Run*, title the MgmtID as **IAWF**, then select “Run in
  background”
- Download FVS outputs in the View Outputs tab
  - Under *Load*, select both runs, then select **FVS_Carbon**,
    **FVS_PotFire**, and **FVS_StrClass** and move to Explore
    - Select all stands and all years
    - Leave all variables selected
    - Download
    - Save as: `FVS_Output/Project ID/ProjectID` + TCN +
      `_IA_TRIAADS.xlsx`
      - Example: `FVS_Output/8GG25501/8GG25501_1.4_IA_TRIAADS.xlsx`

### 7. Configure `treatment_vars.R`

- Edit `TCN` and `end_year` to match your treatment
- Set `type` to `TA`
- Ignore `area_ac`

### 8. Run Script 3

- Script 3 is run once per FVS output file. If your treatment requires
  Impact Area analysis, run Script 3 for the treatment area, then modify
  `treatment_vars.R` by setting `type` to `IA`, and re-run Script 3.
  <br>
- Each time you render Script 3, rename the resulting html file with the
  TCN and area type
  - e.g. `3-FVS-post-processing_FR_2new_TA.html`
  - This will keep a record of the Script 3 run that won’t be
    overwritten the next time you render the file.

### 9. Run TRIAADS

- Navigate to `R-code/TRIAADS/gigafire-randig-calfire-treemap` and open
  `runshiny.R`.
- Run the two lines of code in `runshiny.R`.
- The Shiny app will appear. Under `Upload Treatment Shapefile`,
  navigate to `triaads_input/` and find the zipped folder with your
  project ID and TCN.
- If the treatment will not have impact area analysis, as identified in
  Script 1, uncheck the box “Include Impact Area Analysis”.
  - If the treatment requires impact area analysis, go to
    `Upload Impact Area Shapefile` and upload the zipped folder in
    `triaads_input/` with your project ID, TCN, and “IA” in the title.
- Set the Extent Expansion to 50%
- Hit Prepare Data
- Move to the second tab.
- Hit Browse under `Upload FVS Data Files`
  - Navigate to the folder `FVS_Output` and find the file(s) for your
    TCN
  - If your analysis includes impact area, upload two files – one
    treatment and one impact area – under the same window.
    - E.g. `25-WP-NEU-79327857_1new_IA_TRIAADS` and
      `25-WP-NEU-79327857_1new_treatment_TRIAADS`
- Check that under `Select Analysis Year`, the year of simulated
  wildfire is displayed (five years after treatment)
- Click `Prepare Landscape Inputs`
- Scroll down and click `Run RANDIG for Control`
  - Do not modify the fire settings, like fuel moisture and wind speed
  - RANDIG may take a few minutes to run. Be patient and wait until
    results figures appear.
    - Note: If the shapefile for this TCN includes multiple polygons
      separated by untreated land, RANDIG will simulate fires across the
      entire area, including untreated land between the treated
      polygons. If this step of TRIAADS is taking an exceedingly long
      time, consider separating your TCN into two separate TCNs and
      running them individually.
  - When results have appeared, click `Run RANDIG for Treatment`
- In Analysis tab, select Run Analysis.
  - Download the table of results and save, naming the file with your
    Project ID and TCN.

### 10. Run Script 4

- Script 4 only needs to be run once per project.

### 11. Fill in the remainder of the Excel Calculator Tool

- Populate the yellow cells with results from the scripts and TRIAADS

### 12. Check results

- Look over each of the values in tables in columns I-J and the bar
  chart. Do the values make sense? Are any of them negative that
  shouldn’t be?<br>

# Reforestation QM Procedure

1.  [Prepare files](#1-prepare-files)
2.  [Open the Forest Restoration & Management Calculator Tool excel
    file](#2-open-the-forest-restoration--management-calculator-tool-excel-file)
3.  [Configure `project_vars.R` and
    `treatment_vars.R`](#3-configure-project_varsr-and-treatment_varsr)
4.  [Run Scripts 1, 2, 0 and 7, in that
    order](#4-run-scripts-1-2-0-and-7-in-that-order)
5.  [Open FVS software and run simulations for the reforestation and
    baseline
    scenarios](#5-open-fvs-software-and-run-simulations-for-the-reforestation-and-baseline-scenarios)
6.  [Configure and run Script 5 for each of the FVS output
    files](#6-configure-and-run-script-5-for-each-of-the-fvs-output-files)
7.  [Return to the Forest Restoration & Management Calculator Tool excel
    file](#7-return-to-the-forest-restoration--management-calculator-tool-excel-file)

### A note about site preparation and planting activities

If a project includes only site preparation or only planting, the
Reforestation portion of the QM can be completed to reflect only those
activities. The Calculator Tool includes a toggle for whether the
reforestation activity includes site preparation, planting, or both.

### 1. Prepare files

- Refer to the [Data and software downloads
  table](#downloading-data-and-software) to identify the data and
  scripts required.
- Download and install or unzip each item.

### 2. Open the Forest Restoration & Management **Calculator Tool** excel file.

- Populate the green cells with project information.

### 3. Configure `project_vars.R` and `treatment_vars.R`

- Replace the file paths and directory locations
- Set the TreeMap vintage in Script 1 based on the year of your
  treatments
- Define the project name, treatment name, and shapefile name<br>

### 4. Run Scripts 1, 2, 0 and 7, in that order

- Scripts 1, 2, and 0 only need to be run once per project.
- Script 7 must be run once per reforestation treatment.

### 5. Run FVS simulations for the reforestation and baseline scenarios

1.  Open FVS and start a new project
2.  Select the appropriate variant, which can be found at the end of the
    Script 7 results
3.  Add stands
    - This simulation will have only one representative stand. Under
      Stands, select and add Bareground.
    - If the planting will occur beneath residual overstory trees,
      upload a tree list. See FVS documentation or contact
      [*ForestQM@arb.ca.gov*](mailto:ForestQM@arb.ca.gov) for
      assistance.
    - Set Time and CarbCalc variables according to the [FVS input
      variables table](fvs-input-values), leaving other values as
      defaults
4.  Under Select Outputs, select Carbon and fuels
5.  Add planted seedlings for the **Project Scenario**
    - Under Management, select Planting & Natural Regeneration and set
      the following parameters:
      - Year or cycle number = year of site preparation (year of
        planting will be determined in the next input)
        - If there is no site preparation, enter the year of planting
      - Years following disturbance = the number of years between site
        preparation and planting. If site preparation and planting occur
        in the same year or if there is no site preparation, enter 0.
      - Sprouting = Off
      - Type of regeneration = Plant
      - Enter the trees/acre by species that will be planted
      - For Percent survival, enter 70 (or less, if you expect higher
        mortality)
      - Uniform spatial distribution
      - If more than two species will be planted, repeat the above steps
        or select Change to freeform
6.  Add ingrowth regeneration
    - Under Editor, upload the regeneration file for the project’s FVS
      variant
    - E.g., `Regen_SpeciesMethod_WS.kcp`
    - Save in run
7.  Rename “PL,” assign MgmtID to “PL,” and Run
8.  Configure for the **Baseline Scenario**
    - Duplicate and rename the run to “BS”
    - Remove planted seedlings
9.  Add naturally regenerating seedlings
    - Under Management, select Planting & Natural Regeneration and set
      the following parameters:
      - Year or cycle number = year of the wildfire
      - Sprouting = Off
      - Years following disturbance = 20
      - Type of regeneration = Natural
      - Enter the TPA by species that are predicted by Script 7
      - Percent survival = 80
      - Height = 6 (ft)
      - If more than two species are predicted, select Change to
        freeform
      - Uniform spatial distribution
10. Add the `CycleAt` keyword under Base FVS System
    - Set Year to 20 years after the year of the wildfire
11. Add growth modifiers
    - Select Diameter Growth Modifiers under Modifiers tab
    - Select Adjust small tree diameter growth
    - Change the Multiplier value to 0.7 and Year or cycle number to the
      year of the fire. Leave All species selected.
    - Repeat for the Height Growth Modifier called Adjust small tree
      height model. Again use a multiplier value of 0.7.
12. Change the MgmtID to “BS” and Run
13. Download FVS outputs in the View Outputs tab
    - Under Load, select all runs and then select FVS_Carbon and move to
      Explore
      - Select all years and all variables
      - Download and save in the folder `FVS_Output/<Project_ID>`
      - Name the file `Project_ID + TCN + "RF".xlsx`

### 6. Configure and run Script 5 for each of the FVS output files

- Replace the file paths and directory locations at the start of the
  script to match the FVS output
- Adjust `area_ac` to match your planned reforestation acreage
- Set `end_yr` to match End of Project in the Calculator Tool.
- Run Script 5

### 7. Return to the Forest Restoration & Management **Calculator Tool** excel file.

- Populate the yellow cells with results from Script 5

# Pest Management QM Procedure

1.  [Prepare files](#1-prepare-files-1)
2.  [Open the Forest Restoration & Management Calculator Tool Excel
    file](#2-open-the-forest-restoration--management-calculator-tool-excel-file-1)
3.  [Run script `Prep_NIDRM.qmd`](#3-run-script-prep_nidrmqmd)
4.  [Run Scripts 1 and 2](#4-run-scripts-1-and-2-1)
5.  [Run FVS simulations for the Treatment
    Area](#5-run-fvs-simulations-for-the-treatment-area-1)
6.  [Run FVS simulations for the Impact
    Area](#6-run-fvs-simulations-for-the-impact-area-1)
7.  [Run Script 3](#7-run-script-3)
8.  [Run Script 6](#8-run-script-6)
9.  [Complete the Forest Restoration & Management Calculator Tool excel
    file](#9-complete-the-forest-restoration--management-calculator-tool-excel-file)

### 1. Prepare files

- Refer to the [Data and software downloads
  table](#downloading-data-and-software) to identify the data and
  scripts required.
- Download and install or unzip each item.

### 2. Open the Forest Restoration & Management **Calculator Tool** excel file.

- Populate the green cells with project information.

### 3. Run script `Prep_NIDRM.qmd`

- Replace the file paths with the locations of NIDRM files that you
  downloaded in Step 1
- Run

### 4. Run Scripts 1 and 2

- You only need to run each of these scripts once per project, even if
  there are multiple treatment types.
- They read in all shapefiles in a specified folder, prepare them for
  FVS, and save files used as inputs for TRIAADS and FVS  
- In cases where impact areas within a project overlap, Script 1 does
  not create impact area files for every treatment (See Section II,
  Overlapping Treatment Areas)<br>
- After running each script, find the .html that was generated and add
  the Project ID to its name
  - e.g. rename `1-Intersect_TreeMap_treatments` to
    `1-Intersect_TreeMap_treatments_25-WP-NEU-79327857`
  - This way, when you run the script again for another project, you
    won’t overwrite this render

### 5. Run FVS simulations for the Treatment Area

1.  Open FVS and start a new project
2.  Upload the treatment area FVS input file generated from Script 2
    into FVS
    - These files will be located in the folder “FVS_Input” and will
      follow the naming convention `FVS_input_` + project_ID + TCN
      - Example: `FVS_input_8GG24601_3.3.xlsx`
3.  Add all stands
4.  Configure for all runs
    - Set `Time` and `CarbCalc` variables according to *Table 33*,
      leaving other values as defaults
    - Add natural regeneration
      - Under Editor, upload the regeneration file for the project’s FVS
        variant
        - E.g. `Regen_SpeciesMethod_NC.kcp`
      - Save in run
5.  Under Select outputs, choose Carbon and fuels
6.  Rename as BSPM (Baseline Pest Management) and title the MgmtID as
    **BSPM**, then Run
7.  Configure and run for TRPM (Treatment Pest Management)
    - Duplicate the BSPM run and rename as **TRPM**
    - Under Components, select Management, Fuel Treatments, and then the
      appropriate fuel treatment type - Generally, the best treatment
      type will be Thin a species across a DBH range, potentially
      followed by piling and burning - Enter information specific to
      your treatment
      - Under Event Monitor, select Compute Stand Variables in Editor,
        then paste in the following:
        `LiveCRem = TreeBio(0,0,1,All,0.,200.,0.,500.)*0.5*0.907185`
    - Rename MgmtID to **TRPM** and **Run**
8.  Download FVS outputs in the View Outputs tab
    - Under Load, select both runs and then select FVS_Carbon and
      FVS_Compute and move to Explore
      - Select all stands and all years
      - Select MgmtID, StandID, Year, Aboveground_Total_Live,
        Belowground_Live, and LIVECREM
      - Download
9.  Run FVS simulations in the Impact Area
    - See section *C. Pest Management FVS Procedure – Impact Area* below
      for step-by-step instructions
10. Script `3-FVS-post-processing-FR.qmd`
    - Script 3 is run once per treatment, so if your project contains
      multiple treatments you may need to run it several times. Make
      sure that each time you run Script 3, it points to the correct FVS
      output file and areas file (areas file is created in Script 1)
    - Set `end_year` based on End of Project as shown in the Calculator
      Tool
11. Script `6-Query-NIDRM.qmd`
    - Customize the `NIDRM_updated.tif` file path (created in Step 3)
      and the file path for the pest management activity shapefile
    - **Run**
12. Return to the Forest Restoration & Management **Calculator Tool**
    excel file and populate the yellow cells with results from Script 6
    and FVS.

### 6. Run FVS simulations for the Impact Area

1.  Start a new project
2.  Upload the impact area FVS input file generated from Script 2 into
    FVS
    - These files will be located in the folder “FVS_Input” and will
      follow the naming convention `FVS_input_IA_` + project_ID + TCN
      - Example: `FVS_input_8GG24601_3.3.xlsx`
3.  Add all stands
4.  Configure according to Step 4 in Section B above
5.  Under Select outputs, choose Carbon and fuels
6.  Rename as **IAPM** and title the MgmtID as **IAPM**, then Run
7.  Download FVS outputs in the View Outputs tab
    - Under Load, select FVS_Carbon and move to Explore
      - Select all stands and all years
      - Select MgmtID, StandID, Year, Aboveground_Total_Live, and
        Belowground_Live
      - Download

### 7. Run Script 3

- Script 3 is run once per FVS output file. If your treatment requires
  Impact Area analysis, run Script 3 for the treatment area, then modify
  `treatment_vars.R` by setting `type` to `IA`, and re-run Script 3.
  <br>

### 8. Run Script 6

- Customize the NIDRM_updated.tif file path (created in Step 3) and the
  file path for the pest management activity shapefile

### 9. Complete the Forest Restoration & Management Calculator Tool excel file

- Populate the yellow cells with results from Script 6 and FVS.

# Forest Conservation QM Procedure

1.  [Prepare files](#1-prepare-files-2)
2.  [Open the Forest Restoration & Management Calculator Tool Excel
    file](#2-open-the-forest-restoration--management-calculator-tool-excel-file-2)
3.  [Configure Scripts 1, 2, and 3](#3-configure-scripts-1-2-and-3)
4.  [Run Scripts 1 and 2](#4-run-scripts-1-and-2-2)
5.  [Run FVS simulations for the project (easement)
    scenario](#5-run-fvs-simulations-for-the-project-easement-scenario)
6.  [Run FVS simulations for the baseline scenario (Forest Management
    Easements
    only)](#6-run-fvs-simulations-for-the-baseline-scenario-forest-management-easements-only)
7.  [Configure and run Script 3](#7-configure-and-run-script-3)
8.  [Fill in the remainder of the Excel Calculator
    Tool](#8-fill-in-the-remainder-of-the-excel-calculator-tool-2)

There are two types of Forest Conservation projects: Avoided Conversion
Easements and Forest Management Easements. For Avoided Conversion, FVS
is run in the easement scenario only. For Forest Management Easements,
FVS is run in both the easement scenario and the baseline
(counterfactual) scenario.

### 1. Prepare files

- Refer to the [Downloading data and software
  table](#downloading-data-and-software) to identify the data and
  scripts required.
- Download and install or unzip each item.

### 2. Open the Forest Restoration & Management Calculator Tool Excel file

- Populate the green cells with project information.

### 3. Configure Scripts 1, 2, and 3

- Customize the file paths and End of Project year in Scripts 1, 2, and
  3 from the [ForestHealthQM GitHub
  repository](https://github.com/arb-ca/ForestHealthQM/R-code)

### 4. Run Scripts 1 and 2

### 5. Run FVS simulations for the project (easement) scenario

- Open FVS software and run simulations for the project (easement)
  scenario following the steps below.

1.  Start a new project in FVS
2.  Upload the FVS input file generated from Script 2 into FVS
    - These files will be located in the folder “FVS_Input” and will
      follow the naming convention `FVS_input_` + project_ID + TCN
      - Example: `FVS_input_8GG24601_3.3.xlsx`
3.  Under Simulate, select “All_Stands” and click “Add stands in
    selected groups.” This adds all stands in the input data to your
    simulation.
4.  Configure for all runs
    - Set Time and CarbCalc variables according to the [FVS input
      variables table](#fvs-input-values), leaving other values as
      defaults
    - Add natural regeneration
      - Under Editor, upload the regeneration file for the project’s FVS
        variant
        - E.g., `Regen_SpeciesMethod_NC.kcp`
      - Save in run
5.  Under Select outputs, choose Carbon and fuels
6.  Configure for the project (easement) scenario
    - The FVS keywords used in this step depend on the specifics of the
      parcel and should reflect the most likely forest management
      practices over the entire project duration if an easement is
      placed on the parcel. Refer to FVS documentation to find the
      keywords that best represent your project. If the project is an
      Avoided Conversion Easement and no forest management is planned,
      no additional keywords are needed.
7.  Rename as PRFC (Project Forest Conservation), set MgmtID to
    **PRFC**, then Run
8.  Download FVS outputs in the View Outputs tab
    - Under Load, select both runs and then select FVS_Carbon and move
      to Explore
      - Select all stands and all years
      - Select MgmtID, StandID, Year, Aboveground_Total_Live,
        Belowground_Live, and Total_Removed_Carbon
      - Download

### 6. Run FVS simulations for the baseline scenario (Forest Management Easements only)

- If the project is a Forest Management Easement, configure and run FVS
  for NPFC (No Project Forest Conservation)
  - Duplicate the PRFC run and rename as **NPFC**
  - Add management keywords that reflect the most likely forest
    management activities throughout the 50–80-year project duration if
    no easement is placed on the parcel. Refer to FVS documentation to
    find the keywords that best represent your location.
  - Rename MgmtID to **NPFC** and Run

### 7. Configure and run Script 3

- Configure Script 3 for each of the FVS output files
  - Replace the file paths and directory locations at the start of the
    script to match the FVS output
  - Adjust `area_ac` to match your planned conservation easement acreage
  - Set `end_yr` to match End of Project in the Calculator Tool
- Run Script 3

### 8. Fill in the remainder of the Excel Calculator Tool

- Return to the Forest Restoration & Management **Calculator Tool**
  excel file and populate the yellow cells with results from Script 3

------------------------------------------------------------------------

# Biomass Utilization QM Procedure

1.  [Open the Forest Restoration & Management Calculator Tool Excel
    file](#1-open-the-forest-restoration--management-calculator-tool-excel-file-3)
2.  [Fill in mill efficiency](#2-fill-in-mill-efficiency)
3.  [Fill in biomass inputs](#3-fill-in-biomass-inputs)

The procedure for running the Biomass Utilization portion of the QM is
the simplest of all activity types. No tools or scripts are required
other than the Calculator Tool. The biomass utilized from reforestation,
pest management, or fuels reduction activities is entered in the
Calculator Tool, along with the types of wood products that will be
created with the biomass. The benefits from Forest Conservation biomass
utilization are calculated automatically based on inputs in the easement
tab(s).

### 1. Open the Forest Restoration & Management Calculator Tool Excel file

- Populate the green cells with project information.

### 2. Fill in mill efficiency

- Fill in the appropriate mill efficiency based on site-specific
  information, if available, or using the default values below:

| Wood type | Default mill efficiency |
|-----------|-------------------------|
| Softwood  | 67.5%                   |
| Hardwood  | 56.8%                   |

### 3. Fill in biomass inputs

Complete the following cells in the Biomass Utilization section of the
Calculator Tool:

- **Cell E16** — “Biomass to be removed from the project area as part of
  implementing reforestation, pest management, or fuels reduction
  activities and delivered to a mill, with and without the current
  project (BDT)”
  - Note: This includes biomass that will be removed both with or
    without funding for the current project. This does not include
    biomass removal as part of Conservation Easements.
- **Cells E40–42** — Avoided disposal inputs (only applicable if the
  project funds utilization of biomass removed as part of management
  practices not associated with the project):
  - E40: “Biomass that would be removed and open pile burned without
    project (BDT)”
  - E41: “Biomass that would be removed and landfilled without project
    (BDT)”
  - E42: “Biomass that would be removed and left to decay on-site
    without project (BDT)”
  - Note: Avoided disposal emissions are only included for projects that
    involve the utilization of biomass that would otherwise be removed
    from the forest, but the removal was not part of the project being
    modeled — in other words, the counterfactual must be forest
    treatments that result in woody biomass waste accumulating in the
    forest or at roadside landing sites. The values in cells E40–42
    should not include material that would only require disposal because
    of the project.
