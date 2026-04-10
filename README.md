README
================
2026-04-10

- [Fuels Reduction QM Procedure](#fuels-reduction-qm-procedure)
- [Restoration QM Procedure](#restoration-qm-procedure)
- [Pest Management QM Procedure](#pest-management-qm-procedure)
- [Forest Conservation QM Procedure](#forest-conservation-qm-procedure)

For full documentation of the California Climate Investments (CCI)
Forest Restoration and Management (FRM) Quantification Methodology (QM),
please see <https://www.caclimateinvestments.ca.gov/tools>.

Please contact <ForestQM@arb.ca.gov> with questions

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
10. [Fill in the remainder of the Excel Calculator
    Tool](#10-fill-in-the-remainder-of-the-excel-calculator-tool)

### 1. Prepare files

- Prepare project shapefiles
  - There should be one shapefile for each treatment activity. Assign
    each activity a Treatment Component Number (TCN). Each shapefile
    must have a field in its attribute table called “TCN” that contains
    the TCN of the activity.
- Refer to Table 32 to identify the data and scripts required.
  - Download and install or unzip each item.
- Prepare raster data
  - Using the tool of your choice (e.g., ArcGIS Pro), perform raster
    math to average the 2011 and 2047 FSim rasters
  - For faster processing times, you may also want to clip the TreeMap
    rasters to a smaller size – e.g., the size of California or your
    county or park
  - Save the LANDFIRE fuel raster for California in the folder
    `ForestHealthQM/R-code/gigafire-randig-calfire-lemma/data/base_data`<br>

### 2. Open the Forest Restoration & Management Calculator Tool Excel file

- populate the green cells with project information.

### 3. Configure `project_vars.R`

- Open `project_vars.R`  
- Replace the file paths and directory locations
- Set the TreeMap vintage in Script 1 based on the year of your
  treatments
- Define the project name, treatment name, and shapefile name<br>

### 4. Run Scripts 1 and 2

- You only need to run each of these scripts once per project, even if
  there are multiple treatment types.
- They read in all shapefiles in a specified folder, prepare them for
  FVS, and save files used as inputs for TRIAADS and FVS  
- In cases where impact areas within a project overlap, Script 1 does
  not create impact area files for every treatment (See Section II,
  Overlapping Treatment Areas)<br>

### 5. Run FVS simulations for the Treatment Area

- Start a new project in FVS
- Upload a treatment area FVS input files generated from Script 2 into
  FVS
  - These files will be located in the folder “FVS_Input” and will
    follow the naming convention “FVS_input\_” + project_ID + TCN
    - Example: `FVS_input_8GG24601_3.3.xlsx`
- **Under Simulate**, select “All_Stands” and click “Add stands in
  selected groups.”  
  This adds all stands in the input data to your simulation.
- **Configure for all runs**
  - Set Time, CarbCalc, and FireCalc variables according to Table 33,
    leaving other values as defaults
  - Add natural regeneration
    - Under Editor, upload the regeneration file for the project’s FVS
      variant
      - Example: `Regen_SpeciesMethod_NC.kcp`
    - Save in run
- Select outputs
  - Carbon and fuels
  - Fire and mortality
  - Stand structure
- Rename as BSNF (Baseline No Fire), set MgmtID to **BSNF**, then Run
- Configure and run BSWF (Baseline With Fire)
  - Duplicate the BSNF run and rename it **BSWF**, then save
  - Add the **SimFire** keyword under the Fire and Fuels Extension
    - Year or cycle number: 5 years after treatment
    - *Note:* If treatment spans multiple years (e.g., thin/pile/burn),
      SimFire occurs 5 years after the **first** component is completed
    - Wind speed → 20 mph
    - Moisture level → 1 (very dry)
    - Temperature → 90
    - Mortality code → 1
    - Percentage of stand area burned → 100
    - Season of fire → 4 (Fall)
  - Rename MgmtID to **BSWF** and Run
- **Configure and run TRWF (Treatment With Fire)**
  - Duplicate the BSWF run and rename as **TRWF**
  - Under Components → Management → Fuel Treatments, select the
    appropriate fuel treatment type
    - Choose from: Thin from Below, Mastication, Prescribed burn, Thin
      with fuel piled and burned, Pile burn surface fuel
    - Enter treatment‑specific information
      - If thinning: set *Proportion of small trees left* = 0.01
      - If mastication: set *Percent masticated* to the midpoint of the
        range (e.g. 95%)
      - For mastication: leave *Proportion of surface fuel composed of
        masticated material (%)* = 70%
  - Under Event Monitor → Compute Stand Variables, paste:  
    `LiveCRem = TreeBio(0,0,1,All,0.,200.,0.,500.)*0.5*0.907185`
  - Rename MgmtID to **TRWF** and Run
- **Configure TRNF (Treatment No Fire)**
  - Duplicate the TRWF run and rename as **TRNF**
  - Delete SimFire
  - Rename MgmtID to **TRNF** and Run
- **Download FVS outputs in the View Outputs tab**
  - Under *Load*, select all runs → choose **FVS_Carbon** and
    **FVS_Compute** → *Explore*
    - Select all stands and all years
    - Select variables: MgmtID, StandID, Year, Aboveground_Total_Live,
      Belowground_Live, LIVECREM
    - Save as: ProjectID + TCN + `_treatment_carbon`
      - Example: `8GG24601_1.4_treatment_carbon.xlsx`
  - Return to Load  
  - For BSNF and TRNF, select **FVS_PotFire** and **FVS_StrClass**
    - Select only the year 5 years after treatment
    - Select all variables
    - Save as: Save as: FVS_Output/<Project ID>/ProjectID + TCN +
      `_treatment_TRIAADS`
      - Example:
        `FVS_Output/8GG24601/8GG24601_1.4_treatment_TRIAADS.xlsx`<br>

### 6. Run FVS simulations for the Impact Area

Note: This is only necessary for TCNs that Script 1 identified as
needing impact area analysis

- Start a new project in FVS
- Under Manage Projects \> Import input data, upload the Impact Area FVS
  input files generated from Script 2 into FVS, then click “Install
  uploaded database”
  - These files will be located in the folder “FVS_Input” and will
    follow the naming convention “FVS_input_IA” + Project_ID + TCN
- Under **Simulate**, select “All_Stands” and click “Add stands in
  selected groups.” This adds all stands in the input data to your
  simulation.
- Configure for all runs
  - Set Time, CarbCalc, and FireCalc variables according to Table 33,
    leaving other values as defaults
  - Add natural regeneration
    - Under Editor, upload the REGIMPUTE regeneration file for the
      project’s FVS variant. This should have “Regen_SpeciesMethod” in
      the file name.
      - Example: `Regen_SpeciesMethod_NC.kcp`
    - Save in run
- Select outputs
  - Carbon and fuels
  - Fire and mortality
  - Stand structure
- Rename as IANF (for Impact Area No Fire)
- Under *Run*, title the MgmtID as **IANF**, then select “Run in
  background”
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
    - Save as: FVS_Output/<Project ID>/\<ProjectID + TCN +
      `_IA_TRIAADS`\>
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

### 9. Run TRIAADS

- Navigate to `R-code/TRIAADS/gigafire-randig-calfire-treemap` and open
  `runshiny.R`.
- Run the two lines of code in `runshiny.R`.
- The Shiny app will appear. Under `Upload Treatment Shapefile`,
  navigate to `triaads_input/` and find the zipped folder with your
  project ID and TCN.
- If the treatment will not have impact area analysis, as identified in
  Script 1, uncheck the box “Include Impact Area Analysis”.
- If the treatment requires impact area analysis, upload the zipped
  folder in `triaads_input/` with your project ID, TCN, and “IA” in the
  title.
- Set the Extent Expansion to 50%
- Hit Prepare Data
- Move to the second tab.
- Hit Browse under `Upload FVS Data Files`
  - If your analysis includes impact area, under
    `Upload FVS Data Files`, upload two files – one treatment and one
    impact area – under the same window.
  - E.g. `25-WP-NEU-79327857_1new_IA_TRIAADS` and
    `25-WP-NEU-79327857_1new_treatment_TRIAADS`
- Check that under `Select Analysis Year`, the year of simulated
  wildfire is displayed (five years after treatment)
- Scroll down and click `Run RANDIG for Control` and
  `Run RANDIG for Treatment`
  - Do not modify the fire settings, like fuel moisture and wind speed
  - RANDIG may take a few minutes to run. Be patient and wait until
    results figures appear. - If your TCN includes multiple polygons
    separated by untreated land, RANDIG will simulate fires across the
    entire area, including untreated land between the treated polygons.
    If this step of TRIAADS is taking a long time, consider separating
    your TCN into two separate TCNs and running them individually.
- In Analysis tab, select Run Analysis.
  - Download the table of results.

### 10. Run Script 4

### 11. Fill in the remainder of the Excel Calculator Tool

- Populate the yellow cells with results from the scripts and
  TRIAADS<br>

# Restoration QM Procedure

# Pest Management QM Procedure

# Forest Conservation QM Procedure
