README
================
2026-04-10

- [Fuels Reduction Procedure](#fuels-reduction-procedure)
  - [Overview (linkable)](#overview-linkable)
  - [Full details](#full-details)

For full documentation of the California Climate Investments (CCI)
Forest Restoration and Management (FRM) Quantification Methodology (QM),
please see <https://www.caclimateinvestments.ca.gov/tools>.

Please contact <ForestQM@arb.ca.gov> with questions

# Fuels Reduction Procedure

## Overview (linkable)

- [1. Prepare files](#1-prepare-files)
- [2. Open the Forest Restoration & Management Calculator Tool Excel
  file](#2-open-the-forest-restoration--management-calculator-tool-excel-file)
- [3. Configure R scripts](#3-configure-r-scripts)
- [4. Run Scripts 1 and 2](#4-run-scripts-1-and-2)
- [5. Run FVS simulations for the Treatment
  Area](#5-run-fvs-simulations-for-the-treatment-area)
- [6. Run FVS simulations for the Impact
  Area](#6-run-fvs-simulations-for-the-impact-area)
- [7. Run Scripts 3–4](#7-run-scripts-34)
- [8. Run TRIAADS](#8-run-triaads)
- [9. Fill in the remainder of the Excel Calculator
  Tool](#9-fill-in-the-remainder-of-the-excel-calculator-tool)

## Full details

1.  **Prepare files**

    - Prepare project shapefiles
      - There should be one shapefile for each treatment activity.
        Assign each activity a Treatment Component Number (TCN). Each
        shapefile must have a field in its attribute table called “TCN”
        that contains the TCN of the activity.
    - Refer to Table 32 to identify the data and scripts required.
      - Download and install or unzip each item.
    - Prepare raster data
      - Using the tool of your choice (e.g., ArcGIS Pro), perform raster
        math to average the 2011 and 2047 FSim rasters
      - For faster processing times, you may also want to clip the
        TreeMap rasters to a smaller size – e.g., the size of California
        or your county or park
      - Save the LANDFIRE fuel raster for California in the folder
        `ForestHealthQM/R-code/gigafire-randig-calfire-lemma/data/base_data`<br><br>

2.  **Open the Forest Restoration & Management Calculator Tool Excel
    file** and populate the green cells with project information.

3.  **Configure R scripts**

    - Open `project_vars.R`  
    - Replace the file paths and directory locations at the start of
      each script to match your computer
    - Set the TreeMap vintage in Script 1 based on the year of your
      treatments
    - Define the project name, treatment name, and shapefile
      name<br><br>

4.  **Run Scripts 1 and 2**

    - You only need to run each of these scripts once per project, even
      if there are multiple treatment types.
    - They read in all shapefiles in a specified folder, prepare them
      for FVS, and save files used as inputs for TRIAADS and FVS  
    - In cases where impact areas within a project overlap, Script 1
      does not create impact area files for every treatment (See Section
      II, Overlapping Treatment Areas)<br><br>

5.  **Run FVS simulations for the Treatment Area**

    - Start a new project in FVS
    - Upload a treatment area FVS input files generated from Script 2
      into FVS
      - These files will be located in the folder “FVS_Input” and will
        follow the naming convention “FVS_input\_” + project_ID + TCN
        - Example: `FVS_input_8GG24601_3.3.xlsx`
    - **Under Simulate**, select “All_Stands” and click “Add stands in
      selected groups.”  
      This adds all stands in the input data to your simulation.
    - **Configure for all runs**
      - Set Time, CarbCalc, and FireCalc variables according to Table
        33, leaving other values as defaults
      - Add natural regeneration
        - Under Editor, upload the regeneration file for the project’s
          FVS variant
          - Example: `Regen_SpeciesMethod_NC.kcp`
        - Save in run
    - Select outputs
      - Carbon and fuels
      - Fire and mortality
      - Stand structure
    - Rename as BSNF (Baseline No Fire), set MgmtID to **BSNF**, then
      Run
    - Configure and run BSWF (Baseline With Fire)
      - Duplicate the BSNF run and rename it **BSWF**, then save
      - Add the **SimFire** keyword under the Fire and Fuels Extension
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
    - **Configure and run TRWF (Treatment With Fire)**
      - Duplicate the BSWF run and rename as **TRWF**
      - Under Components → Management → Fuel Treatments, select the
        appropriate fuel treatment type
        - Choose from: Thin from Below, Mastication, Prescribed burn,
          Thin with fuel piled and burned, Pile burn surface fuel
        - Enter treatment‑specific information
          - If thinning: set *Proportion of small trees left* = 0.01
          - If mastication: set *Percent masticated* to the midpoint of
            the range (e.g. 95%)
          - For mastication: leave *Proportion of surface fuel composed
            of masticated material (%)* = 70%
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
        - Select variables: MgmtID, StandID, Year,
          Aboveground_Total_Live, Belowground_Live, LIVECREM
        - Save as: ProjectID + TCN + `_treatment_carbon`
          - Example: `8GG24601_1.4_treatment_carbon.xlsx`
      - Return to Load  
      - For BSNF and TRNF, select **FVS_PotFire** and **FVS_StrClass**
        - Select only the year 5 years after treatment
        - Select all variables
        - Save as: ProjectID + TCN + `_treatment_TRIAADS`
          - Example: \`8GG24601_1.4_treatment_TRI<br><br>

6.  **Run FVS simulations for the Impact Area**

    - This is only necessary for TCNs that Script 1 identified as
      needing impact area analysis
    - See section **C. Fuels Reduction FVS Procedure–Impact
      Area**<br><br>

7.  **Run Scripts 3–4**

    - Script 3 is run once per treatment. If the project has multiple
      treatments, run it multiple times.
    - Make sure Script 3 points to the correct FVS output file and areas
      file (created in Script 1).
    - In Script 3, set `end_year` based on project duration (see
      Calculator Tool)<br><br>

8.  **Run TRIAADS**

    - See section **D. Fuels Reduction TRIAADS Procedure**<br><br>

9.  **Fill in the remainder of the Excel Calculator Tool**

    - Populate the yellow cells with results from the scripts and Impact
      Area Tool<br><br>
