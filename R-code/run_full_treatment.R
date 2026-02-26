library(quarto)
quarto::quarto_render("1-Intersect_TreeMap_treatments.qmd")
quarto::quarto_render("2-Prepare-FVS-inputs-TM.qmd")
quarto::quarto_render("3-FVS-post-processing_FR.qmd")
quarto::quarto_render("4-wildfire-probability-FSIM.qmd")

