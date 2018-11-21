library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)

scenario_choices <- c(
  "May-Aug outflow X2 actions:  X2 at 74, 81 km through May-Aug,  keep at 74 May-June then 81 Jul-Aug" = "1",
  "Increase entrainment (routing) into north delta, Yolo Bypass (big notch) to allow flows between 3000 and 12000 cfs for passage and FP inundation" = "2",
  "Trap transport (barge salmonids) south delta from above Vernalis" = "3",
  "Yolo Bypass increase food production by augmenting flow in the Yolo Bypass July and September"="4",
  "Increase Turbidity - Sediment supplementation in Sacramento River"="5", 
  "Constrain Old and Middle River (OMR flows): Modify exports flows from Jan 1 through June 15 to meet range of OMR requirements.  OMR flows no more negative than: (scenario 1) -1250cfs and (scenario 2) -5000 cfs"="6",
  "Remove I:E requirement, pulse flow 150TAF 3rd week April, increase flow April-May 150TAF"="7", 
  "Preferential pumping of CVP"="8", 
  "Fish Friendly diversions in central and south Delta"="9", 
  "Franks tract restoration"="10", 
  "Conservation hatchery for Delta Smelt - add adults Nov"="11", 
  "Conservation hatchery for Delta Smelt - add fertilized eggs"="12"
  )

consequence_choices <- c(
  "Water Availability", 
  "Water Quality", 
  "Delta Smelt", 
  "Chinook Salmon"
)

# need to unsource this when I have the chinook-routing visualization
# source("chinook-routing.R")

source("modules/consequence.R")


water_avail_model_results <- read_rds("data/delta-model-results.rds")

sod_exports <- water_avail_model_results %>%
  filter(node_name %in% c("SWP SOD", "CVP SOD"))

nod_exports <- water_avail_model_results %>% 
  filter(node_name %in% c("SWP NOD", "CVP NOD"))

