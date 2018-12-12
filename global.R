library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(rgdal)

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

delta_regions <- readOGR(dsn="data/Hydro_Delta_FromBen/hydro_CH2MHill_DP2013_merged_dslv_Delta_clp.shp")%>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

chinook_regions <- readOGR(dsn="data/Regions1to7/") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

chinook_regions$Id <- factor(c(7, 4, 3, 5, 6, 2, 1),
                             levels = c(7, 4, 3, 5, 6, 2, 1),
                             labels = c("Region 7", 
                                        "Region 4", 
                                        "Region 3", 
                                        "Region 5", 
                                        "Region 6", 
                                        "Region 2", 
                                        "Region 1"))


chinook_routing_points <- tribble(
  ~"lat", ~"lng", ~"location", ~"location_id",
  38.299283, -121.568740, "Sutter in Sacramento", "Sut.in.Sac",
  38.297067, -121.581534, "Sutter in Sutter", "Sut.in.Sut",
  38.244747, -121.503007, "Delta Cross Channel", "Junc.in.DCC",
  38.129588, -121.576870, "Georgiana Slough", "Junc.in.GeoSlo",
  37.988258, -121.463450, "Turner Cut Junction at Turner Cut", "at.TC.TC",
  37.994007, -121.431010, "Turner Cut Junction at San Joaquin River", "at.TC.SJR",
  37.817428, -121.341816, "Old River", "at.HOR.OR",
  37.816058, -121.316736, "San Joaquin", "at.HOR.SJR",
  37.821924, -121.599001, "SWP Salvage", "SWP.salv",
  37.827697, -121.566748, "CVP Salvage", "CVP.salv",
  37.880766, -121.575137, "Old River North", "to.ORN"
)

chinook_locs_id_lookup <- chinook_routing_points$location
names(chinook_locs_id_lookup) <- chinook_routing_points$location_id

# need to unsource this when I have the chinook-routing visualization
source("chinook-routing.R")

source("modules/consequence.R")
source("modules/home.R")
source("modules/chinook.R")

water_avail_model_results <- read_rds("data/delta-model-results.rds")

sod_exports <- water_avail_model_results %>%
  filter(node_name %in% c("SWP SOD", "CVP SOD"))

nod_exports <- water_avail_model_results %>% 
  filter(node_name %in% c("SWP NOD", "CVP NOD"))
