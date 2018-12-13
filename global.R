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
  38.309739, -121.576109, "Sutter in Sacramento", "Sut.in.Sac",
  38.304908, -121.599392, "Sutter in Sutter", "Sut.in.Sut",
  38.244720, -121.503385, "Junction in Delta Cross Channel", "Junc.in.DCC",
  38.232174, -121.521133, "Junction in Georgiana Slough", "Junc.in.GeoSlo",
  38.254235, -121.513459, "Junction in Sacramento River", "Junc.in.Sac",
  37.990010, -121.458557, "Turner Cut at Turner Cut Junction", "at.TC.TC",
  38.003133, -121.450741, "San Joaquin River at Turner Cut Junction", "at.TC.SJR",
  37.807879, -121.331900, "Old River at HOD", "at.HOR.OR",
  37.811096, -121.320068, "San Joaquin at HOD", "at.HOR.SJR",
  37.821924, -121.599001, "SWP Salvage", "SWP.salv",
  37.827697, -121.566748, "CVP Salvage", "CVP.salv",
  37.880766, -121.575137, "Old River North", "to.ORN", 
  38.055121, -121.927622, "Vernalis to Chipps", "Vern.to.Chps", 
  38.055043, -121.898327, "Freeport to Chipps", "Free.to.Chps"
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
