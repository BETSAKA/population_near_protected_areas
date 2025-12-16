# Load libraries ----------------------------------------------------------

library(tidyverse)
library(gt)
library(readxl)
library(writexl)
library(gtExtras)
library(ggplot2)
library(ggrepel)
library(scales)
library(cowplot)
library(stddiff)
# Load and prepare data ---------------------------------------------------

# World bank country list by income classification
wb_country_list <- read_excel("data/OGHIST.xlsx", sheet = "Country_cat",
                              n_max = 219)

# Retain "Low" and "Low-Medium" in 2020
llm_2020 <- wb_country_list |> 
  filter(FY20 %in% c("L", "LM")) |> 
  filter(ISO3 != "TLS", ISO3 != "SSD") # countries that did not exist in 2000

# The Old files were specific to source: worldpop...
old_worldpop_files <- list.files(
  "data/Output_GEE_Worldpop",
  pattern = "\\.csv$",
  full.names = TRUE
)
old_worldpop_files
#  [1] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA.csv"      "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_1.csv"
#  [3] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_2.csv"    "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_3.csv"
#  [5] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_4.csv"    "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_IND.csv"
#  [7] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_PAK.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_PHL.csv"
#  [9] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_PNG.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_PSE1.csv"
# [11] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_PSE2.csv" "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_RWA.csv"
# [13] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SDN.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SEN.csv"
# [15] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SLB.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SLE.csv"
# [17] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SOM.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_STP.csv"
# [19] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_SYR.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_TGO.csv"
# [21] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_TJK.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_TUN.csv"
# [23] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_TZA2.csv" "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_UGA.csv"
# [25] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_UKR.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_UZB.csv"
# [27] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_VNM.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_VUT.csv"
# [29] "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_YEM.csv"  "data/Output_GEE_Worldpop/Population_PAs_ADM0_ADM1_YearlyWDPA_ZMB.csv"

# and GHSL
old_ghsl_files <- list.files(
  "data/Output_GEE_GHSL",
  pattern = "\\.csv$",
  full.names = TRUE
)
old_ghsl_files
#  [1] "data/Output_GEE_GHSL/pop_pa_118.csv"    "data/Output_GEE_GHSL/pop_pa_129.csv"    "data/Output_GEE_GHSL/pop_pa_AFG.csv"
#  [4] "data/Output_GEE_GHSL/pop_pa_AGO.csv"    "data/Output_GEE_GHSL/pop_pa_BDI.csv"    "data/Output_GEE_GHSL/pop_pa_BEN.csv"
#  [7] "data/Output_GEE_GHSL/pop_pa_BFA.csv"    "data/Output_GEE_GHSL/pop_pa_BGD.csv"    "data/Output_GEE_GHSL/pop_pa_BOL.csv"
# [10] "data/Output_GEE_GHSL/pop_pa_BTN.csv"    "data/Output_GEE_GHSL/pop_pa_CAF.csv"    "data/Output_GEE_GHSL/pop_pa_CIV.csv"
# [13] "data/Output_GEE_GHSL/pop_pa_CMR.csv"    "data/Output_GEE_GHSL/pop_pa_COD.csv"    "data/Output_GEE_GHSL/pop_pa_COG.csv"
# [16] "data/Output_GEE_GHSL/pop_pa_COM.csv"    "data/Output_GEE_GHSL/pop_pa_CPV.csv"    "data/Output_GEE_GHSL/pop_pa_DJI.csv"
# [19] "data/Output_GEE_GHSL/pop_pa_EGY.csv"    "data/Output_GEE_GHSL/pop_pa_ERI.csv"    "data/Output_GEE_GHSL/pop_pa_ETH.csv"
# [22] "data/Output_GEE_GHSL/pop_pa_FSM.csv"    "data/Output_GEE_GHSL/pop_pa_GHA.csv"    "data/Output_GEE_GHSL/pop_pa_GIN.csv"
# [25] "data/Output_GEE_GHSL/pop_pa_GMB.csv"    "data/Output_GEE_GHSL/pop_pa_GNB.csv"    "data/Output_GEE_GHSL/pop_pa_HND(1).csv"
# [28] "data/Output_GEE_GHSL/pop_pa_HND.csv"    "data/Output_GEE_GHSL/pop_pa_HTI.csv"    "data/Output_GEE_GHSL/pop_pa_IDN.csv"
# [31] "data/Output_GEE_GHSL/pop_pa_IND.csv"    "data/Output_GEE_GHSL/pop_pa_KEN.csv"    "data/Output_GEE_GHSL/pop_pa_KGZ.csv"
# [34] "data/Output_GEE_GHSL/pop_pa_KHM.csv"    "data/Output_GEE_GHSL/pop_pa_KIR.csv"    "data/Output_GEE_GHSL/pop_pa_LAO.csv"
# [37] "data/Output_GEE_GHSL/pop_pa_LBR.csv"    "data/Output_GEE_GHSL/pop_pa_LSO.csv"    "data/Output_GEE_GHSL/pop_pa_MAR.csv"
# [40] "data/Output_GEE_GHSL/pop_pa_MDA.csv"    "data/Output_GEE_GHSL/pop_pa_MDG.csv"    "data/Output_GEE_GHSL/pop_pa_MLI.csv"
# [43] "data/Output_GEE_GHSL/pop_pa_MMR.csv"    "data/Output_GEE_GHSL/pop_pa_MNG.csv"    "data/Output_GEE_GHSL/pop_pa_MOZ.csv"
# [46] "data/Output_GEE_GHSL/pop_pa_MRT.csv"    "data/Output_GEE_GHSL/pop_pa_MWI.csv"    "data/Output_GEE_GHSL/pop_pa_NER.csv"
# [49] "data/Output_GEE_GHSL/pop_pa_NGA.csv"    "data/Output_GEE_GHSL/pop_pa_NIC.csv"    "data/Output_GEE_GHSL/pop_pa_NPL.csv"
# [52] "data/Output_GEE_GHSL/pop_pa_PAK.csv"    "data/Output_GEE_GHSL/pop_pa_PHL.csv"    "data/Output_GEE_GHSL/pop_pa_PNG.csv"
# [55] "data/Output_GEE_GHSL/pop_pa_PRK.csv"    "data/Output_GEE_GHSL/pop_pa_RWA.csv"    "data/Output_GEE_GHSL/pop_pa_SDN.csv"
# [58] "data/Output_GEE_GHSL/pop_pa_SEN.csv"    "data/Output_GEE_GHSL/pop_pa_SLB.csv"    "data/Output_GEE_GHSL/pop_pa_SLE.csv"
# [61] "data/Output_GEE_GHSL/pop_pa_SLV.csv"    "data/Output_GEE_GHSL/pop_pa_SOM.csv"    "data/Output_GEE_GHSL/pop_pa_SSD.csv"
# [64] "data/Output_GEE_GHSL/pop_pa_STP.csv"    "data/Output_GEE_GHSL/pop_pa_SWZ.csv"    "data/Output_GEE_GHSL/pop_pa_SYR.csv"
# [67] "data/Output_GEE_GHSL/pop_pa_TCD.csv"    "data/Output_GEE_GHSL/pop_pa_TGO.csv"    "data/Output_GEE_GHSL/pop_pa_TJK.csv"
# [70] "data/Output_GEE_GHSL/pop_pa_TLS.csv"    "data/Output_GEE_GHSL/pop_pa_TUN.csv"    "data/Output_GEE_GHSL/pop_pa_TZA.csv"
# [73] "data/Output_GEE_GHSL/pop_pa_UGA.csv"    "data/Output_GEE_GHSL/pop_pa_UKR.csv"    "data/Output_GEE_GHSL/pop_pa_UZB.csv"
# [76] "data/Output_GEE_GHSL/pop_pa_VNM.csv"    "data/Output_GEE_GHSL/pop_pa_VUT.csv"    "data/Output_GEE_GHSL/pop_pa_YEM.csv"
# [79] "data/Output_GEE_GHSL/pop_pa_ZMB.csv"    "data/Output_GEE_GHSL/pop_pa_ZWE.csv"

# see structire
old_worldpop_sample <- read_csv(old_worldpop_files[1])
glimpse(old_worldpop_sample)
# Rows: 1
# Columns: 18
# $ `system:index`       <chr> "2_0000000000000000007f"
# $ adm_level            <dbl> 0
# $ adm_name             <chr> "Zimbabwe"
# $ country_area_km2     <dbl> 391924.2
# $ pa_area10km_2000_km2 <dbl> 149323.4
# $ pa_area10km_2020_km2 <dbl> 195317.8
# $ pa_area_2000_km2     <dbl> 69711.76
# $ pa_area_2020_km2     <dbl> 110328.4
# $ pa_count_2000        <dbl> 124
# $ pa_count_2020        <dbl> 233
# $ pop2000_in_pa        <dbl> 430018.4
# $ pop2000_in_pa10      <dbl> 3134574
# $ pop2000_total        <dbl> 9788608
# $ pop2020_in_pa        <dbl> 1121517
# $ pop2020_in_pa10      <dbl> 5295584
# $ pop2020_total        <dbl> 12369563
# $ shapeGroup           <chr> "ZWE"
# $ .geo                 <chr> "{\"type\":\"MultiPoint\",\"coordinates\":[]}"

old_ghsl_sample <- read_csv(old_ghsl_files[1])
glimpse(old_ghsl_sample)
# Rows: 1
# Columns: 18
# $ `system:index`       <chr> "2_00000000000000000094"
# $ adm_level            <dbl> 0
# $ adm_name             <chr> "Gaza Strip"
# $ country_area_km2     <dbl> 367.5655
# $ pa_area10km_2000_km2 <dbl> 122.9935
# $ pa_area10km_2020_km2 <dbl> 122.9935
# $ pa_area_2000_km2     <dbl> 0.1244595
# $ pa_area_2020_km2     <dbl> 0.1244595
# $ pa_count_2000        <dbl> 1
# $ pa_count_2020        <dbl> 1
# $ pop2000_in_pa        <dbl> 0.7519653
# $ pop2000_in_pa10      <dbl> 332595.9
# $ pop2000_total        <dbl> 1063186
# $ pop2020_in_pa        <dbl> 1.097577
# $ pop2020_in_pa10      <dbl> 543719.7
# $ pop2020_total        <dbl> 1798361
# $ shapeGroup           <chr> "PSE"
# $ .geo                 <chr> "{\"type\":\"MultiPoint\",\"coordinates\":[]}"



# Function to read and process GEE output
process_gee_data <- function(folder_path, pop_source) {
  list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) |> 
    map_dfr(read_csv, col_types = cols(shapeGroup = col_character()), show_col_types = FALSE) |> 
    select(-.geo, -`system:index`) |> 
    rename(ISO3 = shapeGroup) |> 
    group_by(ISO3) |> 
    summarise(
      country_area_km2 = first(country_area_km2),
      across(starts_with("pa_area"), \(x) sum(x, na.rm = TRUE)),
      across(starts_with("pop"), \(x) sum(x, na.rm = TRUE))
    ) |> 
    mutate(
      across(starts_with("pa_area"), ~ .x / country_area_km2 * 100, .names = "{.col}_pct"),
      across(starts_with("pop2000"), ~ .x / pop2000_total * 100, .names = paste0("{.col}_", pop_source, "_pct")),
      across(starts_with("pop2020"), ~ .x / pop2020_total * 100, .names = paste0("{.col}_", pop_source, "_pct"))
    ) %>%
    filter(ISO3 %in% llm_2020$ISO3) %>%
    left_join(select(llm_2020, ISO3, country = COUNTRY), by = "ISO3")
}

# Read and process both datasets
pa_pop_worldpop <- process_gee_data("data/Output_GEE_Worldpop", "worldpop") %>%
  arrange(country)
pa_pop_ghsl <- process_gee_data("data/Output_GEE_GHSL", "ghsl") %>%
  arrange(country)

# New files with both sources processed
new_joint_files <- list.files(
  "data/GEE_PA_Population_Analysis",
  pattern = "\\.csv$",
  full.names = TRUE
)
new_joint_files
> new_joint_files
#  [1] "data/GEE_PA_Population_Analysis/PA_Pop_AFG.csv"                         
#  [2] "data/GEE_PA_Population_Analysis/PA_Pop_AGO.csv"                         
#  [3] "data/GEE_PA_Population_Analysis/PA_Pop_BDI.csv"                         
#  [4] "data/GEE_PA_Population_Analysis/PA_Pop_BEN.csv"                         
#  [5] "data/GEE_PA_Population_Analysis/PA_Pop_BFA.csv"                         
#  [6] "data/GEE_PA_Population_Analysis/PA_Pop_BGD.csv"                         
#  [7] "data/GEE_PA_Population_Analysis/PA_Pop_BOL.csv"                         
#  [8] "data/GEE_PA_Population_Analysis/PA_Pop_BTN.csv"                         
#  [9] "data/GEE_PA_Population_Analysis/PA_Pop_CAF.csv"                         
# [10] "data/GEE_PA_Population_Analysis/PA_Pop_CIV.csv"                         
# [11] "data/GEE_PA_Population_Analysis/PA_Pop_CMR.csv"                         
# [12] "data/GEE_PA_Population_Analysis/PA_Pop_COD.csv"                         
# [13] "data/GEE_PA_Population_Analysis/PA_Pop_COG.csv"                         
# [14] "data/GEE_PA_Population_Analysis/PA_Pop_COM.csv"                         
# [15] "data/GEE_PA_Population_Analysis/PA_Pop_CPV.csv"                         
# [16] "data/GEE_PA_Population_Analysis/PA_Pop_DJI.csv"                         
# [17] "data/GEE_PA_Population_Analysis/PA_Pop_EGY.csv"                         
# [18] "data/GEE_PA_Population_Analysis/PA_Pop_ERI.csv"                         
# [19] "data/GEE_PA_Population_Analysis/PA_Pop_ETH.csv"                         
# [20] "data/GEE_PA_Population_Analysis/PA_Pop_FSM.csv"                         
# [21] "data/GEE_PA_Population_Analysis/PA_Pop_GHA.csv"                         
# [22] "data/GEE_PA_Population_Analysis/PA_Pop_GIN.csv"                         
# [23] "data/GEE_PA_Population_Analysis/PA_Pop_GMB.csv"                         
# [24] "data/GEE_PA_Population_Analysis/PA_Pop_GNB.csv"                         
# [25] "data/GEE_PA_Population_Analysis/PA_Pop_HND.csv"                         
# [26] "data/GEE_PA_Population_Analysis/PA_Pop_HTI.csv"                         
# [27] "data/GEE_PA_Population_Analysis/PA_Pop_IDN.csv"                         
# [28] "data/GEE_PA_Population_Analysis/PA_Pop_IND.csv"                         
# [29] "data/GEE_PA_Population_Analysis/PA_Pop_KEN.csv"                         
# [30] "data/GEE_PA_Population_Analysis/PA_Pop_KGZ.csv"                         
# [31] "data/GEE_PA_Population_Analysis/PA_Pop_KHM.csv"                         
# [32] "data/GEE_PA_Population_Analysis/PA_Pop_KIR.csv"                         
# [33] "data/GEE_PA_Population_Analysis/PA_Pop_LAO.csv"                         
# [34] "data/GEE_PA_Population_Analysis/PA_Pop_LBR.csv"                         
# [35] "data/GEE_PA_Population_Analysis/PA_Pop_LSO.csv"                         
# [36] "data/GEE_PA_Population_Analysis/PA_Pop_MAR.csv"                         
# [37] "data/GEE_PA_Population_Analysis/PA_Pop_MDA.csv"                         
# [38] "data/GEE_PA_Population_Analysis/PA_Pop_MDG.csv"                         
# [39] "data/GEE_PA_Population_Analysis/PA_Pop_MLI.csv"                         
# [40] "data/GEE_PA_Population_Analysis/PA_Pop_MMR.csv"                         
# [41] "data/GEE_PA_Population_Analysis/PA_Pop_MNG.csv"                         
# [42] "data/GEE_PA_Population_Analysis/PA_Pop_MOZ.csv"                         
# [43] "data/GEE_PA_Population_Analysis/PA_Pop_MRT.csv"                         
# [44] "data/GEE_PA_Population_Analysis/PA_Pop_MWI.csv"                         
# [45] "data/GEE_PA_Population_Analysis/PA_Pop_NER.csv"                         
# [46] "data/GEE_PA_Population_Analysis/PA_Pop_NGA.csv"                         
# [47] "data/GEE_PA_Population_Analysis/PA_Pop_NIC.csv"                         
# [48] "data/GEE_PA_Population_Analysis/PA_Pop_NPL.csv"                         
# [49] "data/GEE_PA_Population_Analysis/PA_Pop_PAK.csv"                         
# [50] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_ARMM.csv"               
# [51] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Bicol_Region.csv"       
# [52] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Cagayan_Valley.csv"     
# [53] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Calabarzon.csv"         
# [54] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_CAR.csv"                
# [55] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Caraga.csv"             
# [56] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Central_Luzon.csv"      
# [57] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Central_Visayas.csv"    
# [58] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Davao_Region.csv"       
# [59] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Eastern_Visayas.csv"    
# [60] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Ilocos_Region.csv"      
# [61] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Mimaropa.csv"           
# [62] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_NCR.csv"                
# [63] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Northern_Mindanao.csv"  
# [64] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Soccsksargen.csv"       
# [65] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Western_Visayas.csv"    
# [66] "data/GEE_PA_Population_Analysis/PA_Pop_PHL_ADM1_Zamboanga_Peninsula.csv"
# [67] "data/GEE_PA_Population_Analysis/PA_Pop_PNG.csv"                         
# [68] "data/GEE_PA_Population_Analysis/PA_Pop_PRK.csv"                         
# [69] "data/GEE_PA_Population_Analysis/PA_Pop_RWA.csv"                         
# [70] "data/GEE_PA_Population_Analysis/PA_Pop_SDN.csv"                         
# [71] "data/GEE_PA_Population_Analysis/PA_Pop_SEN.csv"                         
# [72] "data/GEE_PA_Population_Analysis/PA_Pop_SLB.csv"                         
# [73] "data/GEE_PA_Population_Analysis/PA_Pop_SLE.csv"                         
# [74] "data/GEE_PA_Population_Analysis/PA_Pop_SLV.csv"                         
# [75] "data/GEE_PA_Population_Analysis/PA_Pop_SOM.csv"                         
# [76] "data/GEE_PA_Population_Analysis/PA_Pop_SSD.csv"                         
# [77] "data/GEE_PA_Population_Analysis/PA_Pop_STP(1).csv"                      
# [78] "data/GEE_PA_Population_Analysis/PA_Pop_STP.csv"                         
# [79] "data/GEE_PA_Population_Analysis/PA_Pop_SWZ.csv"                         
# [80] "data/GEE_PA_Population_Analysis/PA_Pop_SYR.csv"                         
# [81] "data/GEE_PA_Population_Analysis/PA_Pop_TCD.csv"                         
# [82] "data/GEE_PA_Population_Analysis/PA_Pop_TGO.csv"                         
# [83] "data/GEE_PA_Population_Analysis/PA_Pop_TJK.csv"                         
# [84] "data/GEE_PA_Population_Analysis/PA_Pop_TLS.csv"                         
# [85] "data/GEE_PA_Population_Analysis/PA_Pop_TUN.csv"                         
# [86] "data/GEE_PA_Population_Analysis/PA_Pop_TZA.csv"                         
# [87] "data/GEE_PA_Population_Analysis/PA_Pop_UGA.csv"                         
# [88] "data/GEE_PA_Population_Analysis/PA_Pop_UKR.csv"                         
# [89] "data/GEE_PA_Population_Analysis/PA_Pop_UZB.csv"                         
# [90] "data/GEE_PA_Population_Analysis/PA_Pop_VNM.csv"                         
# [91] "data/GEE_PA_Population_Analysis/PA_Pop_VUT.csv"                         
# [92] "data/GEE_PA_Population_Analysis/PA_Pop_YEM.csv"                         
# [93] "data/GEE_PA_Population_Analysis/PA_Pop_ZMB.csv"                         
# [94] "data/GEE_PA_Population_Analysis/PA_Pop_ZWE.csv"

new_joint_sample <- read_csv(new_joint_files[1])
glimpse(new_joint_sample)
# Rows: 34
# Columns: 52
# $ iso3                        <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "…
# $ adm_level                   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
# $ country_name                <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Afghani…
# $ adm1_name                   <chr> "Kandahar", "Zabul", "Uruzgan", "Daykundi", "Ghanzi", "Paktika", "Khost", "Paktia", "Logar", "Wardak", "Kabul", "…
# $ pa_count_strict_2000        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
# $ pa_area_strict_2000_km2     <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.000…
# $ pa_area10km_strict_2000_km2 <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0…
# $ pop2000_in_pa_strict_WP     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 591, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3036
# $ pop2000_in_pa10_strict_WP   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 23882, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18993
# $ pop2000_in_pa_strict_GHSL   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7494
# $ pop2000_in_pa10_strict_GHSL <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20385, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 26451
# $ pa_count_strict_2020        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3
# $ pa_area_strict_2020_km2     <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.000…
# $ pa_area10km_strict_2020_km2 <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0…
# $ pop2020_in_pa_strict_WP     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1698, 46613, 0, 4939, 0, 613, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24…
# $ pop2020_in_pa10_strict_WP   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20040, 82324, 0, 21149, 0, 23632, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ pop2020_in_pa_strict_GHSL   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1545, 70047, 0, 2869, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42435
# $ pop2020_in_pa10_strict_GHSL <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21540, 123253, 0, 38571, 0, 31692, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ pa_count_non_2000           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1
# $ pa_area_non_2000_km2        <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0…
# $ pa_area10km_non_2000_km2    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0…
# $ pop2000_in_pa_non_WP        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1001, 0, 0, 0, 0, 0, 0, 0, 78, 6, 0, 5053
# $ pop2000_in_pa10_non_WP      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34070, 0, 0, 0, 0, 0, 0, 0, 2806, 1496, 0, 20946
# $ pop2000_in_pa_non_GHSL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1231, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10798
# $ pop2000_in_pa10_non_GHSL    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52308, 0, 0, 0, 0, 0, 0, 0, 1078, 4, 0, 31395
# $ pa_count_non_2020           <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1
# $ pa_area_non_2020_km2        <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 383.7240866, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 98.4681179, …
# $ pa_area10km_non_2020_km2    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 1822.9609, 0.0000, 0.0000, 0.0000, 0.0000, 672.5121, 411.6055, 0.0000, 0.0000, 0.…
# $ pop2020_in_pa_non_WP        <dbl> 0, 0, 0, 0, 4767, 0, 0, 0, 0, 1266, 21723, 0, 0, 0, 0, 0, 0, 0, 0, 32850, 0, 60629, 66641, 0, 75, 0, 0, 0, 0, 0, …
# $ pop2020_in_pa10_non_WP      <dbl> 0, 0, 0, 0, 27044, 0, 0, 0, 0, 20572, 2759430, 0, 0, 0, 0, 0, 0, 0, 0, 136275, 0, 167344, 193805, 0, 4481, 0, 0, …
# $ pop2020_in_pa_non_GHSL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4898, 0, 0, 0, 0, 0, 0, 0, 0, 97389, 0, 81129, 121633, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ pop2020_in_pa10_non_GHSL    <dbl> 0, 0, 0, 0, 35911, 0, 0, 0, 0, 8085, 4528679, 0, 0, 0, 0, 0, 0, 0, 0, 287360, 0, 291099, 273869, 0, 3417, 0, 0, 0…
# $ pa_count_all_2000           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 2
# $ pa_area_all_2000_km2        <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0…
# $ pa_area10km_all_2000_km2    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0…
# $ pop2000_in_pa_all_WP        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 591, 1001, 0, 0, 0, 0, 0, 0, 0, 78, 6, 0, 8090
# $ pop2000_in_pa10_all_WP      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 23882, 34070, 0, 0, 0, 0, 0, 0, 0, 2806, 1496, 0, …
# $ pop2000_in_pa_all_GHSL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1231, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18292
# $ pop2000_in_pa10_all_GHSL    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20385, 52308, 0, 0, 0, 0, 0, 0, 0, 1078, 4, 0, 460…
# $ pa_count_all_2020           <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 3, 0, 2, 2, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 4
# $ pa_area_all_2020_km2        <dbl> 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 3.837241e+02, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0…
# $ pa_area10km_all_2020_km2    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 1822.9609, 0.0000, 0.0000, 0.0000, 0.0000, 672.5121, 411.6055, 0.0000, 0.0000, 0.…
# $ pop2020_in_pa_all_WP        <dbl> 0, 0, 0, 0, 4767, 0, 0, 0, 0, 1266, 21723, 0, 0, 0, 0, 0, 1698, 46613, 0, 37789, 0, 61242, 66641, 0, 75, 0, 0, 0,…
# $ pop2020_in_pa10_all_WP      <dbl> 0, 0, 0, 0, 27044, 0, 0, 0, 0, 20572, 2759430, 0, 0, 0, 0, 0, 20040, 82324, 0, 146904, 0, 170354, 193805, 0, 4481…
# $ pop2020_in_pa_all_GHSL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4898, 0, 0, 0, 0, 0, 1545, 70047, 0, 100259, 0, 81131, 121633, 0, 0, 0, 0, 0, 0, 0,…
# $ pop2020_in_pa10_all_GHSL    <dbl> 0, 0, 0, 0, 35911, 0, 0, 0, 0, 8085, 4528679, 0, 0, 0, 0, 0, 21540, 123253, 0, 291987, 0, 297785, 273869, 0, 3417…
# $ pop2000_total_WP            <dbl> 711142, 241160, 228957, 326310, 1400917, 288709, 262721, 340801, 257709, 324998, 2478365, 904600, 251525, 284820,…
# $ pop2020_total_WP            <dbl> 1208946, 300693, 333047, 481038, 771324, 406546, 708412, 532332, 413639, 571294, 4157791, 1519507, 449811, 385160…
# $ pop2000_total_GHSL          <dbl> 896988, 225083, 254504, 333776, 1608161, 310556, 287081, 377635, 285682, 301674, 3120451, 998465, 275849, 310871,…
# $ pop2020_total_GHSL          <dbl> 1908593, 409662, 507386, 666385, 1284339, 595302, 1053192, 810703, 619846, 723409, 6006457, 2276142, 683500, 5678…
# $ country_area_km2            <dbl> 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642028.1, 642…
# $ adm_area_km2                <dbl> 53386.649, 17549.385, 10797.801, 17984.572, 22851.647, 18963.565, 3912.093, 5488.689, 4848.484, 9811.774, 4527.67…

# Read all new joint files and bind into one dataframe
new_joint_files_all <- list.files("data/GEE_PA_Population_Analysis",
                  pattern = "\\.csv$",
                  full.names = TRUE)

new_joint_all <- new_joint_files_all |>
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

# Ensure consistent ISO3 and country column names
if ("iso3" %in% names(new_joint_all)) {
  new_joint_all <- new_joint_all |> rename(ISO3 = iso3)
}
if ("country_name" %in% names(new_joint_all)) {
  new_joint_all <- new_joint_all |> rename(country = country_name)
}
# if country column already named COUNTRY or country, prefer lowercase 'country'
if ("COUNTRY" %in% names(new_joint_all) & !("country" %in% names(new_joint_all))) {
  new_joint_all <- new_joint_all |> rename(country = COUNTRY)
}

# Identify numeric columns to sum (we will keep country_area_km2 as first)
numeric_cols <- new_joint_all |> select(where(is.numeric)) |> names()
sum_cols <- setdiff(numeric_cols, "country_area_km2")

# Consolidate by country (ISO3)
pa_pop_new_joint <- new_joint_all |>
  group_by(ISO3, country) |>
  summarise(
  country_area_km2 = first(country_area_km2),
  across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
  .groups = "drop"
  )

# Compute % area for any pa_area *_km2 columns
area_cols <- pa_pop_new_joint |> select(matches("pa_area.*_km2$")) |> names()
if (length(area_cols) > 0) {
  pa_pop_new_joint <- pa_pop_new_joint |>
  mutate(across(all_of(area_cols),
          ~ .x / country_area_km2 * 100,
          .names = "{.col}_pct"))
}

# Compute population percentages:
# for each column like pop2000_in_pa_*_{WP|GHSL} create pop..._pct using pop{year}_total_{WP|GHSL}
pop_in_pattern <- "pop(\\d{4})_in_.*_(WP|GHSL)$"
pop_in_cols <- pa_pop_new_joint |> select(matches(pop_in_pattern)) |> names()

for (col in pop_in_cols) {
  # extract year and source
  yr <- str_extract(col, "\\d{4}")
  src <- str_extract(col, "WP|GHSL")
  total_col <- paste0("pop", yr, "_total_", src)
  pct_col <- paste0(col, "_pct")
  if (total_col %in% names(pa_pop_new_joint)) {
  pa_pop_new_joint <- pa_pop_new_joint |>
    mutate(!!pct_col := if_else(.data[[total_col]] > 0,
                  .data[[col]] / .data[[total_col]] * 100,
                  NA_real_))
  } else {
  pa_pop_new_joint <- pa_pop_new_joint |>
    mutate(!!pct_col := NA_real_)
  }
}

# Also compute percentages for pop..._in_pa10_*_{WP|GHSL} if pattern differs (catch any remaining)
pop_in_10_pattern <- "pop(\\d{4})_in_.*10.*_(WP|GHSL)$"
pop_in_10_cols <- pa_pop_new_joint |> select(matches(pop_in_10_pattern)) |> names()
for (col in setdiff(pop_in_10_cols, pop_in_cols)) {
  yr <- str_extract(col, "\\d{4}")
  src <- str_extract(col, "WP|GHSL")
  total_col <- paste0("pop", yr, "_total_", src)
  pct_col <- paste0(col, "_pct")
  if (total_col %in% names(pa_pop_new_joint)) {
  pa_pop_new_joint <- pa_pop_new_joint |>
    mutate(!!pct_col := if_else(.data[[total_col]] > 0,
                  .data[[col]] / .data[[total_col]] * 100,
                  NA_real_))
  } else {
  pa_pop_new_joint <- pa_pop_new_joint |>
    mutate(!!pct_col := NA_real_)
  }
}

# Ensure ordering and a reproducible column order: country, ISO3, country_area_km2, then everything else
other_cols <- setdiff(names(pa_pop_new_joint), c("country", "ISO3", "country_area_km2"))
pa_pop_new_joint <- pa_pop_new_joint |> select(country, ISO3, country_area_km2, all_of(other_cols))

# Return the consolidated dataframe
pa_pop_new_joint

# compute India's share for all pop{year}_total_{WP|GHSL} columns
cols_to_use <- pa_pop_new_joint |>
  select(matches("^pop\\d{4}_total_(WP|GHSL)$")) |>
  names()

total_pop_by_var <- pa_pop_new_joint |>
  summarise(across(all_of(cols_to_use), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "var", values_to = "total_pop") |>
  mutate(year = str_extract(var, "\\d{4}"),
         source = str_extract(var, "WP|GHSL"))

india_pop_by_var <- pa_pop_new_joint |>
  filter(ISO3 == "IND") |>
  summarise(across(all_of(cols_to_use), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "var", values_to = "india_pop") |>
  mutate(year = str_extract(var, "\\d{4}"),
         source = str_extract(var, "WP|GHSL"))

india_share <- india_pop_by_var |>
  select(-var) |>
  left_join(select(total_pop_by_var, -var), by = c("year", "source")) |>
  mutate(share_prop = if_else(total_pop > 0, india_pop / total_pop, NA_real_),
    share_pct = share_prop * 100) |>
  select(year, source, india_pop, total_pop, share_prop, share_pct)

india_share

# Merge without duplicating `pa_area` fields
pa_pop_combined <- full_join(pa_pop_worldpop, pa_pop_ghsl, 
                             by = c("ISO3", "country"),
                             suffix = c("_worldpop", "_ghsl")) %>%
  # areas are the same btw wordlpop and ghsl, we rename worlpop
  rename(country_area_km2 = country_area_km2_worldpop,
         pa_area_2000_km2 = pa_area_2000_km2_worldpop,
         pa_area_2020_km2 = pa_area_2020_km2_worldpop,
         pa_area10km_2000_km2 = pa_area10km_2000_km2_worldpop,
         pa_area10km_2020_km2 = pa_area10km_2020_km2_worldpop,
         pa_area_2000_km2_pct = pa_area_2000_km2_pct_worldpop,
         pa_area_2020_km2_pct = pa_area_2020_km2_pct_worldpop,
         pa_area10km_2000_km2_pct = pa_area10km_2000_km2_pct_worldpop,
         pa_area10km_2020_km2_pct = pa_area10km_2020_km2_pct_worldpop) %>%
  # and remove the ghsl
  select(-country_area_km2_ghsl, -pa_area_2000_km2_ghsl, -pa_area_2020_km2_ghsl,
         -pa_area10km_2000_km2_ghsl, pa_area10km_2020_km2_ghsl)

# Compute "Total" (all countries)
compute_totals <- function(data, label) {
  data %>%
    summarise(
      country = label,
      country_area_km2 = sum(country_area_km2, na.rm = TRUE),
      
      # Compute total PA areas in km2 (not percentages)
      pa_area_2000_km2 = sum(pa_area_2000_km2, na.rm = TRUE),
      pa_area_2020_km2 = sum(pa_area_2020_km2, na.rm = TRUE),
      pa_area10km_2000_km2 = sum(pa_area10km_2000_km2, na.rm = TRUE),
      pa_area10km_2020_km2 = sum(pa_area10km_2020_km2, na.rm = TRUE),
      
      # Compute total population counts
      pop2000_total_worldpop = sum(pop2000_total_worldpop, na.rm = TRUE),
      pop2020_total_worldpop = sum(pop2020_total_worldpop, na.rm = TRUE),
      pop2000_total_ghsl = sum(pop2000_total_ghsl, na.rm = TRUE),
      pop2020_total_ghsl = sum(pop2020_total_ghsl, na.rm = TRUE),
      
      # Compute total populations in PAs
      pop2000_in_pa_worldpop = sum(pop2000_in_pa_worldpop, na.rm = TRUE),
      pop2020_in_pa_worldpop = sum(pop2020_in_pa_worldpop, na.rm = TRUE),
      pop2000_in_pa_ghsl = sum(pop2000_in_pa_ghsl, na.rm = TRUE),
      pop2020_in_pa_ghsl = sum(pop2020_in_pa_ghsl, na.rm = TRUE),
      
      # Compute total populations in 10km buffers
      pop2000_in_pa10_worldpop = sum(pop2000_in_pa10_worldpop, na.rm = TRUE),
      pop2020_in_pa10_worldpop = sum(pop2020_in_pa10_worldpop, na.rm = TRUE),
      pop2000_in_pa10_ghsl = sum(pop2000_in_pa10_ghsl, na.rm = TRUE),
      pop2020_in_pa10_ghsl = sum(pop2020_in_pa10_ghsl, na.rm = TRUE)
    ) %>%
    mutate(
      # Compute percentages properly using total areas and population counts
      pa_area_2000_km2_pct = pa_area_2000_km2 / country_area_km2 * 100,
      pa_area_2020_km2_pct = pa_area_2020_km2 / country_area_km2 * 100,
      pa_area10km_2000_km2_pct = pa_area10km_2000_km2 / country_area_km2 * 100,
      pa_area10km_2020_km2_pct = pa_area10km_2020_km2 / country_area_km2 * 100,
      
      pop2000_in_pa_worldpop_pct = pop2000_in_pa_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa_worldpop_pct = pop2020_in_pa_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa_ghsl_pct = pop2000_in_pa_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa_ghsl_pct = pop2020_in_pa_ghsl / pop2020_total_ghsl * 100,
      
      pop2000_in_pa10_worldpop_pct = pop2000_in_pa10_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa10_worldpop_pct = pop2020_in_pa10_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa10_ghsl_pct = pop2000_in_pa10_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa10_ghsl_pct = pop2020_in_pa10_ghsl / pop2020_total_ghsl * 100
    )
}

# Compute "Total" for all countries
pa_pop_total <- compute_totals(pa_pop_combined, "Total")

# Compute "Total without India"
pa_pop_total_no_india <- pa_pop_combined %>%
  filter(ISO3 != "IND") %>% 
  compute_totals("Total without India")

# Combine totals with main dataset
pa_pop_summary_with_total <- bind_rows(pa_pop_total, 
                                       pa_pop_total_no_india, 
                                       pa_pop_combined) %>%
  select(country, ISO3,
         pa_area_2000_km2_pct, pa_area_2020_km2_pct,
         pa_area10km_2000_km2_pct, pa_area10km_2020_km2_pct,
         pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct,
         pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct,
         pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct,
         pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct)


# Save intermediary result for manuscript citation ------------------------


# Save for in-text citation
save(pa_pop_worldpop, pa_pop_ghsl, pa_pop_total, pa_pop_total_no_india, 
     file = "results/pa_pop_total.rds")

# Produce Table S1 --------------------------------------------------------


# Generate Table S1 with expanded structure
pa_pop_summary_with_total %>%
  select(-ISO3) %>%
  gt() %>%
  tab_header(
    title = "Table S1: PA Coverage and Population Proximity (2000-2020)",
    subtitle = "Percentage of land and population within and near protected areas"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  # Level 3 headers: 2000 | 2020
  cols_label(
    country = "Country",
    pa_area_2000_km2_pct = "2000", pa_area_2020_km2_pct = "2020",
    pop2000_in_pa_worldpop_pct = "2000", pop2020_in_pa_worldpop_pct = "2020",
    pop2000_in_pa_ghsl_pct = "2000", pop2020_in_pa_ghsl_pct = "2020",
    pa_area10km_2000_km2_pct = "2000", pa_area10km_2020_km2_pct = "2020",
    pop2000_in_pa10_worldpop_pct = "2000", pop2020_in_pa10_worldpop_pct = "2020",
    pop2000_in_pa10_ghsl_pct = "2000", pop2020_in_pa10_ghsl_pct = "2020"
  ) %>%
  # Level 2 headers: Land (WDPA), Population (WorldPop), Population (GHSL)
  tab_spanner(label = "within PAs", 
              columns = c(pa_area_2000_km2_pct, pa_area_2020_km2_pct),
              id = "land_pa") %>%
  tab_spanner(label = "within PAs", 
              columns = c(pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct),
              id = "worldpop_pa") %>%
  tab_spanner(label = "within PAs", 
              columns = c(pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct),
              id = "ghsl_pa") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pa_area10km_2000_km2_pct, pa_area10km_2020_km2_pct),
              id = "land_pa10") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct),
              id = "worldpop_pa10") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct),
              id = "ghsl_pa10") %>%
  # Level 1 headers: Land (WDPA), Population (WorldPop), Population (GHSL)
  tab_spanner(label = "% land within (WDPA)", 
              spanners = c("land_pa", "land_pa10")) %>%
  tab_spanner(label = "% population (WorldPop)", 
              spanners = c("worldpop_pa", "worldpop_pa10")) %>%
  tab_spanner(label = "% population (GHSL)", 
              spanners = c("ghsl_pa", "ghsl_pa10")) %>%
  # Format
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(0),
    column_labels.padding = px(0)
  ) %>%
  tab_source_note(source_note = "Source: Analysis based on WDPA, WorldPop & GHSL data (2000-2020)") %>%
  
  # Thin vertical lines for all columns
  tab_style(
    style = cell_borders(sides = "right", weight = px(0.5)),
    locations = cells_body(columns = everything())
  ) %>%
  
  # Thick vertical lines
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_body(columns = c(country, pa_area10km_2020_km2_pct,
                                       pop2020_in_pa10_ghsl_pct))
  ) %>%
  
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = country %in% c("Total", "Total without India"))
  ) -> table_s1
table_s1

# Save table in multiple formats
gtsave(table_s1, "results/table_s1.html")
gtsave(table_s1, "results/table_s1.tex")
gtsave(table_s1, "results/table_s1.docx")
write_rds(table_s1, "results/table_s1.rds")

# Produce Table 1 ---------------------------------------------------------

compute_smd <- function(x1, x2) {
  mean_diff <- abs(mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE))
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
  smd <- mean_diff / pooled_sd
  return(smd)
}

# Compute SMDs for 2000 and 2020
smd_results <- tibble(
  Metric = c("Population within PAs", "Population within 10km of PAs"),
  `2000` = c(
    compute_smd(pa_pop_combined$pop2000_in_pa_worldpop, pa_pop_combined$pop2000_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2000_in_pa10_worldpop, pa_pop_combined$pop2000_in_pa10_ghsl)
  ),
  `2020` = c(
    compute_smd(pa_pop_combined$pop2020_in_pa_worldpop, pa_pop_combined$pop2020_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2020_in_pa10_worldpop, pa_pop_combined$pop2020_in_pa10_ghsl)
  )
)

# Create the gt table
table_1 <- smd_results %>%
  gt() %>%
  tab_header(
    title = "Table 1: Standard Mean Differences Between GHSL and Worldpop Estimates"
  ) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(2),
    column_labels.padding = px(2)
  ) %>%
  cols_label(Metric = "") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = everything()), 
      cells_body(rows = everything(), columns = Metric) 
    )
  )
table_1

# Save the table
gtsave(table_1, "results/table_1.html")
gtsave(table_1, "results/table_1.tex")
gtsave(table_1, "results/table_1.docx")
write_rds(table_1, "results/table_1.rds")


# Produce Table S2 ----------------------------------------------------------

# Compute differences between Worldpop and GHSL

pa_pop_diff <- pa_pop_combined %>%
  select(ISO3, country,
         pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct,
         pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct,
         pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct,
         pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct)  %>%
  pivot_longer(
    cols = -c(ISO3, country),
    names_to = c("year", "metric", "source"),
    names_pattern = "pop(\\d{4})_in_(pa10|pa)_(worldpop|ghsl)"
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  pivot_wider(
    names_from = source,
    values_from = value
  ) %>%
  mutate(abs_diff = abs(worldpop - ghsl),
         diff_perc = (abs_diff / ghsl))

pa_area_diff <- pa_pop_ghsl %>%
  select(ISO3, pa_area_2000_km2_pct, pa_area_2020_km2_pct) %>%
  pivot_longer(
    cols = -ISO3,
    names_to = c("year"),
    names_pattern = "pa_area_(\\d{4})_km2_pct",
    values_to = "pa_area_pct"
  ) %>%
  mutate(year = as.numeric(year))

pa_pop_diff <- pa_pop_diff %>%
  left_join(pa_area_diff, by = c("ISO3", "year"))

major_discrepancies <- pa_pop_diff %>%
  filter(abs_diff > 5 & diff_perc > 0.1) %>%
  arrange(desc(abs_diff))

# length(unique(major_discepancies$ISO3))

table_s2 <- major_discrepancies %>%
  mutate(
    worldpop = worldpop / 100,
    ghsl = ghsl / 100,
    metric = case_when(
      metric == "pa" ~ "Within PAs",
      metric == "pa10" ~ "Within 10km of PAs"
    )
  ) %>%
  select(country, year, metric, worldpop, ghsl, abs_diff, diff_perc) %>%
  gt() %>%
  # Table title and subtitle
  tab_header(
    title = md("Table S2: Largest Differences Between GHSL and WorldPop Estimates"),
    subtitle = md("*Absolute difference of more then 5 percentage points (pp) and relative difference of more than 10%*")
  ) %>%
  # Adjust column labels to be on two lines
  cols_label(
    country = "Country",
    year = "Year",
    metric = "Area",
    worldpop = md("**WorldPop**<br>** Estimate**"),
    ghsl = md("**GHSL**<br>** Estimate**"),
    abs_diff = md("**Absolute**<br>**Difference**"),
    diff_perc = md("**Relative**<br> **Difference**")
  ) %>%
  # Bold column headers
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Format numeric values: percentages for estimates and relative differences
  fmt_percent(
    columns = c(worldpop, ghsl, diff_perc),
    decimals = 1
  ) %>%
  # Format absolute difference with "pp" suffix
  fmt_number(
    columns = abs_diff,
    decimals = 1,
    suffixing = FALSE
  ) %>%
  text_transform(
    locations = cells_body(columns = abs_diff),
    fn = function(x) paste0(x, " pp")
  ) %>%
  # Align all columns to the center
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # Adjust table appearance
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(4),  # Adjust spacing
    column_labels.padding = px(6)  # Increase padding for two-line headers
  )

gtsave(table_s2, "results/table_s2.html")
gtsave(table_s2, "results/table_s2.tex")
gtsave(table_s2, "results/table_s2.docx")
write_rds(table_s2, "results/table_s2.rds")

# Produce Figure 1 ---------------------------------------------------------

# % of Population Near PAs
ggplot(pa_pop_combined, aes(x = reorder(country, pop2020_in_pa10_ghsl))) +
  geom_point(aes(y = pop2000_in_pa10_ghsl), color = "gray", size = 3) +
  geom_point(aes(y = pop2020_in_pa10_ghsl), color = "steelblue", size = 3) +
  geom_segment(aes(y = pop2000_in_pa10_ghsl, 
                   yend = pop2020_in_pa10_ghsl, 
                   xend = country, 
                   color = pop2020_in_pa10_ghsl > pop2000_in_pa10_ghsl), 
               linewidth = 1.2) +
  scale_color_manual(values = c("red", "steelblue"), 
                     labels = c("Decrease", "Increase")) +
  coord_flip() +
  labs(
    title = "Figure 1: Change in % Population Near PAs (GHSL 2000-2020)",
    x = NULL,
    y = "% of Population",
    color = "Trend"
  ) +
  theme_minimal() -> figure_1
figure_1
ggsave("results/figure_1.png", plot = figure_1, width = 8, height = 9, ,
       unit = "in", dpi = 300)

# Produce Figure 2 ---------------------------------------------------------

# Prepare a note with the country names
country_note <- llm_2020 %>%
  select(ISO3, country = COUNTRY) %>%
  arrange(ISO3) %>%
  mutate(text = paste0(ISO3, ": ", country)) %>%
  pull(text) %>%
  paste(collapse = ", ")

# Define zoom parameters
x_zoom1 = 1 # x limit
y_zoom1 = 1 # y limit
col_zoom1 = "purple" # zoom box color

# Compute trend and filter to avoid inflated growth ratios
pa_pop_summary2 <- pa_pop_worldpop |> 
  mutate(var_land = (pa_area_2020_km2_pct - pa_area_2000_km2_pct) / 
           pa_area_2000_km2_pct,
         var_pop = (pop2020_in_pa10_worldpop_pct - pop2000_in_pa10_worldpop_pct) / 
           pop2000_in_pa10_worldpop_pct) |> 
  filter(pa_area_2000_km2_pct > 1)


# Compute the number of countries in the subsample
num_countries <- nrow(pa_pop_summary2)



# Create a dummy dataframe for the legend entry
legend_line <- data.frame(x = c(0, 1), y = c(0, 1), type = "Equal PA & Population Growth")

# Create plot
figure_2 <- pa_pop_summary2 |> 
  ggplot(aes(x = var_land, y = var_pop, label = ISO3)) +
  geom_point(aes(size = pop2020_total / 1e6), color = "blue", alpha = 0.7) +
  scale_size_continuous(
    breaks = c(10, 100, 200),
    labels = c("10", "100", "200"),
    guide = guide_legend(
      title = "Total Population (millions)",
      title.position = "top"
    )
  ) +
  annotate("rect", xmin = 0, xmax = x_zoom1, ymin = 0, ymax = y_zoom1, 
           color = col_zoom1, fill = NA, linewidth = 1) +
  geom_text_repel(size = 3) +  # Add country labels
  geom_abline(slope = 1, intercept = 0, color = "green") +  
  geom_line(data = legend_line, aes(x = x, y = y, linetype = type), 
            color = "green", inherit.aes = FALSE) + 
  scale_linetype_manual(name = NULL, values = c("Equal PA & Population Growth" = "solid")) + 
  labs(
    title = "Figure 2: Evolution of PA Land Coverage and Nearby Population Share (GHSL 2000-2020)",
    x = "Growth in Land Share Covered by PAs", 
    y = "Growth in Population Share Within 10km of PAs",
    subtitle = paste0("Subsample of ", nrow(pa_pop_summary2), 
                      " countries classified as Low- or Low-Middle Income in 2020, ",
                      "with at least\n1% PA coverage in 2000")  
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.position = "right"
  )

# Create zoom by modifying plot
figure_2_zoom <- figure_2 +
  scale_x_continuous(breaks = seq(0, x_zoom1, by = 1), limits = c(0, x_zoom1)) +  
  scale_y_continuous(breaks = seq(0, y_zoom1, by = 1), limits = c(0, y_zoom1)) +
  labs(title = NULL, subtitle = NULL, x = " ", y = " ",
       caption = str_wrap(paste("Country codes:", country_note), 
                          width = 140)) +   # Remove title and labels
  theme(
    plot.caption = element_text(size = 8, hjust = 0)  # Adjust caption size & alignment
  )
# Remove legend from plot
figure_2 <- figure_2 + theme(legend.position = "none")

# Arrange using cowplot
figure_2 <- plot_grid(
  figure_2,
  figure_2_zoom, 
  nrow = 2,
  rel_heights = c(1,1)  # Main plot takes more height
)

# Display the final arranged plot
save_plot("results/figure_2.png", plot = figure_2, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)

# Produce Figure 3 --------------------------------------------------------

library(ggplot2)
library(ggrepel)
library(scales)
library(cowplot)

# Define zoom parameters
x_zoom2 = 3 # x limit
y_zoom2 = 5 # y limit
col_zoom2 = "purple" # zoom box color

# Determine the max values for dynamic axis limits
max_x <- max(pa_pop_worldpop$pa_area_2020_km2_pct, na.rm = TRUE)
max_y <- max(pa_pop_worldpop$pop2020_in_pa10_worldpop_pct, na.rm = TRUE)

# Create my_plot with the legend
figure_3 <- ggplot(pa_pop_worldpop, aes(x = pa_area_2020_km2_pct, 
                                                   y = pop2020_in_pa10_worldpop_pct, 
                                                   size = pop2020_total / 1e6)) +
  annotate("rect", xmin = 0, xmax = x_zoom2, ymin = 0, ymax = y_zoom2, 
           color = col_zoom2, fill = NA, linewidth = 1) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_text_repel(aes(label = ISO3), size = 3) +  
  scale_size_continuous(
    breaks = c(10, 100, 200),
    labels = c("10", "100", "200"),
    guide = guide_legend(title = "Total Population (millions)", 
                         title.position = "top")
  ) +
  scale_x_continuous(breaks = seq(0, max_x, by = 10), limits = c(0, max_x)) +  
  scale_y_continuous(breaks = seq(0, max_y, by = 10), limits = c(0, max_y)) +  
  labs(
    title = "Figure 3: PA Coverage vs Population Near PAs (2020)",
    x = "% of Country Covered by PAs", 
    y = "% of Population Near PAs"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line( size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"  # Keep legend for extraction
  )

# Create my_plot_zoom by modifying my_plot
figure_3_zoom <- figure_3 +
  scale_x_continuous(breaks = seq(0, x_zoom2, by = 1), limits = c(0, x_zoom2)) +  
  scale_y_continuous(breaks = seq(0, y_zoom2, by = 1), limits = c(0, y_zoom2)) +
  labs(title = NULL, x = " ", y = " ",
       caption = str_wrap(paste("Country codes:", country_note), 
                          width = 140))  +
  theme(
    plot.caption = element_text(size = 8, hjust = 0)  
  )

# Remove legend from my_plot
figure_3 <- figure_3 + theme(legend.position = "none")

# Arrange using cowplot
figure_3 <- plot_grid(
  figure_3,
  figure_3_zoom,  
  nrow = 2,
  rel_heights = c(1, 1) 
)

# Display the final arranged plot
save_plot("results/figure_3.png", plot = figure_3, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)


# Produce Figure 4 --------------------------------------------------------


# Create the lollipop plot
figure_4 <- pa_pop_diff %>%
  filter(!is.na(worldpop) & !is.na(ghsl)) %>%
  ggplot(aes(y = pa_area_pct)) +
  # Connecting line (kept blue for clarity)
  geom_segment(aes(x = ghsl, xend = worldpop, yend = pa_area_pct), 
               color = "blue", linewidth = 1) +
  # GHSL black dot with legend
  geom_point(aes(x = ghsl, color = "GHSL"), size = 2) +
  # WorldPop blue dot with legend
  geom_point(aes(x = worldpop, color = "WorldPop"), size = 2) +
  # Facet by year (horizontal) and metric (vertical) with custom labels
  facet_grid(rows = vars(metric), cols = vars(year),
             labeller = labeller(metric = c("pa" = "Within PAs", "pa10" = "Within 10km of PAs"))) +
  # Define colors in the legend
  scale_color_manual(
    name = "Population Source",
    values = c("GHSL" = "black", "WorldPop" = "blue")
  ) +
  # Labels and theme
  labs(
    title = "Figure 4: Comparison of Population Estimates from GHSL and WorldPop",
    x = "Population Estimate (% of National Population)",
    y = "PA Coverage (% of National Territory)"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot("results/figure_4.png", plot = figure_4, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)



# Figures from Naidoo 2019 ------------------------------------------------

# Estimations from Naidoo 2019 --------------------------------------------

naidoo_tb <- read_csv("data/Naidoo2019-TableS1.csv", 
                      col_types = cols(`Survey Years` = col_character())) |> 
  mutate(perc_households = (Households_10km_PA / Households_n) * 100)

perc_hh_10km_pa <- naidoo_tb |> 
  summarise(mean_perc_hh = mean(perc_households))

# perc_hh_10km_pa
# # A tibble: 1 × 1
# mean_perc_hh
# <dbl>
#   1        0.222

