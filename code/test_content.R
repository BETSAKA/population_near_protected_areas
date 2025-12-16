library(tidyverse)

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
