# SCAPE dataset processing script
# January 5, 2021
# Heili Lowman

# The following script will take the enormous dataset of SCAPE tool outputs, complete with stream segment geometries, and instead output a much smaller .csv file without said geometries.

# Load packages
library(tidyverse)
library(sf)

# Load shape file with SCAPE modeled stream constraints.
constraints <- st_read("/Users/heilil/Desktop/smc_datasets/strm_constraints/strm_constraints.shp")

# remove geometries column
con_wo_geom <- constraints %>%
  as_tibble() %>%
  select(-geometry)

# making smaller dataset for use in sqi shiny app
con_trim <- con_wo_geom %>%
  select(COMID, Ref10, qt10, qt50, qt90)

# Export files for use.
write_csv(con_wo_geom, "strm_constraints.csv")
write_csv(con_trim, "strm_constraints_trim.csv")

# End of script.