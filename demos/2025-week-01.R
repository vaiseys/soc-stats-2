library(tidyverse)
library(gssr)
library(gssrdoc)

gss2024 <- gss_get_yr(2024)

saveRDS(gss2024, "data/gss2024.rds")
