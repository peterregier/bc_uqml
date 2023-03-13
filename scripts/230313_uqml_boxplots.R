## This script creates descriptive figures for the dataset used in the UQML 
## manuscript authored by Hou et al. All data were collected at Beaver Creek.
##
## Peter Regier
## 2023-03-13
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## load packages
require(pacman)
p_load(tidyverse,
       cowplot,
       janitor)

## set plot theme
theme_set(theme_bw())


# 2. Load data -----------------------------------------------------------------

## Source: smb://pnl.gov/Projects/BeaverCreek_UQML/data/Uploaded_Jun23_2020/Premis_BC_compiled_Merged_Cora_6_23_20.csv
df_raw <- read_csv("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/projects/premis_pr/data/Premis_BC_compiled_Merged_Cora_6_23_20.csv") %>% 
  clean_names() %>% 
  filter(grepl("BC", point)) %>% # Remove "F", "tree.397" and NAs
  mutate(plot = fct_relevel(as.factor(plot), c("marsh", "mouth", "middle", "freshwater"))) %>% 
  mutate(landscape = fct_relevel(as.factor(landscape), c("river", "river.bank", "floodplain", "hillslope"))) %>% 
  filter(plot != "freshwater")

make_plot <- function(var, y_label){
  ggplot(df_raw, aes(landscape, {{var}})) + geom_boxplot() + 
    facet_wrap(~plot, scales = "free", nrow = 1) + 
    labs(x = "", y = y_label)
}

plot_grid(make_plot(ch4_umolar, "CH4 (uM)"), 
          make_plot(sensor_hydraulic_head_m, "Water level (m)"), 
          make_plot(salinity_psu, "Salinity (PSU)"),
          make_plot(do_mgl, "DO (mg/L)"), 
          make_plot(ph, "pH"), 
          make_plot(temperature_c, "Temp (C)"), 
          make_plot(doc_mgl, "DOC (mg/L)"), 
          ncol = 1, align = "hv")
ggsave("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/230313_BC_UQML_boxplots.png", width = 9, height = 13)



