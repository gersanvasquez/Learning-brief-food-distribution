###################################################

#Create a map of choropleth map of Haiti
#Communes reached during food distribution

#Last updated: 7 October 2024
#Created by: Gersan Vasquez
##################################################

# Load packages
library(sf)
library(tidyverse)
library(ggrepel)
library(viridis)


#Create dataframe using communes reached andfood packages by round
df <- data.frame(
  area = c("Delmas", "Tabarre", "Petion-Ville", "Port-au-Prince", "Croix-Des-Bouquets", "Mirebalais"),
  lat = c(18.5434935, 18.56978451, 18.51402366, 18.53549774, 18.58203215, 18.83501759),
  long = c(-72.30466672, -72.25882352, -72.28719017, -72.3169073, -72.22655181, -72.1023918),
  round1 = c(745, 199, 277, 110, 45, 450),
  round2 = c(1072, 98, NA, 130, NA, NA),
  total = c(1817, 297, 277, 240, 45, 450)
)


# Set the working directory
setwd("C:/Users/GersanVasquez/Documents/CORE/Shapefiles/hti_adm_cnigs_20181129")

# Read the Haiti shapefile
haiti <- st_read("hti_admbnda_adm2_cnigs_20181129.shp")

#Visualize it
plot(haiti)
unique(haiti$ADM2_EN)

#Merge both datasets
mapap <- haiti %>% 
  filter(ADM2_EN %in% c("Delmas", "Tabarre", "Petion-Ville", "Port-au-Prince", "Croix-Des-Bouquets", "Mirebalais"))

#Visualize it
plot(mapap)

# Merge the shapefile data with the df data on area names
mapap_merged <- mapap %>% 
left_join(df, by = c("ADM2_EN" = "area"))

#Create Choropleth map
ggplot(data = mapap_merged) +
geom_sf(aes(fill = total), color = "white") +  # 'total' to fill color
geom_point(aes(x = long, y = lat), color = "black", size = 2) +
geom_text_repel(
aes(x = long, y = lat, label = ADM2_EN),
size = 3.5,
nudge_x = 0, 
nudge_y = 0,
segment.color = "black",
color = "black"
) +
scale_fill_gradientn(
colors = c(
"#fcfdbf", 
"#fdb365",  
"#d34d3e",  
"#9c179e",  
"#3b0f70"  
),     
trans = "log", 
breaks = c(50, 300, 1800)) +
labs(
caption = "Author's elaboration",
x = "",
y = ""
) +
theme_ipsum_rc()+
theme(
legend.position = "right",
legend.title = element_blank()
)+
theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.text.y = element_blank(),
axis.text.x = element_blank(),
plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e"),
strip.text = element_text(hjust = 0.5, size = 14,vjust = 0.75,face="bold")) 
  
    
