#Refinery parcel
refinery <- readOGR(dsn="refinery/", layer="Refinery") %>%
  spTransform(., "+init=EPSG:4326") %>%
  st_as_sf(.)

#Save as .rds
st_write(refinery, "refinery.geojson") 