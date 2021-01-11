# GISS_FinalProject

Loading a few packages I will use during my project.

```{r, cache=TRUE}
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(fs)
library(tidyverse)
library(stringr)
library(raster)
library(janitor)
library(wesanderson)

```

Loading London Boroughs boundaries
```{r, cache=TRUE}
LondonBoroughs <- st_read(here::here("Data", "statistical-gis-boundaries-london",
                                     "ESRI", "London_Borough_Excluding_MHW.shp"))

```

Project the map
```{r, cache=TRUE}
library(stringr)
LondonBoroughs <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(LondonBoroughs)

```






```{r, cache=TRUE}


```
