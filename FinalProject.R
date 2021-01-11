#Loading a few packages I will use during my project
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

#Loading London Boroughs boundaries
LondonBoroughs <- st_read(here::here("Data", "statistical-gis-boundaries-london",
                                     "ESRI", "London_Borough_Excluding_MHW.shp"))
#Project the map
library(stringr)
LondonBoroughs <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(LondonBoroughs)

#Loading shapefile of LSOAs
lsoas <- st_read(here::here("Data", "2011_london_boundaries","LSOAs","LSOAs.shp"))

#Adjust projections of LsOAs
lsoas <- lsoas %>%
  st_transform(., 27700)

qtm(lsoas)

#Load csv files of Accessibility Analysis
ptal <- read_csv(here::here("Data","PTAL_2015.csv"),
                 locale = locale(encoding = "latin1"),
                 na = "n/a")
#Examine the file, variables, columns and cleaning
Datatypelist1 <- ptal %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist1

ptal%>%
  colnames()
 
ptal<-ptal %>%
  distinct()

#convert into spatial data
ptal<-ptal%>%
  st_as_sf(.,coords = c("X", "Y"), 
           crs =27700, 
           agr = "constant")

#Distribution of PTAL across London
ggplot(ptal, aes(x = PTAL2015))  +
  geom_histogram(fill="white",colour="black", stat="count")

#----------------NEWHAM------------
#Verify distribution of PTAL in Newham
#Newham boundaries
Newham <- LondonBoroughs %>%
  filter(., NAME=="Newham")

#Check to see that the correct borough has been pulled out
tm_shape(Newham) +
  tm_polygons(col = NA, alpha = 0.25)

#clip the data to our single borough
ptalNewham <- ptal[Newham,]
#check that it's worked
tmap_mode("view")
## tmap mode set to interactive viewing
tmap_mode("plot")
  tm_shape(Newham) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(ptalNewham) +
  tm_dots(col = "PTAL2015",palette="Set3",size = 0.65,shape = 22,
          title = "PTAL 2015")+
  tm_compass(north=0,type="rose",size=2)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_layout(title = "PTAL grid in Newham
            Borough",
            title.color = "black")

#Verify distribution of AI in Newham
tmap_mode("view")
## tmap mode set to interactive viewing
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.5) +
  tm_shape(ptalNewham) +
  tm_dots(col = "AI2015",size = 0.05,shape = 22)

#-------------------------------
##Cleaning PTAL points inside each LSOAs
#Assigning lsoas by cleaned point
library(sf)
ptalLSOA <- st_intersection(ptal, lsoas)

ptalFiltered <- ptalLSOA[,c(1:4)]

#Classifying PTALS level per LSOAS
ptalFiltered_group <- ptalFiltered %>%
  group_by(LSOA11CD,PTAL2015)%>%
  summarise(count=n())

#Displaying the data in a wider format easy to process
ptalFiltered_wider <- ptalFiltered_group%>%
  st_set_geometry(.,NULL)%>%
  pivot_wider(names_from = PTAL2015,values_from=count)

#Rename columns names
ptalFiltered_wider <- ptalFiltered_wider%>%
  clean_names()

ptalFiltered_wider <- ptalFiltered_wider%>%
  dplyr::rename(c('quadrants_0'=x0,'quadrants_1a'=x1a,
  'quadrants_1b'=x1b,'quadrants_2'=x2,'quadrants_3'=x3,
  'quadrants_4'=x4,'quadrants_5'=x5,'quadrants_6a'=x6a,
  'quadrants_6b'=x6b))
ptalFiltered_wider <- ptalFiltered_wider%>%  
  dplyr::select(lsoa11cd,quadrants_0,quadrants_1a,quadrants_1b,
  quadrants_2,quadrants_3,quadrants_4,quadrants_5,
  quadrants_6a,quadrants_6b)

#Replace NA values where theres no quadrants with that features
ptalFiltered_wider[is.na(ptalFiltered_wider)] = 0

#Add sum of interest
ptalFiltered_wider <- ptalFiltered_wider %>%
   mutate(Total_quadrants=quadrants_0+quadrants_1a+
            quadrants_1b+quadrants_2+quadrants_3+
            quadrants_4+quadrants_5+quadrants_6a+
            quadrants_6b)
#MAtrix of composition of PTAL by LSOAS
ptal_comp <- ptalFiltered_wider %>%
  mutate(perc_0=quadrants_0/Total_quadrants*100)%>%
  mutate(perc_1a=quadrants_1a/Total_quadrants*100)%>%
  mutate(perc_1b=quadrants_1b/Total_quadrants*100)%>%
  mutate(perc_2=quadrants_2/Total_quadrants*100)%>%
  mutate(perc_3=quadrants_3/Total_quadrants*100)%>%
  mutate(perc_4=quadrants_4/Total_quadrants*100)%>%
  mutate(perc_5=quadrants_5/Total_quadrants*100)%>%
  mutate(perc_6a=quadrants_6a/Total_quadrants*100)%>%
  mutate(perc_6b=quadrants_6b/Total_quadrants*100)%>%
  dplyr::select(c(1,12:20))

#Add variable of highest coposition
ptal_comp <- ptal_comp%>% 
  mutate(highest_perc = max(perc_0,perc_1a,perc_1b,
                            perc_2,perc_3,perc_4,
                            perc_5,perc_6a,perc_6b))
#Distribution of predominant percentage  
binsize <- diff(range(ptal_comp$highest_perc))/10
ggplot(ptal_comp, aes(x =highest_perc))  +
  geom_histogram(binwidth=binsize,fill="white",colour="black")

#Distribution of LSOAS with highest accesibility
binsize <- diff(range(ptal_comp$perc_6a))/10
ggplot(ptal_comp, aes(x =perc_6a))  +
  geom_histogram(binwidth=binsize,fill="white",colour="black")

binsize <- diff(range(ptal_comp$perc_6b))/10
ggplot(ptal_comp, aes(x =perc_6b))  +
  geom_histogram(binwidth=binsize,fill="white",colour="black")

#Predominant PTAL per lsoa
#Assign column number of the maximun
col_number <- max.col(ptal_comp[,c(2:10)],ties.method = "first")
ptal_comp <- data.frame(ptal_comp,col_number)
#Match PTAL level with the column number
#Define matrix
col_match1 <- c(1:9)
col_match2 <- c("0","1a","1b","2","3","4","5","6a","6b")
match <- data.frame(col_match1,col_match2)
#Match both dataframes
ptal_comp <- ptal_comp%>%
  left_join(.,match,by=c("col_number"="col_match1"))
#Rename column names and delete match column
ptal_comp <- ptal_comp%>%
  dplyr::rename(predominant=`col_match2`)
  
#Merge LSOAs shape with PTAL table per LSOAs
lsoasMap <- lsoas%>%
  merge(.,
        ptal_comp, 
        by.x="LSOA11CD", 
        by.y="lsoa11cd",
        no.dups = TRUE)
#-----------------------
#Verify composition of accessibility in Newham
lsoasMapNewham <- lsoasMap %>%
  filter(., LAD11NM=="Newham")

tmap_mode("view")
## tmap mode set to interactive viewing
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(lsoasMapNewham) +
  tm_polygons(col = "predominant")
#Analysing most accessibles LSOAS of Newham
#Between 5-6 PTAL
lsoas_accessibles56 <- lsoasMapNewham%>%
  filter(str_detect(`predominant`,"^6") | str_detect(`predominant`,"5"))
# tmap mode set to interactive viewing
  tm_shape(lsoasMapNewham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.05) +
  tm_shape(lsoas_accessibles56) +
  tm_polygons(col = "highest_perc",palette="GnBu",
              title = "Predominance", # title for legend,
              labels = c("20% to 40%", "40% to 60%","60% to 80%","80% to 100%"))+
  tm_compass(north=0,type="rose")+
  tm_scale_bar(position=c("right", "top"))+
  tm_layout(title = "Level of predominance in LSOAs with 
            high accessibility in Newham",
            title.color = "black")+
  tm_text(text="predominant")
 


#Between 5-6 PTAL with predominance upper than 50%
lsoas_accessibles <- lsoasMapNewham%>%
  filter(`highest_perc`>50)%>%
  filter(str_detect(`predominant`,"^6") | str_detect(`predominant`,"5"))
#Percentages  
tmap_mode("view")
tm_shape(lsoasMapNewham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.05) +
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "LAD11NM",palette="Blues",
              title = "LSOAs with more than 50% 
              with high accessibility",
              labels =" ")+
  tm_layout(title = "Defined area",
            title.color = "black")


#-------------INIT   IMD2015-----------------

##Loading and cleaning IMD data
#Load csv files Deprivation indexes
imd2015 <- read_csv(here::here("Data","ID_London_2015.csv"),
                    locale = locale(encoding = "latin1"),
                    na = "n/a")
#Examine the file, variables, columns and cleaning
Datatypelist2 <- imd2015 %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist2

imd2015%>%
  colnames()

imd2015<-imd2015 %>%
  distinct()
#Select columns of interest (Just Scores)
imd2015<-imd2015 %>% 
  dplyr::select(c(1,4),contains("Score")) 
#Distribution of IMD score across London
ggplot(imd2015, aes(x =`IMD Score`))  +
  geom_histogram(fill="white",colour="black")

#Clip the data into Newham maps
imd2015Map <- lsoas%>%
  merge(.,
        imd2015, 
        by.x="LSOA11CD", 
        by.y="X1",
        no.dups = TRUE)

imd2015Map <- imd2015Map[c(1,2,5,6,15,16,18:25)]

#Standardisation of values of IMD to compare betweeen years
#Percentage differences from average of London
summary(imd2015Map)

#Averages IMD of London is 23.58

imd2015Map_stand <- imd2015Map %>% 
  mutate(perc_diff= `IMD Score`/23.58)%>%
  #new column to assess the categories in comparison with London
  mutate(Londoncompare=case_when(perc_diff>1 ~ "above London average",
                                 TRUE ~ "below London average"))%>%
  mutate(perc_diff_income= `Income Score (rate)`/0.1656)%>%
  mutate(Londoncompare_income=case_when(perc_diff_income>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_employ= `Employment Score (rate)`/0.1121)%>%
  mutate(Londoncompare_employ=case_when(perc_diff_employ>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_educ= `Education, Skills and Training Score`/14.09)%>%
  mutate(Londoncompare_educ=case_when(perc_diff_educ>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_health= `Health Deprivation and Disability Score`/-0.1988)%>%
  mutate(Londoncompare_health=case_when(perc_diff_health>1 ~ "above London average",
                                      TRUE ~ "below London average"))%>%
  mutate(perc_diff_crime= `Crime Score`/0.5465)%>%
  mutate(Londoncompare_crime=case_when(perc_diff_crime>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_housing= `Barriers to Housing and Services Score`/29.63)%>%
  mutate(Londoncompare_housing=case_when(perc_diff_housing>1 ~ "above London average",
                                       TRUE ~ "below London average"))%>%
  mutate(perc_diff_environ= `Living Environment Score`/30.51)%>%
  mutate(Londoncompare_environ=case_when(perc_diff_environ>1 ~ "above London average",
                                       TRUE ~ "below London average"))
#Ranking values to compare changes

imd2015Map_stand <- imd2015Map_stand %>% 
  mutate(ranking = dense_rank(desc(`IMD Score`)))%>% 
  mutate(ranking_income = dense_rank(desc(`Income Score (rate)`)))%>% 
  mutate(ranking_employ = dense_rank(desc(`Employment Score (rate)`)))%>%
  mutate(ranking_educ = dense_rank(desc(`Education, Skills and Training Score`)))%>% 
  mutate(ranking_health = dense_rank(desc(`Health Deprivation and Disability Score`)))%>%
  mutate(ranking_crime = dense_rank(desc(`Crime Score`)))%>%
  mutate(ranking_housing = dense_rank(desc(`Barriers to Housing and Services Score`)))%>%
  mutate(ranking_environ = dense_rank(desc(`Living Environment Score`)))


#-----------------------
#View IMD scores in Newham
imd2015MapNewham <- imd2015Map_stand %>%
  filter(., LAD11NM=="Newham")

tmap_mode("view")
## tmap mode set to interactive viewing
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015MapNewham) +
  tm_polygons(col = "IMD Score",palette="PuBu")
#-----
##View IMD scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above <- imd2015MapNewham %>%
  filter(`Londoncompare`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above) +
  tm_polygons(col = "perc_diff")


#filter the LSOAS that are below the London average  
imd2015Below <- imd2015MapNewham %>%
  filter(`Londoncompare`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below) +
  tm_polygons(col = "perc_diff")
#-----
##View IMD income scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_income <- imd2015MapNewham %>%
  filter(`Londoncompare_income`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_income) +
  tm_polygons(col = "perc_diff_income")


#filter the LSOAS that are below the London average  
imd2015Below_income <- imd2015MapNewham %>%
  filter(`Londoncompare_income`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_income) +
  tm_polygons(col = "perc_diff_income")

#-----
##View IMD employment scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_employ <- imd2015MapNewham %>%
  filter(`Londoncompare_employ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_employ) +
  tm_polygons(col = "perc_diff_employ")


#filter the LSOAS that are below the London average  
imd2015Below_employ <- imd2015MapNewham %>%
  filter(`Londoncompare_employ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_employ) +
  tm_polygons(col = "perc_diff_employ")
#-----
##View IMD education scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_educ <- imd2015MapNewham %>%
  filter(`Londoncompare_educ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_educ) +
  tm_polygons(col = "perc_diff_educ")


#filter the LSOAS that are below the London average  
imd2015Below_educ <- imd2015MapNewham %>%
  filter(`Londoncompare_educ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_educ) +
  tm_polygons(col = "perc_diff_educ")

#-----
##View IMD health scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_health <- imd2015MapNewham %>%
  filter(`Londoncompare_health`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_health) +
  tm_polygons(col = "perc_diff_health")


#filter the LSOAS that are below the London average  
imd2015Below_health <- imd2015MapNewham %>%
  filter(`Londoncompare_health`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_health) +
  tm_polygons(col = "perc_diff_health")

#-----
##View IMD crime scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_crime <- imd2015MapNewham %>%
  filter(`Londoncompare_crime`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_crime) +
  tm_polygons(col = "perc_diff_crime")


#filter the LSOAS that are below the London average  
imd2015Below_crime <- imd2015MapNewham %>%
  filter(`Londoncompare_crime`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_crime) +
  tm_polygons(col = "perc_diff_crime")

#-----
##View IMD housing scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_housing <- imd2015MapNewham %>%
  filter(`Londoncompare_housing`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_housing) +
  tm_polygons(col = "perc_diff_housing")


#filter the LSOAS that are below the London average  
imd2015Below_housing <- imd2015MapNewham %>%
  filter(`Londoncompare_housing`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_housing) +
  tm_polygons(col = "perc_diff_housing")
#-----
##View IMD environment scores compared with London average

#filter the LSOAS that are above the London average
imd2015Above_environ <- imd2015MapNewham %>%
  filter(`Londoncompare_environ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Above_environ) +
  tm_polygons(col = "perc_diff_environ")


#filter the LSOAS that are below the London average  
imd2015Below_environ <- imd2015MapNewham %>%
  filter(`Londoncompare_environ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2015Below_environ) +
  tm_polygons(col = "perc_diff_environ")


#-------------END   IMD2015-----------------

#-------------INIT  IMD2019-----------------

##Loading and cleaning IMD data
#Load csv files Deprivation indexes
imd2019 <- read_csv(here::here("Data","ID_London_2019.csv"),
                    locale = locale(encoding = "latin1"),
                    na = "n/a")
#Examine the file, variables, columns and cleaning
Datatypelist3 <- imd2019 %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist3

imd2019%>%
  colnames()

imd2019<-imd2019 %>%
  distinct()
#Select columns of interest (Just Scores)
imd2019<-imd2019 %>% 
  dplyr::select(c(1,4),contains("Score")) 
#Distribution of IMD score across London
ggplot(imd2019, aes(x =`IMD Score`))  +
  geom_histogram(fill="white",colour="black")

#Clip the data into Newham maps
imd2019Map <- lsoas%>%
  merge(.,
        imd2019, 
        by.x="LSOA11CD", 
        by.y="LSOA code (2011)",
        no.dups = TRUE)

imd2019Map <- imd2019Map[c(1,2,5,6,15,16,18:25)]
#-----------------------
#Standardisation of values of IMD rask to compare betweeen years
#Percentage differences from average of London
summary(imd2019Map)

#Averages IMD of London is 23.58

imd2019Map_stand <- imd2019Map %>% 
  mutate(perc_diff= `IMD Score`/21.50)%>%
  #new column to assess the categories in comparison with London
  mutate(Londoncompare=case_when(perc_diff>1 ~ "above London average",
                                 TRUE ~ "below London average"))%>%
  mutate(perc_diff_income= `Income Score (rate)`/0.1357)%>%
  mutate(Londoncompare_income=case_when(perc_diff_income>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_employ= `Employment Score (rate)`/0.08813)%>%
  mutate(Londoncompare_employ=case_when(perc_diff_employ>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_educ= `Education, Skills and Training Score`/13.07)%>%
  mutate(Londoncompare_educ=case_when(perc_diff_educ>1 ~ "above London average",
                                      TRUE ~ "below London average"))%>%
  mutate(perc_diff_health= `Health Deprivation and Disability Score`/-0.3837)%>%
  mutate(Londoncompare_health=case_when(perc_diff_health>1 ~ "above London average",
                                        TRUE ~ "below London average"))%>%
  mutate(perc_diff_crime= `Crime Score`/0.2580)%>%
  mutate(Londoncompare_crime=case_when(perc_diff_crime>1 ~ "above London average",
                                       TRUE ~ "below London average"))%>%
  mutate(perc_diff_housing= `Barriers to Housing and Services Score`/31.62)%>%
  mutate(Londoncompare_housing=case_when(perc_diff_housing>1 ~ "above London average",
                                         TRUE ~ "below London average"))%>%
  mutate(perc_diff_environ= `Living Environment Score`/29.21)%>%
  mutate(Londoncompare_environ=case_when(perc_diff_environ>1 ~ "above London average",
                                         TRUE ~ "below London average"))
#Ranking values to compare changes

imd2019Map_stand <- imd2019Map_stand %>% 
  mutate(ranking = dense_rank(desc(`IMD Score`)))%>% 
  mutate(ranking_income = dense_rank(desc(`Income Score (rate)`)))%>% 
  mutate(ranking_employ = dense_rank(desc(`Employment Score (rate)`)))%>%
  mutate(ranking_educ = dense_rank(desc(`Education, Skills and Training Score`)))%>% 
  mutate(ranking_health = dense_rank(desc(`Health Deprivation and Disability Score`)))%>%
  mutate(ranking_crime = dense_rank(desc(`Crime Score`)))%>%
  mutate(ranking_housing = dense_rank(desc(`Barriers to Housing and Services Score`)))%>%
  mutate(ranking_environ = dense_rank(desc(`Living Environment Score`)))

#-----------------------
#View IMD scores in Newham
imd2019MapNewham <- imd2019Map_stand %>%
  filter(., LAD11NM=="Newham")

tmap_mode("view")
## tmap mode set to interactive viewing
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019MapNewham) +
  tm_polygons(col = "IMD Score",palette="PuBu")
#-----
##View IMD scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above <- imd2019MapNewham %>%
  filter(`Londoncompare`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above) +
  tm_polygons(col = "perc_diff")


#filter the LSOAS that are below the London average  
imd2019Below <- imd2019MapNewham %>%
  filter(`Londoncompare`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below) +
  tm_polygons(col = "perc_diff")
#-----
##View IMD income scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_income <- imd2019MapNewham %>%
  filter(`Londoncompare_income`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_income) +
  tm_polygons(col = "perc_diff_income")


#filter the LSOAS that are below the London average  
imd2019Below_income <- imd2019MapNewham %>%
  filter(`Londoncompare_income`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_income) +
  tm_polygons(col = "perc_diff_income")

#-----
##View IMD employment scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_employ <- imd2019MapNewham %>%
  filter(`Londoncompare_employ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_employ) +
  tm_polygons(col = "perc_diff_employ")


#filter the LSOAS that are below the London average  
imd2019Below_employ <- imd2019MapNewham %>%
  filter(`Londoncompare_employ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_employ) +
  tm_polygons(col = "perc_diff_employ")
#-----
##View IMD education scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_educ <- imd2019MapNewham %>%
  filter(`Londoncompare_educ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_educ) +
  tm_polygons(col = "perc_diff_educ")


#filter the LSOAS that are below the London average  
imd2019Below_educ <- imd2019MapNewham %>%
  filter(`Londoncompare_educ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_educ) +
  tm_polygons(col = "perc_diff_educ")

#-----
##View IMD health scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_health <- imd2019MapNewham %>%
  filter(`Londoncompare_health`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_health) +
  tm_polygons(col = "perc_diff_health")


#filter the LSOAS that are below the London average  
imd2019Below_health <- imd2019MapNewham %>%
  filter(`Londoncompare_health`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_health) +
  tm_polygons(col = "perc_diff_health")

#-----
##View IMD crime scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_crime <- imd2019MapNewham %>%
  filter(`Londoncompare_crime`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_crime) +
  tm_polygons(col = "perc_diff_crime")


#filter the LSOAS that are below the London average  
imd2019Below_crime <- imd2019MapNewham %>%
  filter(`Londoncompare_crime`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_crime) +
  tm_polygons(col = "perc_diff_crime")

#-----
##View IMD housing scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_housing <- imd2019MapNewham %>%
  filter(`Londoncompare_housing`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_housing) +
  tm_polygons(col = "perc_diff_housing")


#filter the LSOAS that are below the London average  
imd2019Below_housing <- imd2019MapNewham %>%
  filter(`Londoncompare_housing`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_housing) +
  tm_polygons(col = "perc_diff_housing")
#-----
##View IMD environment scores compared with London average

#filter the LSOAS that are above the London average
imd2019Above_environ <- imd2019MapNewham %>%
  filter(`Londoncompare_environ`=="above London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Above_environ) +
  tm_polygons(col = "perc_diff_environ")


#filter the LSOAS that are below the London average  
imd2019Below_environ <- imd2019MapNewham %>%
  filter(`Londoncompare_environ`=="below London average")
#Plot the variations
tm_shape(Newham) +
  tm_polygons(fill=NA,col = "black", alpha = 0.25) +
  tm_shape(imd2019Below_environ) +
  tm_polygons(col = "perc_diff_environ")

#-------------END   IMD2019-----------------

#-------------INIT  COMPARISON 2015-2019-----------------

#Assess changes in rankings per LSOAs between 2015 and 2019
#Change to a dataframe
imd2015MapNewham <- imd2015MapNewham%>%
  st_set_geometry(.,NULL)

rankings_2015_2019 <- imd2019MapNewham %>%
  merge(.,
        imd2015MapNewham, 
        by.x="LSOA11CD", 
        by.y="LSOA11CD",
        no.dups = TRUE)
#Select columns of interest
rankings_2015_2019%>%
  colnames()
rankings_2015_2019 <- rankings_2015_2019%>%
  dplyr::select(c(1:14,31:38,44:51,68:76))
#Calculate variation in rankings between 2015 and 2019 and population
rankings_2015_2019 <- rankings_2015_2019%>%
  mutate(diff_ranking=ranking.x-ranking.y)%>%
  mutate(compare_ranking=case_when(diff_ranking>0 ~ "Improved its position",
                                   diff_ranking<0 ~ "Worsened its position",
                                   diff_ranking==0 ~ "The same position"))%>%
  mutate(diff_ranking_income=ranking_income.x-ranking_income.y)%>%
  mutate(compare_ranking_income=case_when(diff_ranking_income>0 ~ "Improved its position",
                                   diff_ranking_income<0 ~ "Worsened its position",
                                   diff_ranking_income==0 ~ "The same position"))%>% 
  mutate(diff_ranking_employ=ranking_employ.x-ranking_employ.y)%>%
  mutate(compare_ranking_employ=case_when(diff_ranking_employ>0 ~ "Improved its position",
                                        diff_ranking_employ<0 ~ "Worsened its position",
                                        diff_ranking_employ==0 ~ "The same position"))%>%
  mutate(diff_ranking_educ=ranking_educ.x-ranking_educ.y)%>%
  mutate(compare_ranking_educ=case_when(diff_ranking_educ>0 ~ "Improved its position",
                                   diff_ranking_educ<0 ~ "Worsened its position",
                                   diff_ranking_educ==0 ~ "The same position"))%>%
  mutate(diff_ranking_health=ranking_health.x-ranking_health.y)%>%
  mutate(compare_ranking_health=case_when(diff_ranking_health>0 ~ "Improved its position",
                                        diff_ranking_health<0 ~ "Worsened its position",
                                        diff_ranking_health==0 ~ "The same position"))%>%
  mutate(diff_ranking_crime=ranking_crime.x-ranking_crime.y)%>%
  mutate(compare_ranking_crime=case_when(diff_ranking_crime>0 ~ "Improved its position",
                                          diff_ranking_crime<0 ~ "Worsened its position",
                                          diff_ranking_crime==0 ~ "The same position"))%>%
  mutate(diff_ranking_housing=ranking_housing.x-ranking_housing.y)%>%
  mutate(compare_ranking_housing=case_when(diff_ranking_housing>0 ~ "Improved its position",
                                         diff_ranking_housing<0 ~ "Worsened its position",
                                         diff_ranking_housing==0 ~ "The same position"))%>%
  mutate(diff_ranking_environ=ranking_environ.x-ranking_environ.y)%>%
  mutate(compare_ranking_environ=case_when(diff_ranking_environ>0 ~ "Improved its position",
                                           diff_ranking_environ<0 ~ "Worsened its position",
                                           diff_ranking_environ==0 ~ "The same position"))

  
#Convert into absolut values to use them as a buble size
rankings_2015_2019 <- rankings_2015_2019 %>%
  mutate(diff_ranking_a = abs(diff_ranking)) %>%
  mutate(diff_ranking_income_a = abs(diff_ranking_income)) %>%
  mutate(diff_ranking_employ_a = abs(diff_ranking_employ)) %>%
  mutate(diff_ranking_educ_a = abs(diff_ranking_educ)) %>%
  mutate(diff_ranking_health_a = abs(diff_ranking_health)) %>%
  mutate(diff_ranking_crime_a = abs(diff_ranking_crime)) %>%
  mutate(diff_ranking_housing_a = abs(diff_ranking_housing)) %>%
  mutate(diff_ranking_environ_a = abs(diff_ranking_environ))

prueba <- rankings_2015_2019

prueba$diff_ranking_a[prueba$diff_ranking_a == 0] <- 0.0001
prueba$diff_ranking_income_a[prueba$diff_ranking_income_a == 0] <- 0.0001
prueba$diff_ranking_educ_a[prueba$diff_ranking_educ_a == 0] <- 0.0001
prueba$diff_ranking_health_a[prueba$diff_ranking_health_a == 0] <- 0.0001
prueba$diff_ranking_environ_a[prueba$diff_ranking_environ_a == 0] <- 0.0001
prueba$diff_ranking_crime_a[prueba$diff_ranking_crime_a == 0] <- 0.0001
prueba$diff_ranking_housing_a[prueba$diff_ranking_housing_a == 0] <- 0.0001
prueba$diff_ranking_employ_a[prueba$diff_ranking_employ_a == 0] <- 0.0001

#Create tags for the popups
prueba <- prueba%>%
  mutate(popup1=str_c(compare_ranking," in ",diff_ranking_a," ranks"))

prueba <- prueba%>%
  mutate(popup2=str_c(compare_ranking_income," in ",diff_ranking_income_a," ranks"))%>%
  mutate(popup3=str_c(compare_ranking_employ," in ",diff_ranking_employ_a," ranks"))%>%
  mutate(popup4=str_c(compare_ranking_educ," in ",diff_ranking_educ_a," ranks"))%>%
  mutate(popup5=str_c(compare_ranking_health," in ",diff_ranking_health_a," ranks"))%>%
  mutate(popup6=str_c(compare_ranking_crime," in ",diff_ranking_crime_a," ranks"))%>%
  mutate(popup7=str_c(compare_ranking_housing," in ",diff_ranking_housing_a," ranks"))%>%
  mutate(popup8=str_c(compare_ranking_environ," in ",diff_ranking_environ_a," ranks"))

#Rename column

#Leaflet maps

#Basemaps
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"

#----IMD Rankings
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_a", 
             col = "compare_ranking", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale = 2.5,
             style = "fixed",
             palette = c("aquamarine3","coral2",  "gray"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking.y",
                            "Ranking 2019:" = "ranking.x","Change: " = "popup1"))+
  tm_layout(title = "Changes in Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Income
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_income_a", 
             col = "compare_ranking_income", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =3,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_income.y",
                            "Ranking 2019:" = "ranking_income.x","Change: " = "popup2"))+
  tm_layout(title = "Changes in Income Deprivation Rankings by LSOAs in Newham",
            title.color = "black")
#----IMD Education
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_educ_a", 
             col = "compare_ranking_educ", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =4,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_educ.y",
                            "Ranking 2019:" = "ranking_educ.x","Change: " = "popup4"))+
  tm_layout(title = "Changes in Education Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Education
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_educ_a", 
             col = "compare_ranking_educ", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =4,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_educ.y",
                            "Ranking 2019:" = "ranking_educ.x","Change: " = "popup4"))+
  tm_layout(title = "Changes in Education Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Health
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_health_a", 
             col = "compare_ranking_health", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =3,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_health.y",
                            "Ranking 2019:" = "ranking_health.x","Change: " = "popup5"))+
  tm_layout(title = "Changes in Health Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Environ
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_environ_a", 
             col = "compare_ranking_environ", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =3,
             style = "fixed",
             palette = c("aquamarine3","coral2", "gray"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_environ.y",
                            "Ranking 2019:" = "ranking_environ.x","Change: " = "popup8"))+
  tm_layout(title = "Changes in Environment Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Crime
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_crime_a", 
             col = "compare_ranking_crime", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =2.5,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_crime.y",
                            "Ranking 2019:" = "ranking_crime.x","Change: " = "popup6"))+
  tm_layout(title = "Changes in Crime Deprivation Rankings by LSOAs in Newham",
            title.color = "black")

#----IMD Housing
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_housing_a", 
             col = "compare_ranking_housing", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =2.5,
             style = "fixed",
             palette = c("aquamarine3", "gray","coral2"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_housing.y",
                            "Ranking 2019:" = "ranking_housing.x","Change: " = "popup7"))+
  tm_layout(title = "Changes in Housing Deprivation Rankings by LSOAs in Newham",
            title.color = "black")
#----IMD Employment
tm_basemap(carto) + 
  tm_shape(lsoas_accessibles) +
  tm_polygons(col="LAD11CD",palette = "red",alpha=0.25)+
  tm_shape(prueba) +
  tm_borders(col = "black",alpha=0.5) +
  tm_bubbles("diff_ranking_employ_a", 
             col = "compare_ranking_employ", 
             title.col="2015 - 2019",
             border.col = "white", 
             scale =2,
             style = "fixed",
             palette = c("gray", "coral2","aquamarine3"),
             popup.vars = c("LSOA:" = "LSOA11NM.x","Ranking 2015:" = "ranking_employ.y",
                            "Ranking 2019:" = "ranking_employ.x","Change: " = "popup3"))+
  tm_layout(title = "Changes in Employment Deprivation Rankings by LSOAs in Newham",
            title.color = "black")
                            
