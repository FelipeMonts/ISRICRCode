##############################################################################################################
# 
# 
# Felipe Montes
# Trial to get soils from Kyrgystan From the Isric webpage
# 2019/01/23
# 
# Links to package Raster https://cran.r-project.org/web/packages/raster/index.html
#                          https://cran.r-project.org/web/packages/raster/raster.pdf
#
#
# other web resources that would help \:
#  https://stanford.edu/~mgorkove/cgi-bin/rpython_tutorials/Scraping_a_Webpage_Rendered_by_Javascript_Using_Python.php
# 
#   https://datascienceplus.com/scraping-javascript-rendered-web-content-using-r/
# 
##############################################################################################################
# 
#  
#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

setwd('C:\\Felipe\\PIHM-CYCLES\\PIHM\\Soils\\ISRICWorldSoils') ;


# install.packages('bitops',dependencies = T);
# install.packages('RCurl',dependencies = T);
# install.packages('rgdal');
# install.packages('GSIF');
# install.packages('raster');
# install.packages('plotKML');
# install.packages('XML');
# install.packages('lattice');
# install.packages('aqp');
# install.packages('soiltexturep');
# install.packages('stringi') ;
# install.packages('rvest');
# install.packages('V8');

# SoilGrids tutorial
# Reference: [http://gsif.isric.org/doku.php?id=wiki:tutorial_soilgrids]
# Tom.Hengl@isric.org

library(RCurl)
library(rgdal)
library(GSIF)
library(raster)
library(plotKML)
library(XML)
library(lattice)
library(aqp)
library(soiltexture)
library(stringi)

library(openxlsx) ;

library(rvest) ;
library(V8) ;


##### Scrap the html source of the website

soilGrids.url<-'https://soilgrids.org/#!/?layer=ORCDRC_M_sl2_250m&vector=1' ;

soilGrids.web<-read_html(soilGrids.url)

soilGrids.web %>% html_nodes('div') %>% html_nodes('.content')

html_nodes(soilGrids.web, css = '.content')


str(soilGrids.web)


############## REad the polygon selected in Google earth

SelectedSoilArea1<-readOGR("At_Bashy.shp") ;
SelectedSoilArea2<-readOGR("At_Bashy.kml") ;
SelectedSoilArea3<-readOGR("At_Bashy2.kml");
SelectedSoilArea4<-readOGR("At_Bashy3.kml");

plot(SelectedSoilArea2);
plot(SelectedSoilArea1,add=T);
plot(SelectedSoilArea3,add=T);
plot(SelectedSoilArea4,add=T);




###### get information from the ISRIC Soil Raster data


###### Read the metadata describing the raster files information

ISRIC.meta<-read.csv('META_GEOTIFF_1B.csv',header = T)[c(1,4:31,41:89,209),1:15] ;
str(ISRIC.meta)
head(ISRIC.meta)
tail(ISRIC.meta)
ISRIC.meta[,1]

######## Variables reported in raster layers for depths  #####
 
#List of variables  

#ISRIC.variables<-unique(stri_split_fixed(ISRIC.meta[c(4:89),1],"_",simplify = T)[,1]) ;
ISRIC.variables<-unique(stri_split_fixed(ISRIC.meta[,1],"_",simplify = T)[,1]) ;

#Variable description

#ISRIC.var.desc<-unique((ISRIC.meta[c(5:89),3])) ;

ISRIC.var.desc<-unique((ISRIC.meta[,3])) ;

ISRIC.Parameters<-data.frame(ISRIC.variables,ISRIC.var.desc);



########################################################################################################## 
# 
#          the raster for variable [1]  BDRICM Depth to bedrock (R horizon) up to 200 cm does not work. 
# 
# 
###########################################################################################################  




#Soil layer levels 

ISRIC.layers<-ISRIC.meta[c(2:8),c( 'ATTRIBUTE_LABEL' , 'DEPTH', 'HORIZON_UPPER_DEPTH', 'HORIZON_LOWER_DEPTH')];


ISRIC.layers$HORIZON_LABEL<-stri_split_fixed(ISRIC.layers$ATTRIBUTE_LABEL,"_",simplify = T)[,3] ;

# ISRIC.layers$HORIZON_LABEL[1]<-"Depth.BR"

ISRIC.layers$DEPTH.m<-stri_split_fixed(ISRIC.layers$DEPTH," ",simplify = T)[,1] ;

ISRIC.layers$HORIZON_UPPER_DEPTH.m<-stri_split_fixed(ISRIC.layers$HORIZON_UPPER_DEPTH," ",simplify = T)[,1] ;

ISRIC.layers$HORIZON_LOWER_DEPTH.m<-stri_split_fixed(ISRIC.layers$HORIZON_LOWER_DEPTH," ",simplify = T)[,1] ;



########################################################################################################## 
#
#  HORIZON_UPPER_DEPTH.m and HORIZON_LOWER_DEPTH.m have the same values in the metadata  META_GEOTIFF_1B.csv database. 
#  It is not clear whybut that is the way it is. Inorder with work with properly defined soil horizons there needs to be
#  a difference between the upper and lower horizon depth, a thickness. This is corrrected in the code below
# 
# 
###########################################################################################################  

ISRIC.layers$Thickness.m<-c(0.05,0.1,0.15,0.3,0.4,1.0,0.05) ;

ISRIC.layers$Low_Depth.m<-as.numeric(ISRIC.layers$HORIZON_UPPER_DEPTH.m)+ISRIC.layers$Thickness.m 




#Names of the raster files to read

names.1<-stri_split_fixed(ISRIC.meta[1:77,1],"_",simplify = T,n=4)[,c(1,2,3)] ;
ISRIC.files<-paste(names.1[,1],names.1[,2],names.1[,3], "1km_Kyrgyzstan.tiff",sep = "_") ;

ISRIC.files[1]<-paste(names.1[1,1],names.1[1,2], "1km_Kyrgyzstan.tiff",sep = "_")





#### Get the raster files available and create  a raster stack of the raster layers for each variable ######

list.files()

KyrgyszSoils<-raster("TAXOUSDA_1km_Kyrgyzstan.tiff") ;
hasValues(KyrgyszSoils)
inMemory(KyrgyszSoils)

plot(KyrgyszSoils) 
plot(SelectedSoilArea2,add=T)
plot(SelectedSoilArea1,add=T)


# BDRICM_M_250m_ll.tif Depth to bedrock (R horizon) up to 200 cm

BDRICM.ras<-stack(ISRIC.files[1]) 



#    BLDFIE       Bulk density (fine earth, oven dry) in kg / cubic-meter

#cerate a raster stack/brick with the soil layers raster files that include the spatial Bulk densityinformation

BLDFIE.ras<-stack(ISRIC.files[2:8]) ;

nlayers(BLDFIE.ras)

BLDFIE.brik<-brick(BLDFIE.ras) ;

nlayers(BLDFIE.brik)

plot(BLDFIE.ras);

plot(BLDFIE.brik);

# crop the soil layers for Kirgystan by the selected area in gogle earth to reduce the size of the raster stack/brik

BLDFIE.brik.select<-crop(BLDFIE.brik,SelectedSoilArea4) ;

plot(BLDFIE.brik.select)


#explore the ssize of the seleted area raster stack/brick

ncol(BLDFIE.brik.select)
nrow(BLDFIE.brik.select)
ncell(BLDFIE.brik.select)

# get the bulk density values from the raster stack/brick of spatial bulk density information


Pedon.values<-getValues(BLDFIE.brik.select) ;

# get the coordiantes of the raster cells of the seletced area raster stack/brick

Pedon.coord<-xyFromCell(BLDFIE.brik.select,seq(1,ncell(BLDFIE.brik.select)))  ;


#  Transform the Pedon.info query in to the right format to be converted into a SoilProfileCollection object
#   https://ncss-tech.github.io/AQP/aqp/aqp-intro.html
#Pedon.info$id<-Pedon.info$mukey ;
# Pedon.info$top<-Pedon.info$hzdept_r ;
# Pedon.info$bottom<-Pedon.info$hzdept_r ;
#Pedon.info$name<-Pedon.info$hzname ;

## init SoilProfileCollection objects from data.frame
# depths(sp1) <- id ~ top + bottom


# SoilProfileCollection objects are typically created by "promoting" data.frame objects (rectangular tables of data) that contain at least three essential columns:
#   
#   an ID column uniquely identifying groups of horizons (e.g. pedons)
# horizon top boundaries
# horizon bottom boundaries
# 
# The data.frame should be pre-sorted according to the profile ID and horizon top boundary. Formula notation is used to define the columns used to promote a data.frame object:
# 
# 


head(ISRIC.layers,7)
str(ISRIC.layers)
str(values(BLDFIE.brik.select))

ISRIC.pedon<-data.frame(ISRIC.layers$HORIZON_LABEL , as.numeric((ISRIC.layers$DEPTH.m)), as.numeric(as.character(ISRIC.layers$HORIZON_UPPER_DEPTH.m)),ISRIC.layers$Low_Depth.m,ISRIC.layers$Thickness.m );

names(ISRIC.pedon)<-c('Horizon', 'Depth','top', 'bottom','thickness');

ISRIC.pedon$ID<-"ISRIC" ;


str(ISRIC.pedon)
print(ISRIC.pedon)

# Create the generic soil collection object

depths(ISRIC.pedon)<-ID ~ top + bottom  ;
str(ISRIC.pedon)
depth_units(ISRIC.pedon)<-'m'


ISRIC.pedon$BLDFIE<-t(Pedon.values)[,1]

plot(ISRIC.pedon, color='BLDFIE', name='BLDFIE')


siteNames(ISRIC.pedon)

horizons(ISRIC.pedon)

site(ISRIC.pedon)


# SoilProfileCollection object 






#    CECSOL                  Cation exchange capacity of soil in cmolc/kg
CECSOL.ras<-stack(ISRIC.files[8:14]) ;
plot(CECSOL.ras);


# CLYPPT             Clay content (0-2 micro meter) mass fraction in %

CLYPPT.ras<-stack(ISRIC.files[15:21]) ;
plot(CLYPPT.ras);


#  CRFVOL                              Coarse fragments volumetric in %

CRFVOL.ras<-stack(ISRIC.files[22:28]) ;
plot(CRFVOL.ras);



# # OCDENS                 Soil organic carbon density in kg per cubic-m
# 
# OCDENS.ras<-stack(ISRIC.files[29:35]) ;
# plot(OCDENS.ras);
# 
# 


#  ORCDRC Soil organic carbon content (fine earth fraction) in g per kg

ORCDRC.ras<-stack(ISRIC.files[36:42]) ;
plot(ORCDRC.ras);


# PHIHOX                                          Soil pH x 10 in H2O 

PHIHOX.ras<-stack(ISRIC.files[43:49]) ;
plot(PHIHOX.ras);



#PHIKCL                                           Soil pH x 10 in KCl

PHIKCL.ras<-stack(ISRIC.files[50:56]) ;
plot(PHIKCL.ras);



#SLTPPT            Silt content (2-50 micro meter) mass fraction in %

SLTPPT.ras<-stack(ISRIC.files[57:63]) ;
plot(SLTPPT.ras);


# SNDPPT         Sand content (50-2000 micro meter) mass fraction in %

SNDPPT.ras<-stack(ISRIC.files[64:70]) ;
plot(SNDPPT.ras);

# # TEXMHT                                   Texture class (USDA system)
# 
# TEXMHT.ras<-stack(ISRIC.files[71:77]) ;
# plot(TEXMHT.ras);
# 






plot(stack(ISRIC.files[1:5]))
plot(crop(stack(ISRIC.files[1:5]),SelectedSoilArea))
as.data.frame(crop(stack(ISRIC.files[1:5]),SelectedSoilArea),xy=T)

##### Create  3

######  Extract the Selected area from the ISRIC Soil Raster data

cropped<-crop(KyrgyszSoils,SelectedSoilArea); 
plot(cropped) 
image(cropped)

str(cropped)
##### Transform the raster cropped raster layer into data frame with coordinates 

cropped.dtf<-as.data.frame(cropped, xy=T)



# ##-----------------------------------
# ## Accessing data
# ##-----------------------------------
# 
# ## (a) FTP download:
# ## location of soilgrids:
# sg.ftp <- "ftp://ftp.soilgrids.org/data/recent/"
# filenames = getURL(sg.ftp, ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filenames = strsplit(filenames, "\r*\n")[[1]]
# filenames[1:5]
# 
# ## download to a local directory:
# ORC.name <- filenames[grep(filenames, pattern=glob2rx("ORCDRC_M_sl1_250m_ll.tif$"))]
# ORC.name
# try(download.file(paste(sg.ftp, ORC.name, sep=""), ORC.name))
# ## 2.8GB Geotiff!!
# 
# ## check that everything is OK:
# GDALinfo(ORC.name)
# 
# ## We focus on Ghana
# wg.url <- url("http://gsif.isric.org/lib/exe/fetch.php?media=admin.af.rda")
# load(wg.url)
# proj4string(admin.af) <- "+proj=longlat +datum=WGS84"
# country.af <- as(admin.af, "SpatialLines")
# ## Ghana bounding box:
# ghana <- admin.af[admin.af$FORMAL_EN=="Republic of Ghana",]
# ghana@bbox
# 
# ## load soil Africa Soil Profile DB:
# data(afsp)
# sites <- afsp$sites
# coordinates(sites) <- ~ LONWGS84 + LATWGS84
# proj4string(sites) <- "+proj=longlat +datum=WGS84"
# #af.csy = "+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +units=m +ellps=WGS84 +datum=WGS84"
# #sites.af <- spTransform(sites, CRS(af.csy))
# 
# ## plot country and profiles
# plot(ghana, col="red", lwd=2, asp=1)
# lines(country.af)
# points(sites, pch="+")
# ## in local projection system:
# #ghana.xy <- spTransform(ghana, CRS(af.csy))
# #ghana.xy@bbox
# 
# ## get only Ghana:
# te = as.vector(ghana@bbox)
# unlink("ORC_sl1_Ghana.tif")
# system(paste0(gdalwarp, ' ', ORC.name, ' ORC_sl1_Ghana.tif -te ', paste(te, collapse=" ")))
# ORCDRC_sl1_ghana <- readGDAL("ORC_sl1_Ghana.tif")
# plot(log1p(raster(ORCDRC_sl1_ghana)), col=SAGA_pal[[1]])


#### This Seems the most efficient way to extract the soils from Kyrgyzstan #######




## (b) Web Coverage Service
## location of service:
# wcs = "http://webservices.isric.org/geoserver/wcs?"
# ## create an XML file:
# l1 <- newXMLNode("WCS_GDAL")
# l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
# l1.l <- newXMLNode("CoverageName", "orcdrc_m_sl1_250m", parent=l1)
# l1
# xml.out = "ORCDRC_M_sl1.xml"
# saveXML(l1, file=xml.out)

# ## check if the layer exists:
# system(paste(gdalinfo, xml.out))

## Alternative: calculate offset and region dims:


###### Get the bounding box region for Kyrgyzstan and then get the raster files for this sections######

ContryPolygonInfo<-ogrInfo("C:/Felipe/PIHM-CYCLES/PIHM/Soils/ISRICWorldSoils/KyrgyzstanPolygonWGS84.shp") ;

#SoilGridDepthBRockInfo<-ogrInfo('https://files.isric.org/soilgrids/data/recent/ACDWRB_M_ss_250m_ll.tif');



Bounding.box<-ContryPolygonInfo$extent;


# extent(raster(ORC.name))
# bb <- matrix(nrow=2, c(-180,-62.00081,179.9999,87.37))
# o.x = 172800 + round(172800*(te[1]-bb[1,2])/(bb[1,2]-bb[1,1]))
# o.y = round(71698*(bb[2,2]-te[4])/(bb[2,2]-bb[2,1]))
# d.y = round(71698*(te[4]-te[2])/(bb[2,2]-bb[2,1]))
# d.x = round(172800*(te[3]-te[1])/(bb[1,2]-bb[1,1]))
# o.x; o.y; d.x; d.y


KyrgyzstanUSDA<-raster('TAXOUSDA_1km_Kyrgyzstan.tiff') ;

plot(KyrgyzstanUSDA)

#

## end of script;