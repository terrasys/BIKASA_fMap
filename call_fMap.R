#Basic Settings
#-----------------------------------------------------------------------------------------------------
W.DIR = "..."
FUNC.DIR = "_function/"

print("Import function")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackages.R"))
DOY <- c(91,131,166,193,208,243,274)#Days of the year
OUT.DIR = "_output/"#output directory
IN.DIR = "_input/"#input directory
R.L1 =  "DEM10_L-FB1.asc"#name of first length factor raster file
R.S1 =  "DEM10_S-FB1.asc"#name of first slope factor raster file
R.L2 =  "DEM10_L-FB4.asc"#name of second length factor raster file
R.S2 =  "DEM10_S-FB4.asc"#name of second slope factor raster file
R.K1 = "DEM10_K.asc"#name of first soil erodibility factor raster file
R.K2 = "DEM10_K.asc"#name of second soil erodibility factor raster file
F.L = 2#factor reducing slope length factor
F.S = 1.5#factor reducing slope factor
F.K = 1#factor reducingsoil erodibility factor
R.SHD = "DEM10_SHD.asc"#name of shaded relief file
SHP1 = "DESTLI0503850019.shp"#name of first boundary file
SHP2 = "DESTLI0503850019_b4.shp"#name of second boundary file
V.DYN = "RADOLANGT10MM_buffer5000_202.csv"#file containing phenological phases, corresponding soil coverages as well as heavy rain event indicator values for winter wheat 
V.DYN = "RADOLANGT10MM_buffer5000_215.csv"#file containing phenological phases, corresponding soil coverages as well as heavy rain event indicator values for winter rape seed

ALPHA=0.5#transparence factor for plotting maps with shaded relief
PREFIX = "LOE_"#prefix for output file
SUFFIX = "2"#suffix for output file
CROP=215#DWD crop type
H=4.48#height of output map
W=5.7#wight of output map

#plot field block-specific maps of potential soil erosion risk
source(file.path(W.DIR,FUNC.DIR,"fMap.R"))
fMap(W.DIR,
     IN.DIR,
     V.DYN,
     R.L1,
     R.S1,
     R.L2,
     R.S2,
     R.K1,
     R.K2,
     R.SHD,
     F.L,
     F.S,
     CROP,
     D.L,
     ALPHA,
     SHP1,
     SHP2,
     PREFIX,
     SUFFIX,
     H,
     W)

#plot field block-specific phenological windows and correspinding heavy rain indicator values 
source(file.path(W.DIR,FUNC.DIR,"fPhenoRain.R"))
W.DIR = "d:/Dropbox/_git/BIKASA_fMap/"
IN.DIR = "_input/DESTLI0503850019/"
CROP=215
V.DYN = "RADOLANGT10MM_buffer5000_215.csv"
pdf(paste(W.DIR,OUT.DIR,"LOE_215.pdf",sep=""), 
    height=4.8,width=10)
fPhenoRain(W.DIR,
           IN.DIR,
           V.DYN,
           CROP,
           DOY)
dev.off()


