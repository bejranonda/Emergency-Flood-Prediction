#install.packages(c("rgdal","maptools","gstat","spdep","spatstat","RSAGA","spgrass6"))
rm(list=ls(all=TRUE))
library(sp)
library(maptools)
library(gstat)
#library(RSAGA)
library(lattice) # required for trellis.par.set():
trellis.par.set(sp.theme()) # sets color ramp to bpy.colors()


#Background
#background.mp <- readShapePoly("amphoe_gisdata")
#background.mp <- spTransform(amphoe_gisdata.shp)
bg.f = "../../data/gis/base_layers/amphoe_gisdata.shp"
bg.shp <- readShapePoly(bg.f)
#bg.list = list("sp.polygons", bg.shp,fill = ifelse(alphaChannelSupported(), "blue", "transparent"),alpha = ifelse(alphaChannelSupported(), 0.1, 1))
#bg.list = list("sp.polygons", bg.shp,"transparent", 0.1, 1)
bg.list = list("sp.polygons", bg.shp,"transparent")


data.f = "../../data/processed/BMAwl10d.csv"
grid.size = 1000
ext.grid.size = 5000
col.org = 1:5
col.to.run = 9:16
pdffile = "../../output/maps/IDWbkkGRID1000.pdf"
data.file = "../../data/processed/BMAwl10date.csv"

date.ts =  read.table(data.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

data.table =  read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
xy = cbind(data.table[,4],data.table[,5])
#obs.xy = SpatialPoints(xy)

max.x = max(xy[,1]) + ext.grid.size
min.x = min(xy[,1]) - ext.grid.size
max.y = max(xy[,2]) + ext.grid.size
min.y = min(xy[,2]) - ext.grid.size

x.range = max.x - min.x
y.range = max.y - min.y
x.n = x.range %/% grid.size
y.n = y.range %/% grid.size
x.start = (max.x - min.x - x.n*grid.size)/2 + min.x 
y.start = (max.y - min.y - x.n*grid.size)/2 + min.y



## Create GRID
grid.new = as.data.frame(array(NA, c(x.n*y.n,2)))
colnames(grid.new) = c("x","y")

for(i in 1:x.n){
for(j in 1:y.n){
grid.new[(j-1)*x.n+i,1] = x.start + (i-1)*grid.size
grid.new[(j-1)*x.n+i,2] = y.start + (j-1)*grid.size
cat("\nLine :",(i-1)*y.n+j,x.start + (i-1)*grid.size,y.start + (j-1)*grid.size)
}
}

grid.new = SpatialPoints(grid.new)
gridded(grid.new) = TRUE

#Points plot list
#pt.list = list("sp.points", grid.new, pch = 3, col = "grey", alpha = ifelse(alphaChannelSupported(), .5, 1))
pt.list = list("sp.points", grid.new, pch = 3, col = "grey", .5, 1)

pdf(pdffile, width= 10, height = 5)
par(ps =12,mfrow=c(2,2),mai = c(.8, .5, .2, .5))
#png("r_plot.png", width = 420, height = 340)




##
## Loop to run designed columns
for(i in 1:length(col.to.run)){

col.title = colnames(data.table)[col.to.run[i]]
cat("\nCol. :",i,col.title)

data.use = data.table[,c(col.org,col.to.run[i])]
colnames(data.use)[length(col.org)+1] = "use"

# Create SpatialPointsDataFrame
coordinates(data.use) = c("X","Y")
data(data.use)

#m <- krige(X11~1, data.table, grid.new, model = m)
#m <- krige(X11~1, data.table, grid.new)
m <- krige(use~1, data.use, grid.new)
#m <- krige(data.table[,8]~1,data.table, grid.new)



#Plot GRID
#print(spplot(m["var1.pred"], main = paste("IDW predictions",col.title,date.ts[i,2]),at = seq(0, 3, by = 0.01),col.regions=bpy.colors(300)))
print(spplot(m["var1.pred"], main = paste("IDW predictions",col.title,date.ts[i,2]),at = seq(0, 3, by = 0.01),col.regions=bpy.colors(300),sp.layout = list(bg.list, pt.list)))
print(spplot(m["var1.pred"], main = paste("IDW predictions",col.title,date.ts[i,2]),at = seq(0, 3, by = 0.01),col.regions=bpy.colors(300),sp.layout = list(bg.list)))





cat("-",i,date.ts[i,2])
#plot(background.mp, type = "o")

}
## END Loop to run designed columns

dev.off()
cat("\nEND script")


#rsaga.inverse.distance("BMAwl10d.shp")