#install.packages(c("rgdal","maptools","gstat","spdep","spatstat","RSAGA","spgrass6"))
library(gstat)
library(RSAGA)
library(sp)

data.f = "BMAwl10d.csv"
grid.size = 5000
ext.grid.size = 10000

data.table =  read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
xy = cbind(data.table[,4],data.table[,5])
max.x = max(xy[,1]) + ext.grid.size
min.x = min(xy[,1]) - ext.grid.size
max.y = max(xy[,2]) + ext.grid.size
min.y = min(xy[,2]) - ext.grid.size
x.range = max.x - min.x + ext.grid.size*2
y.range = max.y - min.y + ext.grid.size*2
x.n = x.range %/% grid.size
y.n = y.range %/% grid.size
x.start = (max.x - min.x - x.n*grid.size)/2 + min.x 
y.start = (max.y - min.y - x.n*grid.size)/2 + min.y

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

coordinates(data.table) = c("X","Y")
data(data.table)

x <- krige(X11~1, data.table, grid.new, model = m)
spplot(x["var1.pred"], main = "ordinary kriging predictions")

#rsaga.inverse.distance("BMAwl10d.shp")