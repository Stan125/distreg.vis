library(bamlss.vis)
art_data <- GAMart()
model <- bamlss(list(num ~ s(x1) + s(x2) + s(x3),
                     sigma ~ x1 + x2 + x3), data = art_data)
bamlss.vis::vis()
