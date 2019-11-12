install.packages("ggplot2")
install.packages("imager")
install.packages("mixtools")
install.packages("baseline")

library(mixtools)
library(baseline)
library(ggplot2)
library(imager)

#Load and plot the RGB file
#data.file <- '/mnt/usb-WD_Elements_25A2_575852314531383859503438-0:0-part1/CHB/PASANTIA/imagenes_forestal/data/DJI_0805.jpg'
data.file <- './imagenes_forestal/data/DJI_0791.jpg'
im <- load.image(data.file)

plot(im)

data.file <- '../Captura.png'
recorte <- load.image(data.file)
plot(recorte)

data.file <- '../copa_referencia.png'
recorte <- load.image(data.file)
plot(recorte)

#GPerforms the conversion of the image from RGB to HSL colorspace
im_hsl <- RGBtoHSL(im)

plot(im_hsl)

#prints the image L channel from the HSL colorspace. index 3 is L
plot(channel(im_hsl, 3))

#Extracts the L component from HSL colorspace image
im_L <- im_hsl[,,3]

#Estimation of bimodal distribution parameters
param <- normalmixEM(as.vector(im_L))

hist(im_L)

#Rolling ball algorithm

bc.rollingBall <- baseline(im_L, wm=80, ws=80, method='rollingBall')
## Not run: 
plot(bc.rollingBall)

corregido <- as.cimg(bc.rollingBall@corrected)
plot(corregido)

#Top hat y bottom hat
#mask <- imfill(78,78,val=1)
#structuring element consists in a cricular shape of radius = 7 pixels
mask <- px.circle(7)
top_hat <- as.cimg(im_L) - mopening(as.cimg(im_L),mask)
bottom_hat <-  mclosing(as.cimg(im_L),mask) - as.cimg(im_L)
im_filt <- as.cimg(im_L) + top_hat - bottom_hat
plot(im_filt)
plot(top_hat)

obj_oscuros <- (im_L[,]>param$mu[1])*im_L
plot(as.cimg(obj_oscuros))

#inversión de imagen y adición de máximo de escala de grises
im_L_inv <- obj_oscuros*(-1)+max(obj_oscuros)
#baselines calculados
bline1 <- baseline(obj_oscuros,wm=6, ws=6, method='rollingBall')
bline2 <- baseline(im_L_inv,wm=6, ws=6, method='rollingBall')
#imagen suavizada
im_smooth <- pmax((bline1@baseline)*(-1),(bline2@baseline)*(-1))*(-1)

bline <- baseline(obj_oscuros,wm=80, ws=80, method='rollingBall')
im_smooth <- ((bline1@baseline)*(-1)+max(max(obj_oscuros)))*(-1)

plot(as.cimg(im_smooth))

#Top hat
mask <- px.circle(7)
#top_hat <- as.cimg(im_L) - mopening(as.cimg(im_L),mask,real_mode = FALSE)
#se aplica un umbralado del 50%
plot(threshold(mopening(as.cimg(corregido),mask,real_mode = FALSE),"90%"))
plot(threshold(as.cimg(corregido),"90%"))
plot(as.cimg(im_L))

noventaynueve <- quantile(im_L,.99)
obj_oscuros <- !(im_L[,]<noventaynueve)
plot(as.cimg(obj_oscuros))

hist(obj_oscuros)


