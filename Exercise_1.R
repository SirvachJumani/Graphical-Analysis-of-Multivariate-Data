##########################################
##                                      ##       
##       Multivariate Methods           ##
##                                      ##
##       Exercise Sheet 1:              ##
##      Graphical Analysis of           ##
##        Multivariate Data             ## 
##                                      ##
##########################################

# install.packages("aplpack")
# install.packages("fmsb")
# install.packages("corrgram")



#-------------------------------------------------------------------------------------------------
##  Exercise  1.1        
#-------------------------------------------------------------------------------------------------

## Exercise 1.1 a)

load("SPData.Rda")

head(SPData.raw)
str(SPData.raw)

SPData <- SPData.raw[-c(1:3, ncol(SPData.raw))]
rownames(SPData) <- SPData.raw[,2]

SPData.30 <- as.matrix(SPData[1:30,])

head(SPData.30)
str(SPData.30)


## Exercise 1.1 b)

library(aplpack)
faces(SPData.30, nrow.plot=6, ncol.plot=5)

pdf("SP30.pdf")
faces(SPData.30, nrow.plot=6, ncol.plot=5)
dev.off()


## Exercise 1.1 c)

library(fmsb)

max <- max(SPData.30, na.rm = TRUE)
min <- min(SPData.30, na.rm = TRUE)

SPData.30.plus <- as.data.frame(rbind(rep(max,11) , rep(min,11), SPData.30))

radarchart(SPData.30.plus, cglcol = "black", pcol = rainbow(30))

par(mfrow = c(3, 3))

for(i in 1:9){
  radarchart(SPData.30.plus[c(1,2,2+i),], cglcol = "black", pcol = rainbow(30)[i])
}


for(i in 10:18){
  radarchart(SPData.30.plus[c(1,2,2+i),], cglcol = "black", pcol = rainbow(30)[i])
}


for(i in 19:27){
  radarchart(SPData.30.plus[c(1,2,2+i),], cglcol = "black", pcol = rainbow(30)[i])
}


for(i in 28:30){
  radarchart(SPData.30.plus[c(1,2,2+i),], cglcol = "black", pcol = rainbow(30)[i])
}


## Exercise 1.1 d)

par(mfrow = c(1, 1))

pie(SPData.30[,5], labels=rownames(SPData.30))


## Exercise 1.1 e)

pairs(SPData.30)
cor(SPData.30[complete.cases(SPData.30)==TRUE,])


library(corrgram)

corrgram(SPData.30, order=TRUE)
corrgram(SPData.30, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie)

## Exercise 1.1 f)

heatmap(as.matrix(SPData.30))







