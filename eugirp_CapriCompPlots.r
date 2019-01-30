getwd()

library(reshape2)
library(data.table)
library(lattice)
library(latticeExtra)
library(grid)
library(colorspace)


unique(plotdata$datasource)

nir_data <- plotdata[plotdata$datasource == "nir", ]
unique(nir_data$party)
capri_data <- plotdata[plotdata$datasource == "capri", ]
unique(capri_data$party)

length(unique(nir_data$party))
length(unique(capri_data$party))
nrow(nir_data)
nrow(capri_data)
View(head(nir_data))
View(head(capri_data))

nir_capri <- merge(nir_data, capri_data, by = c("party", "variableUID"), all = TRUE)

length(!is.na(nir_capri$datasource.x))
sum(!is.na(nir_capri$datasource.x))
length(!is.na(nir_capri$datasource.y))
table(is.na(nir_capri$datasource.y))

nir_capri <- nir_capri[!is.na(nir_capri$datasource.x) & !is.na(nir_capri$datasource.y),  ]
nrow(nir_capri)
View(nir_capri)
write.csv(nir_capri, "nir_capri_toPlot.csv", row.names = FALSE)

names(nir_capri) 

nir_capri_N2O_EM <- nir_capri[nir_capri$gas.x == "N2O"& nir_capri$meastype.x == "EM", ]
View(nir_capri_N2O_EM)
names(nir_capri_N2O_EM)
length(nir_capri_N2O_EM)

dif <- (abs((nir_capri_N2O_EM[, 62:88] - nir_capri_N2O_EM[, 19:45]) / nir_capri_N2O_EM[, 19:45])) * 100
names(dif) <- paste0("X", c(1990:2016), ".dif")
head(dif)

nir_capri_N2O_EM <- cbind(nir_capri_N2O_EM, dif)
View(nir_capri_N2O_EM)

nir_capri_N2O_EM_kk1 <- nir_capri_N2O_EM
#nir_capri_N2O_EM <- nir_capri_N2O_EM_kk1

aggreg_1 <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

for (ct in unique(countries3)){
  for (ctg in LETTERS[1:6][-5]){
    for (gs in c("CH4", "N2O")){
      
      aggreg <- nir_capri_N2O_EM[nir_capri_N2O_EM$party %in% ct
                               & grepl(paste0("^3.", ctg), nir_capri_N2O_EM$sector_number.x)
                               & nir_capri_N2O_EM$gas.x %in% gs, ]
      #print(aggreg)
      if (nrow(aggreg) == 0) {
        #print(paste("nothing for", ct, ctg, gs, sep = " "))
        next}
      #View(aggreg)
      #names(aggreg)
      aggreg_nir <- as.data.frame(t(apply(aggreg[19:45], 1, function(x) ((x * 100) / colSums(aggreg[19:45], na.rm = TRUE)))))
      aggreg_capri <- as.data.frame(t(apply(aggreg[62:88], 1, function(x) ((x * 100) / colSums(aggreg[62:88], na.rm = TRUE)))))
      #aggreg_nir[is.na(aggreg_nir)] <- 0
      #aggreg_capri[is.na(aggreg_capri)] <- 0
      
      aggreg_avg <- aggreg_capri + aggreg_nir
      aggreg_avg <- aggreg_avg / 2
      names(aggreg_avg) <- paste0("X", c(1990:2016), ".ContrTot")
      
      aggreg <- cbind(aggreg[, c(1:2)], aggreg_avg)
      aggreg_1 <- rbind(aggreg_1, aggreg)
      #View(aggreg_1)
      
    }
  }
}

nir_capri_N2O_EM_kk <- nir_capri_N2O_EM
#nir_capri_N2O_EM <- nir_capri_N2O_EM_kk

nir_capri_N2O_EM <- merge(nir_capri_N2O_EM, aggreg_1, by = c("party", "variableUID"), all = TRUE)
View(nir_capri_N2O_EM)
names(nir_capri_N2O_EM)

years_1 <- years
years <- years[1:4]

data2plot_tot <- as.data.frame(matrix(nrow = 0, ncol = 0))

for (y in years){
  
  data2plot <- nir_capri_N2O_EM[, c(1, 6, which(grepl(y, names(nir_capri_N2O_EM))))]
  #data2plot <- droplevels(data2plot)
  #data2plot <-  data2plot[!duplicated(data2plot[, c(1,2)]), ]
  
  ##data2plot <- nir_capri_N2O_EM[, c(1, 6, 89:142)]
  data2plot <- data2plot[, -c(3:4)]
  ##data2plot <- melt(data2plot, id.vars = c(1:29))
  ##names(data2plot)[c(30, 31)] <- c("variable_ContrTot", "value_ContrTot")
  ##data2plot <- melt(data2plot, id.vars = c(1:2, 30:31))
  ##names(data2plot)[c(5, 6)] <- c("variable_dif", "value_dif")
  
  ##data2plot <-  separate(data2plot, variable_ContrTot, into = c("year", "kk"), sep = "\\.", remove = TRUE)
  ##data2plot <- data2plot[, -c(4)]
  ##data2plot$year <- gsub("X", "", data2plot$year)
  data2plot <- data2plot[grepl("^3.B", data2plot$sector_number.x), ]
  #data2plot <- droplevels(data2plot)
  data2plot_kk <- data2plot
  #data2plot <- data2plot_kk
  
  #data2plot <- data2plot[order(data2plot$X2004.dif), ]
  
  head(data2plot)
  names(data2plot)
  
  col_cat <- c("blue3", "deepskyblue", "cyan", "darkorchid1", "red", "darkorange", 
               "darkolivegreen1", "green", "gold2", "grey31", "grey56")
  col_cat <- col_cat[c(1:length(unique(data2plot$sector_number.x)))]
  
  col_party <- as.character(unique(data2plot$party))
  col_party <- as.character(c("A", "B", "b", "C", "K", "D", "d", "E", "e", "F", "f", "G", "g", "H", "h", "I", "i", "Y", 
                            "L", "l", "X", "M", "N", "P", "p", "R", "S", "s", "Z"))
  countrrr <- country4sub$code3[-c(11, 32, 33)]
  col_party <- c(65,66,98, 67, 75, 68,100,69,101,70,102,71,103,72,104,73,105,89,76,108,120,77,78,80,112,82,83,115,90)
  #data2par <- data.frame(unique(data2plot$party), col_party)
  #names(data2par)[1] <- "party"
  #data2plot <- merge(data2plot, data2par, all.x = TRUE)
  #head(data2plot)
  #rm(col_party)
  
  if (length(unique(data2plot$party)) != length(col_party)) stop("Review symbology of the countries")
  
  #data2plot$party <- droplevels(data2plot$party)
  #data2plot$col_party <- as.character(data2plot$col_party)
  #data2plot$sector_number.x <- droplevels(data2plot$sector_number.x)
  
  #data2plot<- data2plot[order(data2plot[, 1], data2plot[, 2]), ]
  head(data2plot)
  data2plot$year <- y
  
  names(data2plot) <- c("party", "sector_number.x", "dif", "ContrTot", "year")
  data2plot_tot <- rbind(data2plot_tot, data2plot)
  
}
  
data2plot <- data2plot_tot
rm(data2plot_tot)
head(data2plot)
  #tofill <- expand.grid(party = unique(data2plot$party), sector_number.x = unique(data2plot$sector_number.x))
  #head(tofill)
  #data2plot <- merge(data2plot, tofill, all = TRUE)
  #data2plot[is.na(data2plot)] <- 0 
  #head(data2plot)
  #mycol_ini <- trellis.par.get()$superpose.symbol$col
  
pdf("plot_fake_1.pdf", width = 10, height = 10)
  ##xyplot(value_ContrTot ~ value_dif | year, data = data2plot,

plt <- xyplot(ContrTot ~ dif | year, data = data2plot,
              as.table = TRUE, scales = "free", 
              #xlim = c(0, 100),
              #groups = party,
              drop.unused.levels = FALSE,
              #groups = c(party, sector_number.x),
              #pch = data2plot$col_party, 
              #par.settings = list(superpose.symbol = list(pch = col_party, col = col_cat, cex=3)), 
              pch = col_party[droplevels(data2plot$party)], cex = 3,
              col = col_cat[droplevels(data2plot$sector_number.x)],
              #layout = c(ceiling(sqrt(length(years))), round(sqrt(length(years)), 0)),
              key = list(text = list(c(as.character(unique(data2plot$party)), sort(as.character(unique(data2plot$sector_number.x))), "To check")),
                         points = list(pch = c(col_party, rep(15, length(unique(data2plot$sector_number.x))), 1), col = c(rep("black", length(unique(data2plot$party))), col_cat, "black"), fill = c(rep("black", length(unique(data2plot$party))), col_cat, "transparent"), cex = 1.5), 
                         columns = 3),
              #auto.key = list(columns = 3)
              ylab = "Contribution of categ. to Total (%)", xlab = "Difference (%)"
              )

z <- data2plot[order(data2plot$dif), ]
x <- z$dif
y <- z$ContrTot
#xx <- sort(sample(length(range(x)[1]:(range(x)[2])), length(x)))
#nls_fit <- nls(y ~ b * x^(-c), start = list(b = max(y), c = 1))
#plt2 <- xyplot(predict(nls_fit, newdata = xx) ~ xx, type = "l" ,  col = "green", ylim = c(0, 70))
z$kk <- (z$dif / 100) * (z$ContrTot / 100)
zz <- z[z$kk >= 0.05, ]
  
plt_dots <- xyplot(ContrTot ~ dif | year, data = zz,
                   as.table = TRUE, scales = "free", 
                   pch = 1, col = "black", cex = 3.5, lwd = 0.5
                   #type = "smooth"
                   )
  
plot(plt + as.layer(plt_dots))
  
plt_dots1 <- xyplot(ContrTot ~ dif | year, data = zz,
                   as.table = TRUE, scales = "free", 
                   #pch = 1, col = "black", cex = 0.8, lwd = 1
                   type = "smooth"
                   )
  
#plot(plt + as.layer(plt_dots) + as.layer(plt_dots1))
  
dev.off()


#

































nls_fit <- nls(y ~ b * x^(-c), start = list(b = max(y), c = 1))
nls_fit <- nls(y ~  1 * b * x^(c), start = list(b = max(y), c = 1))

plt2 <- xyplot(predict(nls_fit) ~ x, type = "l" ,  col = "green")


nls_fit <- nls(y ~  0.05 * b * x^(-c), start = list(b = max(y), c = 1))
nls_fit <- nls(y ~  0.05 * b * x^(c), start = list(b = max(y), c = 1))
plt3 <- xyplot(predict(nls_fit) ~ x, type = "l" ,  col = "red")

plot(plt + as.layer(plt3))


nls_fit <- nls(y ~  0.05 * x^(c), start = list(c = 1))
plt4 <- xyplot(predict(nls_fit) ~ x, type = "l" ,  col = "blue")


plot(plt2 + as.layer(plt3))
plot(plt2 + as.layer(plt3) + as.layer(plt4))
plot(plt2 + as.layer(plt) + as.layer(plt3) + as.layer(plt4))

plot(plt + as.layer(plt2) + as.layer(plt3) + as.layer(plt4))


z$kk <- (z$X2004.dif / 100) * (z$X2004.ContrTot / 100)



dev.off()





z <- data2plot[order(data2plot$X2004.dif), ]#[1:206,]    #/ 100
x <- z$X2004.dif
y <- z$X2004.ContrTot

z$kk <- (z$X2004.dif / 100) * (z$X2004.ContrTot / 100)

plot(x, y, type='l')
xyplot(y ~ x, type = "smooth")
qplot(x[!is.na(x)], y[!is.na(x)], geom="line")

loess_fit <- loess(y ~ x, na.action = na.exclude)
plot(x,y)
lines(x, predict(loess_fit), col = "red")

nls_fit <- nls(y ~ a + b * x^(-c), start = list(a = 0, b = 1, c = 2))
lines(x, predict(nls_fit), col = "blue")


plot(x,y)
View(z)
nls_fit <- nls(y ~ b * x^(-c), start = list(b = max(y), c = 2))
lines(x, predict(nls_fit), col = "green")

nls_fit <- nls(y ~ 5 + b * x^(-c), start = list(b = max(y), c = 2))
lines(x, predict(nls_fit), col = "red")

nls_fit <- nls(y ~ (a + b * x^(-c)), start = list(a = max(y), b = 2, c = 2))
lines(x, predict(nls_fit), col = "blue")


#
colors() 
topo.colors
heat.colors(4)
auto.key = list(x = .5, y = 0.7, corner = c(0, 0), columns = 2),
par.settings = list(superpose.polygon = list(col = topo.colors(length(unique(data2plot$sector_number.x)))))



#


























plot(X2004.ContrTot ~ X2004.dif, data = data2plot)

loess_fit <- loess(X2004.ContrTot ~ X2004.dif, data2plot, na.action = na.exclude)
lines(data2plot$X2004.dif, predict(loess_fit), col = "red")

nls_fit <- nls(X2004.ContrTot ~ a + b * X2004.dif^(-c), data2plot, start = list(a = 65, b = 20, c = 3.5))
nls_fit <- nls(X2004.ContrTot ~ a + b * X2004.dif^(-c), data2plot, start = list(a = 65, b = 1, c = 2))

y <- data2plot$X2004.ContrTot*(data2plot$X2004.dif)^3
plot(data2plot$X2004.dif, y, type='l')
loess_fit <- loess(y ~ X2004.dif, data2plot, na.action = na.exclude)
lines(data2plot$X2004.dif, predict(loess_fit), col = "Blue")

plot(y ~ data2plot$X2004.dif)                                                      

lines(data2plot$X2004.dif, predict(nls_fit), col = "blue")




#plot(plt + as.layer(plt2))






data <- nir_capri[, c(42, 85)]
data <- data[complete.cases(data), ]
plot(X2016.x ~ X2016.y, data)
# fit a loess line
loess_fit <- loess(X2016.x ~ X2016.y, data)
lines(data$X2016.y, predict(loess_fit), col = "blue")
# fit a non-linear regression
nls_fit <- nls(X2016.x ~ a + b * X2016.y^(-c), data, start = list(a=1000, b=1000, c=1000))
lines(data$X2016.y, predict(nls_fit), col = "red")
