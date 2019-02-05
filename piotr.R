library(dplyr)
library(tidyr)

# read the data -----------------------------------------------------------
dane <- read.table("https://meteomodel.pl/klimat/poltemp/bc/2018.stat.dat",
                   header = TRUE, stringsAsFactors = FALSE)
stat <- read.table("https://meteomodel.pl/klimat/poltemp/bc/dane.2018.dat",
                   header = TRUE, stringsAsFactors = FALSE)

res <- dane %>% left_join(stat, by = c("DATA" = "DATE")) %>% 
  mutate(DATE = as.numeric(as.Date(DATA)))

head(res)

# calculate anomalies -----------------------------------------------------
res$anomaly <- ifelse(res$TEMP - res$AVE > 0, 1, -1)
res$chunk <- c(1, cumsum(abs(diff(res$anomaly))) / 2 + 1) # first element needs to be added

res$kolorki <- ifelse(res$anomaly > 0, "red", "blue")
#res <- res[1:100,]

l <- split(res, res$chunk)

for (i in seq_along(l)){
  if (i != length(l)) {
    # to avoid calculating for the last element
    n <- nrow(l[[i]])
    y <- c(l[[i]]$TEMP[n], l[[i + 1]]$TEMP[1])
    x <- c(l[[i]]$DATA[n], l[[i + 1]]$DATA[1])
    
    temp_pomiedzy <- mean(c(l[[i]]$AVE[n], l[[i + 1]]$AVE[1]))
    aprox <- approx(x = x, y = y, n = 100)
    ind <- which.min(abs(aprox$y - temp_pomiedzy))
    
    l[[i]] <- rbind(l[[i]], l[[i]][n, ])
    l[[i]]$DATA[n + 1] <- aprox$x[ind]
    l[[i]]$AVE[n + 1] <- aprox$y[ind]
    l[[i]]$TEMP[n + 1] <- aprox$y[ind]
    print(i)
    
    l[[i + 1]] <- rbind(l[[i + 1]][1, ], l[[i + 1]])
    l[[i + 1]]$DATA[1] <- aprox$x[ind]
    l[[i + 1]]$AVE[1] <- aprox$y[ind]
    l[[i + 1]]$TEMP[1] <- aprox$y[ind]
    l[[i + 1]]$kolorki <- as.character(l[[i + 1]]$kolorki)
  }
}

plot(res$DATA, res$AVE, bty = "L", col = "black", type = "l", 
     ylab = "Temperature anomalies (wrt. 1981-2010)", xlab = "", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.5, ylim = c(-21.5, 28), xaxs = "i", 
     xaxt = "n", yaxs = "i", main = "2018")
res$DATA <-  (as.Date(res$DATA, origin = "1970-01-01"))
lines(res$DATA, res$AVE, lwd = 2)
axis(1, at = res$DATA[which(format(res$DATA, "%d") == "01")],
     labels = format(res$DATA[which(format(res$DATA, "%d") == "01")], "%b"))
abline(v = res$DATA[which(format(res$DATA, "%d") == "01")], col = "gray")
abline(h = -10:10 * 5, col = "gray")
lines(res$DATA, res$MAX, lty = 2, col = "red")
lines(res$DATA, res$MIN, lty = 2, col = "blue")
lapply(l, function(x) polygon(c(x$DATA, rev(x$DATA)),
                              c(x$TEMP, rev(x$AVE)),
                              col = x$kolorki))
legend(x = res$DATA[10], y = 25, 
       legend = c(2018, 2018, "MAX 1950-2017", "MIN 1950-2017"), 
       cex = 0.8, lty = c(1, 1, 2, 2), lwd = c(4, 4, 1, 1), 
       col = c("red", "blue", "red", "blue"), bg = "#FFFFFF90")
points(x = res$DATA[res$TEMP > res$MAX], 
       y = rep(-21, length(res$DATA[res$TEMP > res$MAX])), 
       pch = 15, lwd = 5, col = "red")
points(x = res$DATA[res$TEMP < res$MIN], 
       y = rep(-21, length(res$DATA[res$TEMP < res$MIN])),
       pch = 15, lwd = 5, col = "blue")

iledni <- res %>%
  mutate(year = format(DATA, "%Y")) %>%
  filter(year == "2018") %>%
  group_by(format(DATA, "%Y-%m-01"), as.factor(anomaly)) %>%
  summarise(suma = sum(anomaly))
head(iledni)

colnames(iledni) <- c("data", "anom", "suma")
iledni$data <- as.Date(iledni$data)
iledni$anom <- as.numeric(as.character(iledni$anom))
head(iledni)

miny <- iledni[iledni$anom == -1, ] %>% as.data.frame()
maxy <- iledni[iledni$anom == 1, ] %>% as.data.frame()
text(miny$data + 8,  26,  labels = abs(miny$suma), col = "blue", cex = 0.8)
text(maxy$data + 22,  26,  labels = abs(maxy$suma), col = "red", cex = 0.8)

title(main = NULL, sub = paste0("liczba dni < średniej:  ", abs(sum(miny$suma))), 
      col.sub = "blue", adj = 1) 
title(main = NULL, sub = paste0("\nliczba dni > średniej:  ", sum(maxy$suma)), 
      col.sub = "red", adj = 0)

