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

res$colors <- ifelse(res$anomaly > 0, "red", "blue")

l <- split(res, res$chunk)

for (i in seq_along(l)){
  if (i != length(l)) {
    # to avoid calculating for the last element
    n <- nrow(l[[i]])
    y <- c(l[[i]]$TEMP[n], l[[i + 1]]$TEMP[1])
    x <- c(l[[i]]$DATE[n], l[[i + 1]]$DATE[1])
    
    temp_between <- mean(c(l[[i]]$AVE[n], l[[i + 1]]$AVE[1]))
    aprox <- approx(x = x, y = y, n = 100)
    ind <- which.min(abs(aprox$y - temp_between))
    
    l[[i]] <- rbind(l[[i]], l[[i]][n, ])
    l[[i]]$DATE[n + 1] <- aprox$x[ind]
    l[[i]]$AVE[n + 1] <- aprox$y[ind]
    l[[i]]$TEMP[n + 1] <- aprox$y[ind]
    print(i)
    
    l[[i + 1]] <- rbind(l[[i + 1]][1, ], l[[i + 1]])
    l[[i + 1]]$DATE[1] <- aprox$x[ind]
    l[[i + 1]]$AVE[1] <- aprox$y[ind]
    l[[i + 1]]$TEMP[1] <- aprox$y[ind]
    l[[i + 1]]$colors <- as.character(l[[i + 1]]$colors)
  }
}

plot(res$DATE, res$AVE, bty = "L", col = "black", type = "l", 
     ylab = "Temperature anomalies (wrt. 1981-2010)", xlab = "", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.5, ylim = c(-21.5, 28), xaxs = "i", 
     xaxt = "n", yaxs = "i", main = "2018")
res$DATE <-  (as.Date(res$DATE, origin = "1970-01-01"))
lines(res$DATE, res$AVE, lwd = 2)
axis(1, at = res$DATE[which(format(res$DATE, "%d") == "01")],
     labels = format(res$DATE[which(format(res$DATE, "%d") == "01")], "%b"))
abline(v = res$DATE[which(format(res$DATE, "%d") == "01")], col = "gray")
abline(h = -10:10 * 5, col = "gray")
lines(res$DATE, res$MAX, lty = 2, col = "red")
lines(res$DATE, res$MIN, lty = 2, col = "blue")
lapply(l, function(x) polygon(c(x$DATE, rev(x$DATE)),
                              c(x$TEMP, rev(x$AVE)),
                              col = x$colors))
legend(x = res$DATE[10], y = 25, 
       legend = c(2018, 2018, "MAX 1950-2017", "MIN 1950-2017"), 
       cex = 0.8, lty = c(1, 1, 2, 2), lwd = c(4, 4, 1, 1), 
       col = c("red", "blue", "red", "blue"), bg = "#FFFFFF90")
points(x = res$DATE[res$TEMP > res$MAX], 
       y = rep(-21, length(res$DATE[res$TEMP > res$MAX])), 
       pch = 15, lwd = 5, col = "red")
points(x = res$DATE[res$TEMP < res$MIN], 
       y = rep(-21, length(res$DATE[res$TEMP < res$MIN])),
       pch = 15, lwd = 5, col = "blue")

nr_of_days <- res %>%
  mutate(year = format(DATE, "%Y")) %>%
  filter(year == "2018") %>%
  group_by(format(DATE, "%Y-%m-01"), as.factor(anomaly)) %>%
  summarise(suma = sum(anomaly))
head(nr_of_days)

colnames(nr_of_days) <- c("data", "anom", "suma")
nr_of_days$data <- as.Date(nr_of_days$data)
nr_of_days$anom <- as.numeric(as.character(nr_of_days$anom))
head(nr_of_days)

miny <- nr_of_days[nr_of_days$anom == -1, ] %>% as.data.frame()
maxy <- nr_of_days[nr_of_days$anom == 1, ] %>% as.data.frame()
text(miny$data + 8,  26, labels = abs(miny$suma), col = "blue", cex = 0.8)
text(maxy$data + 22,  26, labels = abs(maxy$suma), col = "red", cex = 0.8)

title(main = NULL, sub = paste0("number of days < average:  ", abs(sum(miny$suma))), 
      col.sub = "blue", adj = 1) 
title(main = NULL, sub = paste0("\nnumber of days > average:  ", sum(maxy$suma)), 
      col.sub = "red", adj = 0)

