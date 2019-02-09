x <- 1:100
y1 <- sin(1:100 / 10) * 0.8
y2 <- sin(1:100 / 10) * 1.2
plot(x, y2, type = 'l')
lines(x, y1, col = 'red')

df <- data.frame(x = x, y1 = y1, y2 = y2)

df$pos_neg <- ifelse(df$y2 - df$y1 > 0, 1, -1) # above (1) or below (-1) average

# create the number for chunks to be split into lists:
df$chunk <- c(1, cumsum(abs(diff(df$pos_neg))) / 2 + 1) # first element needs to be added`
df$colors <- ifelse(df$pos_neg > 0, "red", "blue") # colors to be used for filling the polygons
# create lists to be plotted:
l <- split(df, df$chunk) # we should get 4 sub-lists
lapply(l, function(x)
  polygon(c(x$x, rev(x$x)), c(x$y2, rev(x$y1)), col = x$colors))

# 
# for (i in seq_along(l)){
#   if(i!=length(l)){ # to avoid calculating for the last element
# n <- nrow(l[[i]])
# y <- c(l[[i]]$TEMP[n], l[[i+1]]$TEMP[1])
# x <- c(l[[i]]$DATA[n], l[[i+1]]$DATA[1])
# 
# 
# temp_pomiedzy <- mean(c(l[[i]]$AVE[n], l[[i+1]]$AVE[1]))
# aprox <- approx(x = x, y = y)
# ind <- which.min(abs(aprox$y-temp_pomiedzy))
# 
# l[[i]] <- rbind(l[[i]],data.frame(DATA=aprox$x[ind], AVE=aprox$y[ind], TEMP=aprox$y[ind], anomal=NA, chunk=NA, colors=NA))
# l[[i+1]] <- rbind(data.frame(DATA=aprox$x[ind], AVE=aprox$y[ind], TEMP=aprox$y[ind], anomal=NA, chunk=NA, colors=l[[i+1]]$colors[1]), l[[i+1]])
# l[[i+1]]$colors <- as.character(l[[i+1]]$colors)
#    }
# }
# 
# plot(df$DATA, df$AVE, type='l', ylim=c(-8,8)) # start plotting
# lines(df$DATA, df$TEMP, type='l')
# # plot polygon for all elements in a list:
# lapply(l, function(x) polygon(c(x$DATA,rev(x$DATA)),c(x$TEMP,rev(x$AVE)),col=x$colors))
# 
