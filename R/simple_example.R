x <- 1:100
y1 <- sin(1:100 / 10) * 0.8
y2 <- sin(1:100 / 10) * 1.2
plot(x, y2, type = 'l')
lines(x, y1, col = 'red')

df <- data.frame(x = x, y1 = y1, y2 = y2)

df$pos_neg <-
  ifelse(df$y2 - df$y1 > 0, 1, -1) # above (1) or below (-1) average

# create the number for chunks to be split into lists:
df$chunk <-
  c(1, cumsum(abs(diff(df$pos_neg))) / 2 + 1) # first element needs to be added`
df$colors <-
  ifelse(df$pos_neg > 0, "red", "blue") # colors to be used for filling the polygons
# create lists to be plotted:
l <- split(df, df$chunk) # we should get 4 sub-lists
lapply(l, function(x)
  polygon(c(x$x, rev(x$x)), c(x$y2, rev(x$y1)), col = x$colors))