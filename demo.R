source('bootlier.R')

challenger_temp_data <- c(66, 70, 69, 80, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 53, 67, 75, 70, 81, 76, 79, 75, 76, 58, 31)

result <- bootstrap.identify.outliers(challenger_temp_data)

p.data <- hist(result$data.truncated, breaks=seq(from=30, to=90, by=5))
p.outlier <- hist(result$data.outlier.set, breaks=seq(from=30, to=90, by=5))
plot(p.data, col = rgb(0,0,1,1/4), xlim=c(30,90))
plot(p.outlier, col = rgb(1,0,0,1/4), xlim=c(30,90), add=T)
















