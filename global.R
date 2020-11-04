url = "https://raw.githubusercontent.com/PhilapR/DS501Project/main/train.csv"
train = read.csv(url)
varnames = setdiff(names(train), c("Id","SalePrice"))