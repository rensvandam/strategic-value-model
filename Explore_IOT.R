# Explore IOT files

# Basic usage - load an RData file
load("./data/IOT/ICESHRE.rdata")

dim(ICESHRE)
names(ICESHRE)
head(ICESHRE, 20)

names(ICESHRE)
str(ICESHRE, list.len = 999, vec.len = 999)


load("./data/IOT/NATIOdomimp.rdata")

# Show dimension names
cat("Dimension 1 (years):", paste(head(dimnames(NATIOdomimp)[[1]]), collapse=", "), "\n")
cat("Dimension 2 (countries):", paste(head(dimnames(NATIOdomimp)[[2]]), collapse=", "), "\n") 
cat("Dimension 3 (categories):", paste(dimnames(NATIOdomimp)[[3]], collapse=", "), "\n")
cat("Dimension 4 (industries):", paste(dimnames(NATIOdomimp)[[4]], collapse=", "), "\n")

# Show a small sample
cat("\nSample for USA, 2019:\n")
print(NATIOdomimp["2019", "USA", c("DOM_10T12", "OUTPUT"), c("D10T12", "EXPO", "TOTAL")])