library(tm)
library(arules)
library(arulesViz)



groc_data <- read.csv(file.choose())
# phone_data1 <- readLines(file.choose()) # if we use readLines functions more lines will be reduced
View(groc_data)
str(groc_data)
# converting everything into character format 
groc_data[] <- lapply(groc_data,as.character)
View(groc_data)
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
groc_data["new_col"] <- apply(groc_data,1,paste_fun)
View(groc_data)

#install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices



library(tm)
x <- Corpus(VectorSource(groc_data$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

# Item Frequecy plot
detach(package:tm, unload=TRUE)
windows()
# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.7,minlen=2))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.7,minlen=3))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.7,minlen=3))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.7,minlen=4))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.8,minlen=4))
rules
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
rules_conf
plot(rules_conf)

# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
rules_lift
inspect(rules_lift)
plot(rules_lift)
# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "matrix")

plot(rules_conf,method = "graph")
plot(rules_conf,method = "scatterplot")
plot(rules_conf,method = "grouped")
plot(rules_conf,method = "matrix")

plot(rules_lift,method = "graph")
plot(rules_lift,method = "scatterplot")
plot(rules_lift,method = "grouped")
plot(rules_lift,method = "matrix")


