####Q https://stackoverflow.com/questions/51254073/tag-the-rows-with-change-in-values ####

#C

test <- structure(list(Year = 1998:2007, Pregnant = structure(c(2L, 2L, 
                                                        1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L), .Label = c("No", "Yes"), class = "factor"), 
               Infection = structure(c(2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 
                                       1L), .Label = c("Negative", "Positive"), class = "factor"), 
               Keep = c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L)), .Names = c("Year", 
                                                                             "Pregnant", "Infection", "Keep"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                   -10L))
new <- by(test,test,tail, n=1)

mylastDF <- do.call(rbind,new)


duplicated(test, fromLast = FALSE)



save <- by(data = test, INDICES = test$Pregnanat, head, n = 1)

by(data = test, INDICES = c(test$Pregnanat, test$Infection))

do.call("rbind", as.list(save))


for(i in 1:10){
  df <- test[i:i+1,]
  if(test[i,"Pregnant"] == "No" && test[i+1,"Pregnant"] != "No") {
    print(df)
    }
}

i <- 1
for(i in 1:10){
  df <- test[i:i+1,]
  if(test[i,"Infection"] == "Negative" && test[i+1,"Infection"] != "Negative") {
    print(df[i:i+1,])
    print(test[i,])
  }
}





####Q https://stackoverflow.com/questions/51269630/ggplotly-add-lines-between-map-points ####

library(ggplot2)
library(plotly)
library(ggmap)

df <- data.frame("Name" = c("A", "A", "A", "B","B"),
                 "lat" = c(42.04614, 40.14664, 37.63910, 29.73602, 33.97907),
                 "lng" = c(-88.03842, -82.98982, -122.41923, -95.58586, -84.21856))

map <- get_map(location = 'united states', zoom = 4, source = "google", color = "bw")
p <- ggmap(map) 
p <- p + geom_point(data = df, aes(x=lng, y=lat, group = factor(Name), colour = Name))
p <- p + geom_point(data = df, aes(x=lng, y=lat, group = Name, colour = Name))
plotly <- ggplotly(p)

 

####Q https://stackoverflow.com/questions/51272462/read-file-and-replace-by-a-variable ####

text <- readLines("text.txt")
textN <- gsub("N", "1", text)
write.table(textN,file="file_name.txt",quote=FALSE, row.names = FALSE, col.names = FALSE)

for(i in 1:2){
  f <- readLines("text.txt")
  f <- gsub("N", i, text)
  write.table(f,
              file=paste0("file_name_",i ,".txt"),
              quote=FALSE, 
              row.names = FALSE, 
              col.names = FALSE)
}

 

####Q https://stackoverflow.com/questions/51272689/ggplot2-how-to-change-the-order-of-a-legend ####

library(tidyverse)
library(ggplot2)
library(ggstance)
dat <- structure(list(Gender = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("Female", "Male"
                                           ), class = "factor"), Status = c("Case", "Case", "Case", "Case", 
                                                                            "Control", "Control", "Control", "Control", "Case", "Case", "Case", 
                                                                            "Case", "Control", "Control", "Control", "Control"), Type = c("Age30", 
                                                                                                                                          "Age30", "Age30", "Age30", "Age30", "Age30", "Age30", "Age30", 
                                                                                                                                          "Age50", "Age50", "Age50", "Age50", "Age50", "Age50", "Age50", 
                                                                                                                                          "Age50"), Risk = c(21.59862, 3.27479, 1.10073, 1.70754, 8.85253, 
                                                                                                                                                             1.66318, 0.23228, 0.44844, 18.01182, 3.80135, 1.40662, 2.75944, 
                                                                                                                                                    4.81212, 1.93184, 0.29695, 0.72521), Disease = c("D1", "D2", 
                                                                                                                                                                                                              "D3", "D4", "D1", "D2", "D3", "D4", "D1", "D2", "D3", "D4", "D1", 
                                                                                                                                                                                                              "D2", "D3", "D4")), .Names = c("Gender", "Status", "Type", "Risk", 
                                                                                                                                                                                                                                             "Disease"), row.names = c(NA, -16L), class = "data.frame")

ggplot(dat, aes(x = Risk, y = forcats::fct_reorder(Status, Risk), 
                group = Status,
                fill = Type)) +
  facet_grid(Disease ~ ., switch = 'y') +
  geom_barh(aes(fill = interaction(Status, Type)), 
            stat = 'identity', position = 'stackv', color = "darkgrey",
            size = 0.3) +
  scale_x_continuous(limits = c(0, 100)) +
  labs( y = "Disease", x = "Risk",
        plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() + 
  theme(
    text = element_text(size=10),
    strip.text.y = element_text(angle = 0),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(size = 11, hjust = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual("", 
                    breaks = c("Case.Age30", "Case.Age50",
                               "Control.Age30", "Control.Age50"),
                    values = c("#756bb1", "#2ca25f", 
                               "#bcbddc", "#99d8c9"), 
                    labels = c("Age 30 Case", "Age 50 Case",
                               "Age 30 Control", "Age 50 Control"))


####Q https://stackoverflow.com/questions/51273042/how-to-create-a-loop-that-applies-different-filters-to-data-frame ####

library(foreach)
library(tidyverse)
library(dplyr)

df1 <- tribble(~chr, ~location, ~gene, ~sample1, ~sample2,
               1, 12345, "FAM1", 0.1, 0,
               1, 124353, "ABCA", 1, 0.5,
               2, 12353, "ALMS1", 2, 0.1,
               3, 23456, "TNN", 0, 0,
               7, 657864, "MYBC3", 0.3, 1)

df2 <- tribble(~sucrose, ~fructose, ~glucose, ~galactose,
"FAM1", "FAM2", "ALMS1", "ALMS2",
"FAM2", "TNN2", "MYBC3", "ABCA",
"FAM3", "MYBC2", "TNN", "ABCA2",
"FAM4", "MYBC", "ABCA2", "FAM3",
"FAM5", "ALMS2", "ABCA3", "FAM4")

foreach()


sucrose <- df1 %>%
  filter(gene %in% df2[[1]]) %>%
  filter(gene != "")

sucrose$Number.of.MMVD.dogs <- (sucrose$sample1 + sucrose$sample2)

sucrose <- sucrose  %>%
  filter(Number.of.MMVD.dogs >= 0.01)

# S

foreach(i= 1:dim(df2)[2], .combine=rbind) %do% { 
  sucrose <- df1 %>%
    filter(gene %in% df2[[i]]) %>%
    filter(gene != "")
  sucrose$Number.of.MMVD.dogs <- (sucrose$sample1 + sucrose$sample2)
  sucrose <- sucrose  %>%
    filter(Number.of.MMVD.dogs >= 0.01)
}

# S

test <- foreach(i = 1:dim(df2)[2], .combine = list) %do% {
  var <- df1 %>%
    filter(gene %in% df2[[i]]) %>%
    filter(gene != "")
  var$Number.of.MMVD.dogs <- (var$sample1 + var$sample2)
  var <- var  %>%
    filter(Number.of.MMVD.dogs >= 0.01)
}


 


####Q https://stackoverflow.com/questions/51274090/impute-data-from-prior-row-similar-to-roll-join-in-r ####

sample_data <- data.frame(
  Class = rep(x = letters[1:10], each = 100),
  group = rep(x = c("inside", "outside"), each = 50),
  Sample_number = seq(1, 50, by = 1),
  x1 = rnorm(1000, mean = 0, sd = .5),
  x2 = 0
)
sample_data$Class_group <- paste0(sample_data$Class, "_", sample_data$group)
sample_data$Class_group <- as.factor(sample_data$Class_group)
sample_data$x1[sample_data$x1 < 0] <- NA 

####Q https://stackoverflow.com/questions/51287113/refering-to-a-variable-within-a-repeat-loop-within-a-function ####

library(Biobase)
f <- function(a){
  c <- data.frame(col_a = c(1,2,3,3,3,3,4), other_col = c(4,1,2,3,3,3,3))
  repeat{
    c$a[duplicated(c$a)] <- c$a[duplicated(c$a)] + 1
    if (length(c$a[!isUnique(c$a)]) == 0) break
  }
  return(c)
}

c <- data.frame(col_a = c(1,2,3,3,3,3,4), other_col = c(4,1,2,3,3,3,3))

f <- function(a){
  repeat{
    c$a[duplicated(c$a)] <- c$a[duplicated(c$a)] + 1
    if (length(c$a[!isUnique(c$a)]) == 0) break
  }
  return(c)
}

my_output <- f(c$col_a)




 

####Q https://stackoverflow.com/questions/51287747/from-a-dataframe-extract-columns-with-numerical-values#new-answer #### 

#C
#generate mixed data
dat <- matrix(rnorm(100), nrow = 20)
df <- data.frame(letters[1 : 20], dat)

#S

library(tidyverse)
str(df)

df[, is.numeric(df)]

df %>% select(which(sapply(., is.numeric)))

which(sapply(df, is.numeric))


#S

library(reshape2)

data <- head(pressure)

head(data)
melt(data, id = "pressure")


#S2
 

####Q https://stackoverflow.com/questions/51292321/extracting-list-data-from-text-file-in-r #### 

#S

library(stringr)

# Load text
text <- scan("text.txt",sep = "\n",what = "character")

# Identify (e.g. AU) rows
rem <- grep("^[[:blank:]]", text)

# Identify 
textLoop <- grep("^[[:upper:]]|^[[:blank:]]", text, value = TRUE)

# Check 
remValue <- grep("^[[:blank:]]", text, value = TRUE)

# Remove rows
textOne <- text[-rem]

# Vectors with only one object
partOne <- substring(textOne, 1, 2)
partTwo <- substring(textOne, 4)

# Collect vectors
textDf <- data.frame(partOne, partTwo)

textList <- as.list(textDf)

#S2

textLoopNew <- c()

for(i in 1:length(textLoop)){
  if(grepl("^[[:blank:]]", textLoop[i])){
    partOne <- substring(textLoop[i-1], 1, 2)
    textLoop[i] <- paste0(partOne, textLoop[i])
    #print(i)
    #print(paste0(partOne, textLoop[i]))
    #print(textLoop[i])
    #print(textLoopNew)
    #i <- 1
    #print(i)
  }
}
textLoop

partOne <- substring(textLoop, 1, 2)
partTwo <- substring(textLoop, 4)

# Collect vectors
textDf <- data.frame(partOne, partTwo)

#S3

library(stringr)

text <- scan("text.txt",sep = "\n",what = "character")

textLoop <- grep("^[[:upper:]]|^[[:blank:]]", text, value = TRUE)

for(i in 1:length(textLoop)){
  if(grepl("^[[:blank:]]", textLoop[i])){
    partOne <- substring(textLoop[i-1], 1, 2)
    textLoop[i] <- paste0(partOne, textLoop[i])
  }
}

textDf <- data.frame(partOne = substring(textLoop, 1, 2),
                     partTwo = substring(textLoop, 4))
 

####Q https://stackoverflow.com/questions/51155627/how-to-compare-results-of-regression-ml-model-after-each-run-programmatically ####

library("purrr")

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("adj.r.squared", "r.squared")

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

lr <- lm(mpg ~ wt, data = mtcars)
str(summary(lr))
 

####Q https://stackoverflow.com/questions/51307422/r-fuzzy-join-between-two-datasets #### 

name_a <- c("Aldo", "Andrea", "Alberto", "Antonio", "Angelo")
name_b <- c("Sara", "Serena", "Silvia", "Sonia", "Sissi")

zip_street_a <- c("1204 Roma Street 8", "1204 Roma Street 8", "1204 Roma Street 8", "1204 Venezia street 10", "1204 Venezia Street 110")

zip_street_b <- c("1204 Roma Street 81", "1204 Roma Street 8A", "1204 Roma Street 8B", "1204 Roma Street 8C", "1204 Venezia Street 10C")

db_a <- data.frame(name_a, zip_street_a)
db_b <- data.frame(name_b, zip_street_b)

names(db_a)[names(db_a)=='zip_street_a'] <- 'zipstreet'
names(db_b)[names(db_b)=='zip_street_b'] <- 'zipstreet'

library("fuzzyjoin")
library("bindrcpp")

match_data <- stringdist_left_join(db_a, db_b,
                                   by = "zipstreet",
                                   ignore_case = TRUE,
                                   method = "jaccard",
                                   max_dist = 1,
                                   distance_col = "dist"
) %>%
  group_by(zipstreet.x)

#S

library("tidyverse")

left_join(db_a, db_b,by = "zipstreet")
agrep() 
####Q ####

library("tidyverse")


df_group <- as.tibble(head(mtcars, n = 20)) %>%
  mutate(brand = word(rownames(.), 1)) %>%
  group_by(brand) %>%
  summarise_all(mean)

str_detect()
anti_join()

