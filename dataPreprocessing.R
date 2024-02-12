library("readxl")

setwd("~/Programming/erasmusCourses/DS/Final Project/proyecto_ds")

d04 <- read_excel("data/uncleaned/2004/2004_todos.xls", sheet = 1)
# There are two columns here that have the exact same data; drop the extra
different_values <- d04[["FechaCaptura...7"]] != d04[["FechaCaptura...8"]]
print(which(different_values))
d04 <- subset(d04, select = -FechaCaptura...7)
names(d04)[names(d04) == "FechaCaptura...8"] <- "FechaCaptura"

d05 <- read_excel("data/uncleaned/2005/2005_todos.xls", sheet = 1)
# There are two columns here that have the exact same data
different_values <- d05[["FechaCaptura...7"]] != d05[["FechaCaptura...8"]]
print(which(different_values))
d05 <- subset(d05, select = -FechaCaptura...7)
names(d05)[names(d05) == "FechaCaptura...8"] <- "FechaCaptura"

d06 <- read_excel("data/uncleaned/2006/2006_todos.xls", sheet = 1)
not_shared1 <- setdiff(names(d06), names(d04))
not_shared2 <- setdiff(names(d04), names(d06))
# There are some columns that are not shared; these should be dropped later 
# if they are unimportant.
# -> Nope, it's just that Dato1 is Pata and Dato2 is Anilla color

# There was an issue with the HoraCaptura encoding in this sheet
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d07 <- read_excel("data/uncleaned/2007/2007_todos.xlsx", sheet = 1, col_types = colTypes)
# Same; the hora is text. 
d08 <- read_excel("data/uncleaned/2008/2008_todos.xlsx", sheet = 1)

colTypes <- rep("text", 59)
colTypes[8] <- "date"
d09 <- read_excel("data/uncleaned/2009/2009_todos.xlsx", sheet = "Hoja1", col_types = colTypes)

# 2010 was spread out over many files
d10_01 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(1).csv", sep=";")
d10_02 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(2).csv", sep=";")
d10_03 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(3).csv", sep=";")
d10_04 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(4).csv", sep=";")
d10_05 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(5).csv", sep=";")
d10_06 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(6).csv", sep=";")
d10_07 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(7).csv", sep=";")
d10_08 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(8).csv", sep=";")
d10_09 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(9).csv", sep=";")
d10_10 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(10).csv", sep=";")
d10_11 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(11).csv", sep=";")
d10_12 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(12).csv", sep=";")

dfs_2010 <- list(d10_01, d10_02, d10_03, d10_04, d10_05, d10_06, d10_07, d10_08, d10_09, d10_10, d10_11, d10_12)

# Combine dataframes into one
d10 <- do.call(rbind, dfs_2010)
