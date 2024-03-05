library("readxl")
library(data.table)
library(lubridate) 

# setwd("~/Programming/erasmusCourses/DS/Final Project/proyecto_ds")

# d04 <- read_excel("data/uncleaned/2004/2004_todos.xls", sheet = 1)
# # There are two columns here that have the exact same data; drop the extra
# different_values <- d04[["FechaCaptura...7"]] != d04[["FechaCaptura...8"]]
# print(which(different_values))
# d04 <- subset(d04, select = -FechaCaptura...7)
# names(d04)[names(d04) == "FechaCaptura...8"] <- "FechaCaptura"
d04 <- fread("data/uncleaned/2004/2004_todos_csv.csv")
d04 <- d04[,-8]
d04$FechaCaptura <- dmy(d04$FechaCaptura)
d04 <- d04[complete.cases(d04$FechaCaptura), ]


# There's an issue with the date encoding, this file needed to be converted to csv
# Reading it as an excel always tried to parse an entire column the same way
# Exporting it as a csv allowed each row to be written with its own date format
# Then the lubridate in R can take care of parsing each row however it should be parsed
# d05 <- read_excel("data/uncleaned/2005/2005_todos.xls", sheet = 1)
# # There are two columns here that have the exact same data
# different_values <- d05[["FechaCaptura...7"]] != d05[["FechaCaptura...8"]]
# print(which(different_values))
# d05 <- subset(d05, select = -FechaCaptura...7)
# names(d05)[names(d05) == "FechaCaptura...8"] <- "FechaCaptura"

d05 <- fread("data/uncleaned/2005/2005_todos_csv.csv")
d05 <- d05[, -7]
d05$FechaCaptura <- dmy(d05$FechaCaptura)
# length(subset(d05, is.na(FechaCaptura))$d05)

d06 <- read_excel("data/uncleaned/2006/2006_todos.xls", sheet = 1, col_types = "text")
d06$NombreAnillador <- "MANUEL VAZQUEZ CASTRO"
d06$FechaCaptura <- as.Date(d06$FechaCaptura, format = "%d-%m-%y")

# There are some columns that are not shared; these should be dropped later 
# if they are unimportant.

# There was an issue with the HoraCaptura encoding in this sheet

d07 <- fread("data/uncleaned/2007/2007_todos_csv.csv")
d07$FechaCaptura <- as.Date(d07$FechaCaptura, format = "%d/%m/%y")

# Same; the hora is text. 
d08 <- read_excel("data/uncleaned/2008/2008_todos.xlsx", sheet = 1, col_types = "text")
d08$CodigoGrupo <-660019
d08$NombreGrupo <- "MANUEL VAZQUEZ CASTRO"
d08$FechaCaptura <- as.Date(d08$FechaCaptura, format = "%d-%m-%y")

# There was also an issue with the dates encoding in this file. Had to change the 
# custom format of the date in excel to all be dmy, then export it to a csv (because there 
# were still a bunch of rows as text). Then coerce them using dmy
# colTypes <- rep("text", 59)
# colTypes[8] <- "date"
# d09 <- read_excel("data/uncleaned/2009/2009_todos.xlsx", sheet = "Hoja1", col_types = colTypes)
d09 <-fread("data/uncleaned/2009/2009_todos_csv.csv")
d09$FechaCaptura <- dmy(d09$FechaCaptura)


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
d10 <- subset(d10, CodigoGrupo != "")

# Some of the dates were missing delimiters (e.g. 13/052010)
# d10$FechaCaptura <- as.Date(d10$FechaCaptura, format = "%d/%m/%Y")
d10$FechaCaptura <- dmy(d10$FechaCaptura)

colTypes <- rep("text", 59)
colTypes[8] <- "date"
d11 <- read_excel("data/uncleaned/2011/2011_todos.xls", sheet = "DATOS", col_types = colTypes)

colTypes <- rep("text", 19)
colTypes[8] <- "date"
d12 <- read_excel("data/uncleaned/2012/2012_todos.xls", sheet = "DATOS", col_types = colTypes)

colTypes <- rep("text", 59)
colTypes[8] <- "date"
d13 <- read_excel("data/uncleaned/2013/2013_todos.xls", sheet = "DATOS", col_types = colTypes)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d13_extra <- read_excel("data/uncleaned/2013/2013_falta.xls", sheet = "DATOS", col_types = colTypes)
not_shared1 <- setdiff(names(d13_extra), names(d13))
not_shared1
not_shared2 <- setdiff(names(d13), names(d13_extra))
not_shared2
names(d13_extra)[which(names(d13_extra) == "Anilla METAL")] <- "Anilla"
d13_extra <- d13_extra[, !names(d13_extra) %in% not_shared1]
d13 <- rbind(d13, d13_extra)

colTypes <- rep("text", 63)
colTypes[8] <- "date"
d14 <- read_excel("data/uncleaned/2014/2014_todos.xls", sheet = "DATOS", col_types = colTypes)
d14_extra <- read_excel("data/uncleaned/2014/2014_falta.xls", sheet = "DATOS", col_types = colTypes)
names(d14)[which(names(d14) == "Anilla METAL")] <- "Anilla"
d14 <- rbind(d14, d14_extra)

d15 <- read_excel("data/uncleaned/2015/2015_todos.xls", sheet = "DATOS", col_types = colTypes)
names(d15)[which(names(d15) == "Anilla METAL")] <- "Anilla"

combine_if_no_common_anillas <- function(df1, df2) {
  # Check if any values in "Anilla" column of df1 are the same as df2
  x <- df2$Anilla %in% df1$Anilla
  if (!any(x)) {
    combined_df <- rbind(df1, df2)
    return(combined_df)
  } else {
    print("DUPLICATED ANILLA")
    print(which(x))
    return(df1)
  }
}

colTypes <- rep("text", 59)
d16_1 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR1.xls", col_types = colTypes)
d16_2 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR2.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16_1, d16_2)
d16_3 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR3.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_3)
d16_4 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR4.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_4)
d16_5 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR5.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_5)
d16_6 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR6.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_6)
names(d16)[names(d16) == "Fecha"] <- "FechaCaptura"
d16$NombreGrupo <- "EUSEBIO GOMEZ REINA"


prep_for_combo_and_combine <- function(df16_, d16) {
  names(df16_)[names(df16_) == "Anilla METAL"] <- "Anilla"
  not_shared <- setdiff(names(df16_), names(d16))
  print("The following columns will be removed: ")
  print(not_shared)
  if(length(not_shared)>0){
    print("Removed unshared")
    df16_ <- df16_[, !names(df16_) %in% not_shared]
  } else{
    print("All columns the same")
  }
  d16 <- combine_if_no_common_anillas(d16, df16_)
  return(d16)
}

d16_7 <- read_excel("data/uncleaned/2016/Enviados/2016ZamSayago.xls", col_types = "text")
d16_7$NombreGrupo <- "JOSE MANUEL SAYAGO ROBLES"
d16 <- prep_for_combo_and_combine(d16_7, d16)
d16_8 <- read_excel("data/uncleaned/2016/Enviados/2016Zam1.xls", col_types = "text")
d16 <- prep_for_combo_and_combine(d16_8, d16)
d16_9 <- read_excel("data/uncleaned/2016/Enviados/2016Zam2.xls", col_types = "text")
d16 <- prep_for_combo_and_combine(d16_9, d16)

# Doesn't work because some of the dates were stored as excel numbers.
# d16$FechaCaptura1 <- as.Date(d16$FechaCaptura, format = "%d/%m/%Y")

# Function to convert Excel serial numbers to dates
excel_to_date <- function(x) {
  # Check if the value is numeric (Excel serial number)
  if(grepl("^-?\\d*\\.?\\d+$", x)) {
    # Convert Excel serial number to date
    as.Date(as.numeric(x) - 1, origin = "1899-12-30",)
  } else {
    # Return as is (text date)
    as.Date(x, format = "%d/%m/%Y")
  }
}

# Apply the function to the column
converted_dates <- lapply(d16$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d16$FechaCaptura <- converted_dates
d16$FechaCaptura <- as.Date(d16$FechaCaptura, format = "%Y-%m-%d")

colTypes <- rep("text", 63)
d17 <- read_excel("data/uncleaned/2017/2017_todos_fixed.xlsx", sheet = "DATOS", col_types = colTypes)
# d17$FechaCaptura <- as.Date(d17$FechaCaptura, format = "%d/%m/%Y")

# TODO: Add this extra sheet of data in
# colTypes[8] <- "date"
# d17_extra <- read_excel("data/uncleaned/2017/2017_extra.xls", sheet = "DATOS", col_types = colTypes)
# any(d17_extra[["Anilla METAL"]] %in% d17[["Anilla METAL"]])
# d17_extra$FechaCaptura <-ymd(d17_extra$FechaCaptura)
# d17 <- rbind(d17, d17_extra)
names(d17)[which(names(d17) == "Anilla METAL")] <- "Anilla"
converted_dates <- lapply(d17$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d17$FechaCaptura <- converted_dates
d17$FechaCaptura <- as.Date(d17$FechaCaptura, format = "%Y-%m-%d")


colTypes <- rep("text", 63)
colTypes[8] <- "date"
d18_1 <- read_excel("data/uncleaned/2018/2018_1.xls", col_types = colTypes)
d18_1$NombreGrupo <- "JUAN MANUEL SAEZ MUNOZ"
d18_2 <- read_excel("data/uncleaned/2018/2018_EGR1.xls", col_types = "text")
d18_2$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_2)[which(names(d18_2) == "Fecha")] <- "FechaCaptura"
d18_2$FechaCaptura <- as.Date(d18_2$FechaCaptura, format = "%d/%m/%Y")
d18 <- prep_for_combo_and_combine(d18_1, d18_2)
d18_3 <- read_excel("data/uncleaned/2018/2018_EGR2.xls", col_types = "text")
d18_3$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_3)[which(names(d18_3) == "Fecha")] <- "FechaCaptura"
d18_3$FechaCaptura <- as.Date(d18_3$FechaCaptura, format = "%d/%m/%Y")
d18 <- combine_if_no_common_anillas(d18, d18_3)
d18_4 <- read_excel("data/uncleaned/2018/2018_EGR3.xls", col_types = "text")
d18_4$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_4)[which(names(d18_4) == "Fecha")] <- "FechaCaptura"
d18_4$FechaCaptura <- as.Date(d18_4$FechaCaptura, format = "%d/%m/%Y")
d18 <- combine_if_no_common_anillas(d18, d18_4)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d18_5 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV1.xls", col_types = colTypes)
d18_5$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_5, d18)
d18_6 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV2.xls", col_types = colTypes)
d18_6$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_6, d18)
d18_7 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV3.xls", col_types = colTypes)
d18_7$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_7, d18)
d18_8 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV4.xls", col_types = colTypes)
d18_8$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_8, d18)
d18_9 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV5.xls", col_types = colTypes)
d18_9$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_9, d18)
d18_10 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV6.xls", col_types = colTypes)
d18_10$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_10, d18)
d18_11 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV7.xls", col_types = colTypes)
d18_11$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_11, d18)

d19_1 <- read_excel("data/uncleaned/2019/ZamPVC-MV2019.xls", sheet="DATOS", col_types = colTypes)
d19_1$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d19_2 <- read_excel("data/uncleaned/2019/ZamEGR2019.xls", col_types = "text")
d19_2$NombreGrupo <-"EUSEBIO GOMEZ REINA"
names(d19_2)[which(names(d19_2) == "Fecha")] <- "FechaCaptura"
d19_2$FechaCaptura <- as.Date(d19_2$FechaCaptura, format = "%d/%m/%Y")
d19 <- prep_for_combo_and_combine(d19_1, d19_2)
d19_3 <- read_excel("data/uncleaned/2019/ZamJuanmaPVC2019.xls", col_types = colTypes)
d19_3$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d19 <- prep_for_combo_and_combine(d19_3, d19)
d19_4 <- read_excel("data/uncleaned/2019/ZamPVC-Sayago2019.xls", col_types = colTypes)
d19_4$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d19 <- prep_for_combo_and_combine(d19_4, d19)

d20 <- read_excel("data/uncleaned/2020/2020_todos.xls", sheet="DATOS", col_types = "text")
names(d20)[which(names(d20) == "Anilla METAL")] <- "Anilla"
converted_dates <- lapply(d20$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d20$FechaCaptura <- converted_dates
d20$FechaCaptura <- as.Date(d20$FechaCaptura, format = "%Y-%m-%d")


d21_1 <- read_excel("data/uncleaned/2021/Zam-EGR2021.xls", col_types = "text")
names(d21_1)[which(names(d21_1) == "Fecha")] <- "FechaCaptura"
d21_1$FechaCaptura <- as.Date(d21_1$FechaCaptura, format = "%d/%m/%Y")
d21_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_2 <- read_excel("data/uncleaned/2021/ZamPVC-MV2021.xls", sheet="DATOS", col_types = colTypes)
d21_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d21 <- prep_for_combo_and_combine(d21_2, d21_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d21_3 <- read_excel("data/uncleaned/2021/Zam-FJPM2021.xls", sheet="DATOS", col_types = colTypes)
d21_3$NombreGrupo <-"FRANCISCO JOSE PARRADO MARQUEZ"
d21 <- combine_if_no_common_anillas(d21, d21_3)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_4 <- read_excel("data/uncleaned/2021/Zam-JCAM2021.xls", sheet="DATOS", col_types = colTypes)
d21_4$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d21 <- prep_for_combo_and_combine(d21_4, d21)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d21_5 <- read_excel("data/uncleaned/2021/Zam-MHH2021.xls", sheet="DATOS", col_types = colTypes)
d21_5$NombreGrupo <-"MARIA HARANA HERRERA"
d21 <- combine_if_no_common_anillas(d21_5, d21)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_6 <- read_excel("data/uncleaned/2021/ZamPVC-JMSM2021.xls", sheet="DATOS", col_types = colTypes)
d21_6$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d21 <- prep_for_combo_and_combine(d21_6, d21)
d21_7 <- read_excel("data/uncleaned/2021/ZamPVC-JMSR2021.xls", sheet="DATOS", col_types = "text")
d21_7$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d21 <- prep_for_combo_and_combine(d21_7, d21)


d22_1 <- read_excel("data/uncleaned/2022/ZamEGR2022.xls", col_types = "text")
names(d22_1)[which(names(d22_1) == "Fecha")] <- "FechaCaptura"
d22_1$FechaCaptura <- as.Date(d22_1$FechaCaptura, format = "%d/%m/%Y")
d22_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d22_2 <- read_excel("data/uncleaned/2022/ZamPVC-MV2022.xls", sheet="DATOS", col_types = colTypes)
d22_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d22 <- prep_for_combo_and_combine(d22_2, d22_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d22_3 <- read_excel("data/uncleaned/2022/ZamFrancisco Javier Perez Mata2022.xls", sheet="DATOS", col_types = colTypes)
d22_2$NombreGrupo <-"FRANCISCO JOSE PARRADO MARQUEZ"
d22 <- combine_if_no_common_anillas(d22_3, d22)
d22_4 <- read_excel("data/uncleaned/2022/ZamJuan Carlos Adame Mejias2022.xls", sheet="DATOS", col_types = colTypes)
d22_4$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d22 <- combine_if_no_common_anillas(d22_4, d22)
d22_5 <- read_excel("data/uncleaned/2022/ZamMaria Harana Herrera2022.xls", sheet="DATOS", col_types = colTypes)
d22_5$NombreGrupo <-"MARIA HARANA HERRERA"
d22 <- combine_if_no_common_anillas(d22_5, d22)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d22_6 <- read_excel("data/uncleaned/2022/ZamPVCJuan Manuel Saenz Muñoz2022.xls", sheet="DATOS", col_types = colTypes)
d22_6$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d22 <- prep_for_combo_and_combine(d22_6, d22)
d22_7 <- read_excel("data/uncleaned/2022/ZamPVCJose Manuel Sayago Robles2022.xls", sheet="DATOS", col_types = colTypes)
d22_7$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d22 <- prep_for_combo_and_combine(d22_7, d22)

d23_1 <- read_excel("data/uncleaned/2023/Zam-EusebioGomez2023.xls", col_types = "text")
names(d23_1)[which(names(d23_1) == "Fecha")] <- "FechaCaptura"
d23_1$FechaCaptura <- as.Date(d23_1$FechaCaptura, format = "%d/%m/%Y")
d23_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 64)
colTypes[8] <- "date"
d23_2 <- read_excel("data/uncleaned/2023/ZamPVC-ManuelVazquez2023.xls", sheet="DATOS", col_types = colTypes)
d23_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d23 <- prep_for_combo_and_combine(d23_2, d23_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d23_3 <- read_excel("data/uncleaned/2023/Zam-JuanCarlosAdame2023.xls", sheet="DATOS", col_types = colTypes)
d23_3$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d23 <- combine_if_no_common_anillas(d23_3, d23)
d23_4 <- read_excel("data/uncleaned/2023/Zam-MariaHarana2023.xls", sheet="DATOS", col_types = colTypes)
d23_4$NombreGrupo <-"MARIA HARANA HERRERA"
d23 <- combine_if_no_common_anillas(d23_4, d23)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d23_5 <- read_excel("data/uncleaned/2023/ZamPVC-JoseManuelSayago2023.xls", sheet="DATOS", col_types = colTypes)
d23_5$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d23 <- prep_for_combo_and_combine(d23_5, d23)


rm(d13_extra, d14_extra, d10_01, d10_02, d10_03, d10_04, d10_05, d10_06, d10_07, d10_08, d10_09,
   d10_10,d10_11,d10_12, dfs_2010, d16_1, d16_2, d16_3, d16_4, d16_5, d16_6, d16_7, d16_8, d16_9,
   d18_1, d18_2, d18_3, d18_4, d18_5, d18_6, d18_7, d18_8, d18_9, d18_10, d18_11,
   d19_1, d19_2, d19_3, d19_4, d21_1, d21_2, d21_3, d21_4, d21_5, d21_6, d21_7,
   d22_1, d22_2, d22_3, d22_4, d22_5, d22_6, d22_7,
   d23_1, d23_2, d23_3, d23_4, d23_5)

# Names of ringers:
# MANUEL VAZQUEZ CASTRO
# EUSEBIO GOMEZ REINA
# JOSE MANUEL SAYAGO ROBLES
# ROBERTO CARLOS PAYA SANCHEZ
# FRANCISCO JOSE PARRADO MARQUEZ
# JUAN MANUEL SAEZ MUNOZ
# ARACELI GARRIDO AGUILERA
# JUAN CARLOS ADAME MEJIAS
# MARIA HARANA HERRERA
# FRANCISCO JAVIER PEREZ MATA



#################### DONE READING FILES. NOW COMBINING #########################
# List of dataframe names
dataframe_names <- c("d04", "d05", "d06", "d07", "d08", "d09", "d10", "d11", "d12", 
                     "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", 
                     "d22", "d23")
all_column_names <- c()

for (df_name in dataframe_names) {
  df_col_names <- colnames(get(df_name))
  all_column_names <- c(all_column_names, df_col_names)
}

unique_column_names <- unique(all_column_names)

# print(sort(unique_column_names))

# Name columns consistently
for (name in dataframe_names) {
  # Check if columns exist before renaming
  if ("NombreGrupo" %in% colnames(get(name))) {
    assign(name, transform(get(name), 
                           NombreAnillador = NombreGrupo))
  }
  if ("CodigoGrupo" %in% colnames(get(name))) {
    assign(name, transform(get(name), 
                           CodigoAnillador = CodigoGrupo))
  }
}

# Define the desired columns
desired_columns <- c("CodigoAnillador", "NombreAnillador", "Anilla", 
                     "NombreEspecie", "CodigoEspecie", "FechaCaptura", 
                     "CodigoEdad", "CodigoSexo", "NombreLocalidad", 
                     "MUNICIPIO", "CodigoLocalidad", "Observaciones", 
                     "CodigoHabitat", "CodigoMetodo", "NumRedes", 
                     "HoraCaptura", "HoraDesde", "HoraHasta", 
                     "TipoRegistro", "CodigoCentro", "CodigoReclamo", "DataFrameName")

# Add a new column with the dataframe name
for (df_name in dataframe_names) {
  current_df <- get(df_name)
  current_df$DataFrameName <- df_name
  assign(df_name, current_df)
}

# Combine dataframes and select desired columns
combined_df <- do.call(rbind, lapply(dataframe_names, function(x) {
  df <- get(x)
  # Add missing columns with NA values
  missing_columns <- setdiff(desired_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }
  # Convert to data frame in case it is a data table
  df <- as.data.frame(df)
  # Reorder columns
  df<- df[, desired_columns]
  return(df)
}))


############# INSPECTING MISSING VALUES ######################
(missing_counts <- colSums(is.na(combined_df)))

# unique(subset(combined_df, is.na(CodigoAnillador))$DataFrameName)

# These sheets all have a lot of dates missing, most likely just because the 
# date is in the wrong format (dd/mm/yy and mm/dd/yyyy combo)
missingDate <- unique(subset(combined_df, is.na(FechaCaptura))$DataFrameName)

for (df_name in missingDate) {
  subset_df <- get(df_name)  # Assuming your dataframes are named according to the values in DataFrameName
  missing_count <- length(subset(subset_df, is.na(FechaCaptura))$DataFrameName)
  print(paste("Missing date count in", df_name, ":", missing_count))
}

# missing_rows <- subset(d17, is.na(FechaCaptura))
# head(missing_rows)

# Sheet is just missing a ton of locality data; nothing I can do about it. Maybe Manolo's data
# will be able to fill it in a bit
missingLocality <- unique(subset(combined_df, is.na(NombreLocalidad))$DataFrameName)
for (df_name in missingLocality) {
  subset_df <- get(df_name)  # Assuming your dataframes are named according to the values in DataFrameName
  missing_count <- length(subset(subset_df, is.na(NombreLocalidad))$DataFrameName)
  print(paste("Missing locality count in", df_name, ":", missing_count))
}


###################### FILL IN MANOLOS DATA #######################

manolos_data <- fread("data/anillamiento_de_Manolo.csv")

# missing_values <- manolos_data$METAL[!manolos_data$METAL %in% combined_df$Anilla]
# 76000 missing, but prob from 1987-2003
# manolos_subset_2004_onwards <- manolos_data[manolos_data$AÑO > 2003, ]
# Find the values in 'manolos_subset$METAL' that are not present in 'combined_subset$Anilla'
# missing_values <- manolos_subset_2004_onwards$METAL[!manolos_subset_2004_onwards$METAL %in% combined_df$Anilla]
# print(length(missing_values))
# There's still another 17500 missing rings.

# Prep to combine
manolos_data$CodigoAnillador <-660019
manolos_data$NombreAnillador <- "MANUEL VAZQUEZ CASTRO"
names(manolos_data)[which(names(manolos_data) == "METAL")] <- "Anilla"
names(manolos_data)[which(names(manolos_data) == "ESPECIE")] <- "NombreEspecie"
names(manolos_data)[which(names(manolos_data) == "FECHA")] <- "FechaCaptura"
names(manolos_data)[which(names(manolos_data) == "LUGAR")] <- "NombreLocalidad"
names(manolos_data)[which(names(manolos_data) == "EDAD")] <- "CodigoEdad"
names(manolos_data)[which(names(manolos_data) == "SEXO")] <- "CodigoSexo"


# Fill in missing locations data
missingLocations <- subset(combined_df, is.na(NombreLocalidad))

# Find the common Anilla values between missingLocations and manolos_data
common_anilla <- intersect(missingLocations$Anilla, manolos_data$Anilla)
# Find the corresponding rows in manolos_data
matching_rows_man <- manolos_data$Anilla %in% common_anilla
# Find the corresponding rows in combined_df
matching_rows_comb <- combined_df$Anilla %in% common_anilla
# Update NombreLocalidad in combined_df where Anilla matches
combined_df$NombreLocalidad[matching_rows_comb] <- manolos_data$NombreLocalidad[matching_rows_man]

# That resolved basically ALL of the missing localities :D
# length(subset(combined_df, is.na(NombreLocalidad)))


# Fill in missing names data
(preTable <- table(combined_df$NombreAnillador))

anillador_missing <- combined_df[combined_df$NombreAnillador == "ZAMALLA", ]
common_anilla <- intersect(anillador_missing$Anilla, manolos_data$Anilla)
# Find the corresponding rows in combined_df
matching_rows_comb <- combined_df$Anilla %in% common_anilla
# Update NombreLocalidad in combined_df where Anilla matches
combined_df$NombreAnillador[matching_rows_comb] <- "MANUEL VAZQUEZ CASTRO"

table(combined_df$NombreAnillador)
# MANUEL VAZQUEZ CASTRO went from 58634 to 116479, 
# and ZAMALLA from 63482 to 5637


# Add the missing rings from Manolo's data to the combined sheet
new_anilla <- setdiff(manolos_data$Anilla, combined_df$Anilla)

if (length(new_anilla) > 0) {
  # Create a subset of manolos_data with only the new Anilla values
  subset_manolos_data <- manolos_data[manolos_data$Anilla %in% new_anilla]
  subset_manolos_data$DataFrameName <- "Manolo"
  
  # Add NA values for missing columns
  missing_columns <- setdiff(colnames(combined_df), colnames(subset_manolos_data))
  for (col in missing_columns) {
    subset_manolos_data[[col]] <- NA
  }
  subset_manolos_data <- subset(subset_manolos_data, select=colnames(combined_df))
  
  # Append subset_manolos_data to combined_df
  combined_df <- rbind(combined_df, subset_manolos_data)
}


(missing_counts <- colSums(is.na(combined_df)))


################ DATA CLEANUP ######################
# Remove duplicate rings
unique_data <- unique(combined_df)
dupes <- duplicated(unique_data, by = "Anilla")
combined_df <- unique_data[!dupes, ]

# Fix dates before 1987
combined_df <- subset(combined_df, as.Date(FechaCaptura) >= as.Date("1987-01-01"))

# 656 down to 555 unique locations
freq_loc_before <- table(combined_df$NombreLocalidad)
freq_loc_before <- as.data.frame(freq_loc_before)
combined_df$NombreLocalidad <- tolower(combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("la puebla del rio", "puebla del rio", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("la puebla del río", "puebla del rio", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("coria del río", "coria del rio", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("reserva biologica de donana", "reserva biologica de doñana", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("san juan del prto", "san juan del puerto", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("sanlucar d guadiana", "sanlucar de guadiana", combined_df$NombreLocalidad)
combined_df$NombreLocalidad <- gsub("sanlúcar de guadiana", "sanlucar de guadiana", combined_df$NombreLocalidad)

# 304 down to 216 unique municipalities
freq_munip_before <- table(combined_df$MUNICIPIO)
freq_munip_before <- as.data.frame(freq_munip_before)
combined_df$MUNICIPIO <- tolower(combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("la puebla del rio", "puebla del rio", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("la puebla del río", "puebla del rio", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("alcalá de los gazules", "alcala de los gazules", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("alcolea del río", "alcolea del rio", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("coria del río", "coria del rio", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("gibraleón", "gibraleon", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("jerez de la frta", "jerez de la frontera", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("la palam del condado", "la palma del condado", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("los palacios-", "los palacios", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("p.n. doñana", "p. nac. de doñana", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("puerto de  santa maria", "puerto de santa maria", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("puerto de santamaria", "puerto de santa maria", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("puerto santa maria", "puerto de santa maria", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("sanlucar de barrameda.", "sanlucar de barrameda", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("santa olalla de  cala", "santa olalla de cala", combined_df$MUNICIPIO)
combined_df$MUNICIPIO <- gsub("villamartín", "villamartin", combined_df$MUNICIPIO)

# Reduced from 32 to 24 names
freq_ani_before <- table(combined_df$NombreAnillador)
freq_ani_before <- as.data.frame(freq_ani_before)
combined_df$NombreAnillador <- tolower(combined_df$NombreAnillador)
combined_df$NombreAnillador <- iconv(combined_df$NombreAnillador, to = "ASCII//TRANSLIT")
combined_df$NombreAnillador <- gsub("'", "", combined_df$NombreAnillador)
combined_df$NombreAnillador <- gsub("~", "", combined_df$NombreAnillador)

freq_ani <- table(combined_df$NombreAnillador)
freq_ani <- as.data.frame(freq_ani)

freq_loc <- table(combined_df$NombreLocalidad)
freq_loc <- as.data.frame(freq_loc)

freq_munip <- table(combined_df$MUNICIPIO)
freq_munip <- as.data.frame(freq_munip)

freq_esp <- table(combined_df$NombreEspecie)
freq_esp <- as.data.frame(freq_esp)

write.csv(combined_df, file = './data/combined_anillamiento.csv', row.names = FALSE)

##Anillamiento

file_path <- "./data/combined_anillamiento.csv"

anillamiento <- read.csv(file_path)

# Function to count NA values in a dataframe
count_na <- function(dataframe) {
  sum(is.na(dataframe))
}

# Call the function with your dataframe
na_count <- count_na(anillamiento)
print(na_count)

na_rows <- anillamiento[!complete.cases(anillamiento), ]

anillamiento$CodigoSexo[is.na(anillamiento$CodigoSexo)] <- -1

na_count <- count_na(anillamiento)
print(na_count)

# Install and load the readr package if not already installed
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

# Set the file path
file_path <- "./limpio/anillamiento.csv"

# Write the dataframe to an Excel file
write_csv(anillamiento, file_path)

# Print a message indicating success
cat("Dataframe exported successfully to", file_path, "\n")

##Controles espa�a y extranjeros

# Set the file path
file_path <- "./data/controles_espana.csv"
file_path2 <- "./data/controles_extranjeros.csv"

# Read the Excel file
controles_espana <- read.csv(file_path)
controles_extranjeros <- read.csv(file_path2)

# Install and load the dplyr package if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Combine DIA, MES, and A�O columns and format as DD/MM/YYYY
controles_espana$"FECHA CONTROL 1" <- as.Date(paste(controles_espana$DIA, controles_espana$MES, controles_espana$A�O, sep = "/"), format = "%d/%m/%Y")
controles_extranjeros$"FECHA CONTROL 1" <- as.Date(paste(controles_extranjeros$DIA, controles_extranjeros$MES, controles_extranjeros$A�O, sep = "/"), format = "%d/%m/%Y")

# Print the head of the dataframe to verify the new column
head(controles_espana,10)
head(controles_extranjeros,10)

controles_espana <- controles_espana[, !(names(controles_espana) %in% c("Grasa", "Ala", "peso", "tarso", "pico", "cola", "Hora"))]
controles_extranjeros <- controles_extranjeros[, !(names(controles_extranjeros) %in% c("Grasa", "Ala", "peso", "tarso", "pico", "cola", "Hora"))]

head(controles_espana,10)
head(controles_extranjeros,10)

anillamiento <- list("anillamiento")

control <- list(
  "control",
  "encontrado herido y no liberado",
  "encontrado herido y  liberado",
  "anillamiento suelta silvestrista",
  "encontrado enfermo y  liberado",
  "encontrado exahusto y liberado en buen estado",
  "encontrado herido y  liberado, disparo",
  "ala rota",
  "atropellado recuperado y liberado",
  "encontrada enferma y liberada en buen estado",
  "encontrada sin volar no liberada",
  "mantenida en cautividad",
  "control",
  "encontrado enfermo y  liberado",
  "control anilla naranja",
  "capturada en red de pesca y liberada",
  "capturada para enjaular",
  "control anilla roja",
  "control cambio anilla ac4162",
  "encontrado en la playa y liberado",
  "encontrado exhausta y liberado en buen estado",
  "encontrado herida y no liberada",
  "mantenida en cautividad"
)

muerto <- list(
  "cazada escopeta", 
  "encontrado muerto",
  "cazada escopeta solo anilla",
  "cazada",
  "atropellado",
  "cazado con red de suelo",
  "colision con tendido electrico",
  "encontrada muerta",
  "encontrada solo anilla",
  "cazada con escopeta",
  "encotrado muerto",
  "cazada con red de tiro no se libera",
  "cazado con escopeta",
  "capturada por buho o rapaz",
  "atropellada",
  "capturada por un animal domestico no gato",
  "cazada por buho o rapaz",
  "cazada por esmerejon",
  "cazado",
  "encontrada anilla con detector de metales",
  "caido en un estanque",
  "enredada en red para proteger frutales y piscifactorias",
  "envenenada",
  "envenenado",
  "pinchado en despensa de lanius excubitor",
  "cazado con escopeta fecha carta",
  "colision con tendido electrico",
  "anilla leida telescopio",
  "cazada con percha",
  "cazada con red de tiro no se libera",
  "capturada por un gato",           
  "cazada con escopeta",
  "colision contra cristales",
  "control anilla roja leido telescopio",
  "encontrada solo anilla",
  "encontrado muerto anilla roja",
  "muerto trampa para otros animales",
  "ahogoda en deposito de agua",
  "anilla en egagropila",
  "capturada por buho o rapaz",
  "capturada por un animal silvestre",
  "cazada red de suelo",
  "cazado con percha",
  "colision con cristales",
  "colision con un avion",      
  "colision contra un coche anilla naranja",
  "encontrada la pata con la anilla",
  "encontrada solo anilla con detector de metales",
  "encontrado muerto anilla naranja",
  "muerta por botulismo",
  "muerta por colision con vehiculo",
  "muerta por un gato",
  "muerto en red japonesa ahogado",
  "otros",
  "solo anilla",
  "solo huesos y pvc"
)

# Function to swap values based on arrays
swap_values <- function(x) {
  x <- tolower(trimws(x))
  if (x %in% muerto) {
    return("muerto")
  } else if (x %in% control) {
    return("control")
  } else if (x %in% anillamiento) {
    return("anillamiento")
  } else {
    return("control")
  }
}

controles_espana$`Observaciones simple` <- sapply(controles_espana$Observaciones, swap_values)

controles_extranjeros$`Observaciones simple` <- sapply(controles_extranjeros$Observaciones, swap_values)

head(controles_espana,10)
head(controles_extranjeros,10)

distance_between <- function(location1, country1, location2, country2) {
  # Install and load the ggmap package if not already installed
  if (!requireNamespace("ggmap", quietly = TRUE)) {
    install.packages("ggmap")
  }
  library(ggmap)
  
  # Install and load the geosphere package if not already installed
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    install.packages("geosphere")
  }
  library(geosphere)
  
  # The API keys may not be valid at this time. Substitute them for appropiate ones
  
  register_stadiamaps("959128be-7040-455a-a96d-3a4fede1ba6a", write = FALSE)
  register_google("AIzaSyB5mUIt_VgX0xIQHDwwdFNtP9nUFHw5t8U")
  
  # Get coordinates for location1 and location2
  coords1 <- geocode(paste(location1, country1, sep = ", "))
  coords2 <- geocode(paste(location2, country2, sep = ", "))
  
  # Calculate distance using Haversine formula
  distance_km <- distVincentyEllipsoid(coords1, coords2) / 1000
  
  ret <- list(distance_km, coords1, coords2)
  
  # Return the distance
  return(ret)
}

merge_control_rows <- function(df) {
  # Iterate through each row
  for (i in 1:nrow(df)) {
    # Extract the current row
    current_row <- df[i, ]
    
    matching_row <- df[df$METAL == current_row$METAL & df$MODO != "A" & df$`FECHA CONTROL 1` > current_row$`FECHA CONTROL 1`, ]
    
    if (nrow(matching_row) > 0) {
      matching_row <- matching_row %>% arrange(`FECHA CONTROL 1`)
      matching_row <- head(matching_row,1)
      
      days <- difftime(matching_row$`FECHA CONTROL 1`, current_row$`FECHA CONTROL 1`, units = "days")
      
      # Create the new date columns
      df[i, "FECHA CONTROL 2"] <- matching_row$`FECHA CONTROL 1`
      if ("LOCALIDAD" %in% colnames(matching_row)) {
        df[i, "LOCALIDAD CONTROL 2"] <- matching_row$LOCALIDAD
        df[i, "PAIS CONTROL 2"] <- matching_row$PAIS
        distance <- distance_between(df[i, "LOCALIDAD"], df[i, "PAIS"], df[i, "LOCALIDAD CONTROL 2"], df[i, "PAIS CONTROL 2"])
      }
      else {
        df[i, "LUGAR CONTROL 2"] <- matching_row$LUGAR
        df[i, "MUNICIPIO CONTROL 2"] <- matching_row$MUNICIPIO
        df[i, "PROVINCIA CONTROL 2"] <- matching_row$PROVINCIA
        distance <- distance_between(df[i, "MUNICIPIO"], df[i, "MUNICIPIO CONTROL 2"], "Spain", "Spain")
      }
      df[i, "Kms"] <- distance[[1]]
      df[i, "LONGITUDE CONTROL 1"] <- distance[[2]][1, "lon"]
      df[i, "LATITUDE CONTROL 1"] <- distance[[2]][1, "lat"]
      df[i, "LONGITUDE CONTROL 2"] <- distance[[3]][1, "lon"]
      df[i, "LATITUDE CONTROL 2"] <- distance[[3]][1, "lat"]
      df[i, "EDAD CONTROL 2"] <- matching_row$EDAD
      df[i, "SEXO CONTROL 2"] <- matching_row$SEXO
      df[i, "ANILLADOR CONTROL 2"] <- matching_row$ANILLADOR
      df[i, "Dias"] <- days
      df[i, "Dias anotados"] <- matching_row$Dias
      df[i, "Kms anotados"] <- matching_row$Kms
      df[i, "Observaciones control 2"] <- matching_row$Observaciones
      df[i, "Observaciones simple control 2"] <- matching_row$`Observaciones simple`
    }
    else {
      df[i, "FECHA CONTROL 2"] <- NA
    }
  }
  
  df <- subset(df, !is.na(`FECHA CONTROL 2`))
  
  return(df)
}

controles_espana_merged <- merge_control_rows(controles_espana)
controles_extranjeros_merged <- merge_control_rows(controles_extranjeros)

# Print the head of the dataframe to verify the new column
head(controles_espana_merged,10)
head(controles_extranjeros_merged,10)

# Install and load the readr package if not already installed
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

# Set the file path
file_path <- "./limpio/controles_espana.csv"
file_path2 <- "./limpio/controles_extranjeros.csv"
file_path_merged <- "./limpio/controles_espana_merged.csv"
file_path_merged2 <- "./limpio/controles_extranjeros_merged.csv"

# Write the dataframe to an Excel file
write_csv(controles_espana, file_path)
write_csv(controles_extranjeros, file_path2)
write_csv(controles_espana_merged, file_path_merged)
write_csv(controles_extranjeros_merged, file_path_merged2)

# Print a message indicating success
cat("Dataframe exported successfully to", file_path, "\n")
cat("Dataframe exported successfully to", file_path2, "\n")
cat("Dataframe exported successfully to", file_path_merged, "\n")
cat("Dataframe exported successfully to", file_path_merged2, "\n")

##Controles_sin_datos_de_anillamiento

# Set the file path
file_path <- "./data/controles_sin_datos_de_anillamiento.csv"

# Read the Excel file
controles_sin_datos_de_anillamiento <- read.csv(file_path)

# Combine DIA, MES, and A�O columns and format as DD/MM/YYYY
controles_sin_datos_de_anillamiento$FECHA <- as.Date(paste(controles_sin_datos_de_anillamiento$Dia, controles_sin_datos_de_anillamiento$Mes, controles_sin_datos_de_anillamiento$A�o, sep = "/"), format = "%d/%m/%Y")

# Print the head of the dataframe to verify the new column
head(controles_sin_datos_de_anillamiento,10)

controles_sin_datos_de_anillamiento <- controles_sin_datos_de_anillamiento[, !(names(controles_sin_datos_de_anillamiento) %in% c("Tarso", "Ancho.tarso", "Cola", "Pico"))]

head(controles_sin_datos_de_anillamiento,10)

controles_sin_datos_de_anillamiento$`Observaciones simple` <- sapply(controles_sin_datos_de_anillamiento$Observaciones, swap_values)

# Install and load the readr package if not already installed
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

# Set the file path
file_path <- "./limpio/controles_sin_datos_de_anillamiento.csv"

# Write the dataframe to an Excel file
write_csv(controles_sin_datos_de_anillamiento, file_path)

# Print a message indicating success
cat("Dataframe exported successfully to", file_path, "\n")