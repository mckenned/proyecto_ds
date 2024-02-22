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
d10$FechaCaptura <- as.Date(d10$FechaCaptura, format = "%d/%m/%Y")

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

d16$FechaCaptura <- as.Date(d16$FechaCaptura, format = "%d/%m/%Y")


colTypes <- rep("text", 63)
d17 <- read_excel("data/uncleaned/2017/2017_todos_fixed.xlsx", sheet = "DATOS", col_types = colTypes)
d17$FechaCaptura <- as.Date(d17$FechaCaptura, format = "%d/%m/%Y")

colTypes[8] <- "date"
d17_extra <- read_excel("data/uncleaned/2017/2017_extra.xls", sheet = "DATOS", col_types = colTypes)
any(d17_extra[["Anilla METAL"]] %in% d17[["Anilla METAL"]])
d17 <- rbind(d17, d17_extra)
names(d17)[which(names(d17) == "Anilla METAL")] <- "Anilla"


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

d20 <- read_excel("data/uncleaned/2020/2020_todos.xls", sheet="DATOS", col_types = colTypes)
names(d20)[which(names(d20) == "Anilla METAL")] <- "Anilla"

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
d22_5 <- read_excel("data/uncleaned/2021/ZamMaria Harana Herrera2022.xls", sheet="DATOS", col_types = colTypes)
d22_5$NombreGrupo <-"MARIA HARANA HERRERA"
d22 <- combine_if_no_common_anillas(d22_5, d22)


colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_6 <- read_excel("data/uncleaned/2021/ZamPVC-JMSM2021.xls", sheet="DATOS", col_types = colTypes)
d21_6$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d21 <- prep_for_combo_and_combine(d21_6, d21)
d21_7 <- read_excel("data/uncleaned/2021/ZamPVC-JMSR2021.xls", sheet="DATOS", col_types = "text")
d21_7$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d21 <- prep_for_combo_and_combine(d21_7, d21)



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


rm(d13_extra, d14_extra, d10_01, d10_02, d10_03, d10_04, d10_05, d10_06, d10_07, d10_08, d10_09,
   d10_10,d10_11,d10_12, dfs_2010, d16_1, d16_2, d16_3, d16_4, d16_5, d16_6, d16_7, d16_8, d16_9,
   d17_extra, d18_1, d18_2, d18_3, d18_4, d18_5, d18_6, d18_7, d18_8, d18_9, d18_10, d18_11,
   d19_1, d19_2, d19_3, d19_4, d21_1, d21_2, d21_3, d21_4, d21_5, d21_6, d21_7)


