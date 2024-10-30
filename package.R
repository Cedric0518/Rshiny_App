packages <- c("stringr", "dplyr","readr","seqinr","dplyr", "ggplot2","FactoMineR","missMDA","factoextra","nFactors",'readr',"GGally","corrplot","lubridate","zoo","shiny","DT")

for (package in packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


