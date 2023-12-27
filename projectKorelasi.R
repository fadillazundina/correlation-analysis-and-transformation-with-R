#import library
library(corrplot)
library(Hmisc)

#load data
data <- read.csv(
  "D:/Semester 5/Analisis Eksplorasi Data/Tugas/whitewine.csv"
  )
View(data)

#korelasi antar variabel
korelasi <- cor(data)
korelasi

#visualisasi
vis <- corrplot(korelasi, method = "color", addCoef.col = "black", 
                tl.col = "black", tl.cex = 0.7, number.cex = 0.7
                )

#hubungan linear antar variabel
pval <- rcorr(as.matrix(data))
pval

#transformasi data
alcohol_Kuadrat <- data$alcohol^2
alcohol_Kuadrat

sulphates_Kuadrat <- data$sulphates^2
sulphates_Kuadrat

alcohol_ln <- log(data$alcohol)
alcohol_ln

sulphates_ln <- log(data$sulphates)
sulphates_ln

alcohol_invers <- (data$alcohol)^-1
alcohol_invers

sulphates_invers <- (data$sulphates)^-1
sulphates_invers

#korelasi transformasi
pval_transkuadrat <- rcorr(as.matrix(alcohol_Kuadrat),
                           as.matrix(sulphates_Kuadrat))$P
pval_transkuadrat

pval_transln <- rcorr(as.matrix(alcohol_ln), 
                      as.matrix(sulphates_ln))$P
pval_transln

pval_transinvers <- rcorr(as.matrix(alcohol_invers), 
                      as.matrix(sulphates_invers))$P
pval_transinvers

korelasi_transinvers <- cor(alcohol_invers, sulphates_invers)
korelasi_transinvers

