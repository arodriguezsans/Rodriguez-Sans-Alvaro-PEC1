## ----setup, include=FALSE-------------------------------------------------------------------------
library(knitr)
library(rmdformats)

## Global options
options(max.print="75")
opts_chunk$set(echo=FALSE,
	             cache=FALSE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)


## -------------------------------------------------------------------------------------------------
# Instalamos las librerías necesarias para esta PEC
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  BiocManager::install()
  BiocManager::install("impute")  
if (!require("httr", quietly = TRUE))
  install.packages("httr")
if (!require("readxl", quietly = TRUE))
  install.packages("readxl")
if (!require("formatR", quietly = TRUE))
  install.packages("formatR")
if (!require("tidyverse", quietly = TRUE))
  install.packages("tidyverse")
if (!require("pheatmap", quietly = TRUE))
  install.packages("pheatmap")


## ----results=FALSE--------------------------------------------------------------------------------
# Cargamos las librerías necesarias
library(httr)
library(readxl)
library(dplyr)

# URL directa del archivo en GitHub
url <- "https://raw.githubusercontent.com/nutrimetabolomics/metaboData/main/Data_Catalog.xlsx"

# Descargamos el archivo con los datasets disponibles temporalmente
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Leemos el archivo .xlsx desde el archivo temporal
data <- read_xlsx(temp_file)

# Eliminamos el archivo temporal (opcional)
unlink(temp_file)


## ----results=TRUE---------------------------------------------------------------------------------
# Mostramos las primeras filas para confirmar
data %>%
    kableExtra::kable(format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(full_width = TRUE) %>% 
    kableExtra::column_spec(1, width = "8em") %>% 
    kableExtra::column_spec(2, width = "3em") %>% 
    kableExtra::column_spec(3, width = "3em") %>% 
    kableExtra::column_spec(4, width = "27em")


## -------------------------------------------------------------------------------------------------
# Seleccionamos y mostramos el dataset objetivo de análisis 
set.seed (123)
seleccion<- sample(1:nrow(data),1)
Desc<- data[seleccion,4]
Dataset <- data[seleccion,1]
Dataset


## -------------------------------------------------------------------------------------------------
# Obtenemos la ruta raw del archivo
url <- "https://raw.githubusercontent.com/nutrimetabolomics/metaboData/main/Datasets/2023-CIMCBTutorial/GastricCancer_NMR.xlsx"

# Pasamos la ruta completa local del proyecto RStudio donde guardaremos
destfile <- "F:\\Cursos\\UOC Master Bio Inf. Est\\M0.157 - UOC - Analisis de datos Omicos\\Reto 1. Las ómicas (25_09_2024) a (6_11_2024)\\PEC1\\Repositorio_git\\Rodriguez-Sans-Alvaro-PEC1\\GastricCancer_NMR.xlsx"

# Descargamos el archivo
response <- GET(url, write_disk(destfile, overwrite = TRUE))


## -------------------------------------------------------------------------------------------------
# Verificamos que se descargó correctamente
if (status_code(response) == 200) {
  print("Archivo descargado correctamente.")
} else {
  print("Error en la descarga.")
}

# Mostramos las primeras filas para confirmar
head(read_xlsx(destfile))


## -------------------------------------------------------------------------------------------------
# Cargamos la librería SummarizedExperiment
library(SummarizedExperiment)

# Cargamos las hojas de excel Data y Peak en dos dataframe
data <- read_excel("GastricCancer_NMR.xlsx", sheet = "Data")
peak <- read_excel("GastricCancer_NMR.xlsx", sheet = "Peak")


## -------------------------------------------------------------------------------------------------
head(data)
class(data)


## -------------------------------------------------------------------------------------------------
head(peak)
class(peak)


## -------------------------------------------------------------------------------------------------
# Configuramos la columna SampleID como nombres de filas
rownames(data) <- data$SampleID
nombres_filas <- rownames(data)

# Extraemos solo las columnas de datos (M1, M2, M3, M4)
assay_data <- as.matrix(data[, grep("^M", names(data))])
rownames(assay_data) <- nombres_filas
assay_data[1:6,1:6]


## -------------------------------------------------------------------------------------------------
# rowData (información de características)
rowData <- data[, c("SampleID","SampleType","Class")]
rownames(rowData) <- rowData$SampleID  
head(rowData)

# colData (información de las muestras)
# Nos aseguramos de que coinciden los nombres de filas con las columnas en assay_data
colData <- peak[, c("Name","Label","Perc_missing","QC_RSD")]
rownames(colData) <- colData$Name
head(colData)


## -------------------------------------------------------------------------------------------------
# Creamos el objeto `SummarizedExperiment`
se <- SummarizedExperiment(
  assays = list(counts = assay_data),
  rowData = rowData,
  colData = colData)

# Mostramos el objeto para verificar que se creó correctamente
se


## -------------------------------------------------------------------------------------------------
# Añadimos metadatos a rowData (información sobre los "genes")
#rowData(se)$SampleType <- data[, c("SampleType")]
#rowData(se)$Class <- data[, c("Class")]

# Añadimos metadatos a colData (información sobre las muestras)
#colData(se)$Label <- peak[, c("Label")]
#colData(se)$Perc_missing <- peak[, c("Perc_missing")]
#colData(se)$QC_RSD <- peak[, c("QC_RSD")]

# Vemos los metadatos añadidos
head(colData(se))
head(rowData(se))


## -------------------------------------------------------------------------------------------------
# Creamos una lista para los metadatos del experimento
metadata_experimento <- list(
  Project_ID="PR000699",
  Project_DOI="doi: 10.21228/M8B10B",
  Project_Title="1H-NMR urinary metabolomic profiling for diagnosis of gastric cancer",
  Institute="University of Alberta",
  Autor="Broadhurst,	David",
  Address="270 Joondalup Drive, Joondalup, WA 6027, AUSTRALIA",
  Email="d.broadhurst@ecu.edu.au",
  Project_Summary="Background: Metabolomics has shown promise in gastric cancer (GC) detection. This research sought to identify whether GC has a unique urinary metabolomic profile compared with benign gastric disease (BN) and healthy (HE) patients. Methods: Urine from 43 GC, 40 BN, and 40 matched HE patients was analysed using 1H nuclear magnetic resonance (1H-NMR) spectroscopy, generating 77 reproducible metabolites (QC-RSD <25%). Univariate and multivariate (MVA) statistics were employed. A parsimonious biomarker profile of GC vs HE was investigated using LASSO regularised logistic regression (LASSO-LR). Model performance was assessed using Receiver Operating Characteristic (ROC) curves. Results: GC displayed a clear discriminatory biomarker profile; the BN profile overlapped with GC and HE. LASSO-LR identified three discriminatory metabolites: 2-hydroxyisobutyrate, 3-indoxylsulfate, and alanine, which produced a discriminatory model with an area under the ROC of 0.95. Conclusions: GC patients have a distinct urinary metabolite profile. This study shows clinical potential for metabolic profiling for early GC diagnosis.",
  Data_Sample="Las columnas M1...M149 describen las concentraciones de metabolitos.",
  Data_SampleType="La columna SampleType indica si la muestra era un control de calidad combinado o  una muestra de estudio.",
  Data_Class="La clase de columna Class indica el resultado clínico observado para ese individuo: GC = cáncer gástrico, BN = tumor benigno, HE = control sano.",
  Peak_Name="Name es el nombre de la columna correspondiente a este metabolito.",
  Peak_Label="Label proporciona un nombre único para el metabolito (o un identificador uNNN).",
  Peak_Perc_missing="Perc_missing indica qué porcentaje de muestras no contienen una medición para este metabolito (datos faltantes).",
  Peak_QC_RSD="QC_RSD es una puntuación de calidad que representa la variación en las mediciones de este metabolito en todas las muestras.")

# Asignamos los metadatos del experimento al objeto SummarizedExperiment
metadata(se) <- metadata_experimento

# Formateamos los metadatos para que sean visible como tabla markdown
infoDF <- data.frame(matrix(rep(NA, 2 * length(metadata(se))), ncol = 2))
for (i in 1:length(metadata(se))) {
    infoDF[i, 1] = names(metadata(se))[i]
    infoDF[i, 2] = metadata(se)[i]
}
colnames(infoDF) = c("Campo", "Descripción")

# Cargamos la librería necesaria
library(dplyr)

infoDF %>%
    kableExtra::kable(infoDF, format = "latex", booktabs = TRUE) %>%
    kableExtra::kable_styling(full_width = TRUE) %>% 
    kableExtra::column_spec(1, width = "10em")


## -------------------------------------------------------------------------------------------------
# Guardamos el objeto
save(se, file = "GastricCancer_NMR.Rda")

# Verificamos si se creó el archivo
if (file.exists("F:\\Cursos\\UOC Master Bio Inf. Est\\M0.157 - UOC - Analisis de datos Omicos\\Reto 1. Las ómicas (25_09_2024) a (6_11_2024)\\PEC1\\Repositorio_git\\Rodriguez-Sans-Alvaro-PEC1\\GastricCancer_NMR.Rda")) {
  print("El archivo se ha creado correctamente.")
} else {
  print("Ha ocurrido un error al guardar el archivo.")
}


## -------------------------------------------------------------------------------------------------
# Cargamos el archivo y verificamos el contenido
load("F:\\Cursos\\UOC Master Bio Inf. Est\\M0.157 - UOC - Analisis de datos Omicos\\Reto 1. Las ómicas (25_09_2024) a (6_11_2024)\\PEC1\\Repositorio_git\\Rodriguez-Sans-Alvaro-PEC1\\GastricCancer_NMR.Rda")
head(se)
str(se)


## -------------------------------------------------------------------------------------------------
dim(assay_data) 
length(assay_data)


## -------------------------------------------------------------------------------------------------
sum(is.na(assay_data))


## -------------------------------------------------------------------------------------------------
length(assay_data[!is.na(assay_data)])


## -------------------------------------------------------------------------------------------------
# Extraemos los datos datos relevantes
rsd <- peak$QC_RSD
percMiss <- peak$Perc_missing

# Filtramos por las condiciones dadas
peak_Clean <- peak[(rsd < 20) & (percMiss < 10), ]

# Mostramos cuantos datos limpios nos quedan
print(paste0("Numero de 'peaks' de interes: ", nrow(peak_Clean)))
peak_Clean[,2]


## -------------------------------------------------------------------------------------------------
# Obtenemos los nombres de las muestras a mantener
samples_a_mantener <- peak_Clean$Name

# Creamos un vector lógico para indicar qué columnas mantener en se
keep <- colnames(se) %in% samples_a_mantener

# Creamos un subconjunto el SummarizedExperiment
se_filtrado <- se[, keep]

# Accedemos a assay
dim(assay(se_filtrado))  # Devuelve una matriz con los datos de expresión
assay(se_filtrado)[1:6,1:6]
 
# Accedemos a rowData
dim(rowData(se_filtrado))
head(rowData(se_filtrado))  # Devuelve un DataFrame con información sobre los genes

# Accedemos a colData
dim(colData(se_filtrado))
head(colData(se_filtrado))  # Devuelve un DataFrame con información sobre las muestras


## -------------------------------------------------------------------------------------------------
boxplot((assay(se_filtrado)), 
        main="Exp. génica por muestra", 
        xlab="Muestra", ylab="Expresión",
        las = 2, col = "lightblue")


## -------------------------------------------------------------------------------------------------
# Histograma 
par(mfrow = c(1, 3))
hist(log(assay(se_filtrado)),
     breaks=50, 
     main="Valores de expresión", 
     xlab="Expresión", ylab="Frecuencia")

hist(log(colData(se_filtrado)$Perc_missing), 
     breaks=10,
     main="Perc_missing entre muestras", 
     xlab="Perc_missing", ylab="Frecuencia")

hist(log(colData(se_filtrado)$QC_RSD),
     breaks=10,
     main="QC_RSD entre muestras", 
     xlab="QC_RSD", ylab="Frecuencia")


## -------------------------------------------------------------------------------------------------
# Boxplot 
par(mfrow = c(1, 3))
boxplot(log(assay(se_filtrado)), 
        main="Exp. génica por muestra", 
        xlab="Muestra", ylab="Expresión",
        las = 2, col = "lightblue")

boxplot(log(assay(se_filtrado)) ~ rowData(se_filtrado)$Class,
        main="Exp. por Clase", 
        xlab="Clase", ylab="Expresión",
        las = 1, col = "lightblue")

boxplot(t(log(assay(se_filtrado))) ~ colData(se_filtrado)$Label, 
        main="Exp. por Metabolito (Label)", 
        xlab="Metabolito", ylab="Expresión",
        las = 2, col = "lightblue")


## -------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
boxplot((colData(se_filtrado)$Perc_missing), 
        main="Perc_missing entre muestras", 
        ylab="Perc_missing",
        las = 2, col = "lightblue")

boxplot((colData(se_filtrado)$QC_RSD), 
        main="QC_RSD entre muestras", 
        ylab="QC_RSD",
        las = 2, col = "lightblue")



## -------------------------------------------------------------------------------------------------
# # Cargamos la librería necesaria
# Heatmap
library(pheatmap)
pheatmap(log(assay(se_filtrado)))


## -------------------------------------------------------------------------------------------------
# # Cargamos las librerías necesarias
library(ggplot2)
library(impute)

# Extraemos la matriz de expresión y se transpone para tener muestras en filas
expr_data <- t(assay(se_filtrado))

# Imputamos valores faltantes con el paquete impute
expr_data <- impute.knn(as.matrix(assay(se_filtrado)))$data

# Realizamos el  PCA
pca <- prcomp(expr_data, center = TRUE, scale. = TRUE)

# Convertimos los resultados a un dataframe para mostrar en ggplot2
pca_df <- as.data.frame(pca$x)
pca_df$Class <- rowData(se_filtrado)$Class  

# Calculamos el porcentaje de varianza explicada
var_explicada <- (pca$sdev^2) / sum(pca$sdev^2) * 100
var_explicada_df <- round(var_explicada[1:2], 2)  # Redondeamos los primeros dos componentes principales

# Gráfico del PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 3) +
  labs(title = "PCA de Expresión Génica", 
       x = paste0("PC1 (", var_explicada_df[1], "% de varianza)"),
       y = paste0("PC2 (", var_explicada_df[2], "% de varianza)")) +
 theme_minimal()

# Mostramos la varianza explicada
paste0("PC1 (", var_explicada_df[1], "% de la varianza) - PC2 (", var_explicada_df[2], "% de la varianza)")
summary(pca)


## -------------------------------------------------------------------------------------------------
# Calculamos la matriz de distancias
dist_matrix <- dist(t(log(assay(se_filtrado))), method = "euclidean")

# Realizamos el agrupamiento jerárquico
hc <- hclust(dist_matrix, method = "complete")

# Visualizamos el dendrograma
plot(hc, main = "Dendrograma de Clustering Jerárquico")


## ----echo=FALSE, file="ADO-PEC1-Res.R", eval=FALSE------------------------------------------------


