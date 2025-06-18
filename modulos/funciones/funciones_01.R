# Módulo de opciones para la lectura del CSV en la interfaz de usuario
options_csv_ui <- function(id_quote, elementos_quote = c("Double quote" = "\"", "Single quote" = "'"),
                           id_sep, elementos_sep = c("," =",", ";" =";"),
                           id_dec, elementos_dec = c("."=".", ","=",")
) {
  tagList(
    radioButtons(id_quote, "Quote", elementos_quote),  # Botones de radio para seleccionar tipo de comillas
    radioButtons(id_sep, "Sep", elementos_sep),        # Botones de radio para seleccionar separador
    radioButtons(id_dec, "Dec", elementos_dec)         # Botones de radio para seleccionar separador decimal
  )
}

# Módulo para validar el archivo CSV introducido en el fileInput
validate_csv <- function(file_introduced, message = FALSE) {
  shiny::validate(need(file_introduced, message = message))  # Validar la existencia del archivo
  file_introduced  # Devolver el archivo introducido
}

# Módulo para validar genes en un dataset
validate_genes <- function(genes) {
  gsg = goodSamplesGenes(genes, verbose = 3)  # Validar la calidad de muestras y genes
  
  if (!gsg$allOK) {
    # Si no todos los genes y muestras son válidos, eliminar los no válidos
    if (sum(!gsg$goodGenes) > 0) 
      printFlush(paste("Removing genes:", paste(colnames(genes)[!gsg$goodGenes], collapse = ", ")))
    if (sum(!gsg$goodSamples) > 0) 
      printFlush(paste("Removing samples:", paste(rownames(genes)[!gsg$goodSamples], collapse = ", ")))
    
    genes_verificados = genes[gsg$goodSamples, gsg$goodGenes]  # Genes válidos después de la validación
    return(genes_verificados)
  } else {
    return(genes)  # Devolver genes originales si todos son válidos
  }
}

# Función para generar un dendrograma de clustering de muestras
cluster <- function(genes_validats, delimiter, minsize,dir_plot) {
    
  sampleTree <- hclust(dist(genes_validats), method = "average")  # Crear dendrograma de clustering
  
  # Plot del dendrograma con título y configuraciones de tamaño de texto
  par(cex = 0.6)
  par(mar = c(0,4,2,0))
  plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5, 
       cex.axis = 1.5, cex.main = 2)
  
  abline(h = delimiter, col = "red")  # Dibujar línea de delimitación en el dendrograma
  
  clust = cutreeStatic(sampleTree, cutHeight = delimiter, minSize = minsize)  # Realizar el clustering

}

# Función para seleccionar muestras de un dendrograma de clustering
samples_keep <- function(genes_validats, delimiter, minsize, number_clust) {
    
  sampleTree <- hclust(dist(genes_validats), method = "average")  # Crear dendrograma de clustering
  clust = cutreeStatic(sampleTree, cutHeight = delimiter, minSize = minsize)  # Realizar el clustering
  
  keepSamples = (clust == number_clust)  # Identificar muestras del cluster deseado
  datExpr = genes_validats[keepSamples, ]  # Seleccionar las muestras del dataset original
  
  return(datExpr)  # Devolver las muestras seleccionadas
}


# Función para generar un dendrograma de clustering combinado con fenotipos
cluster_combined <- function(genes, fenotips, modules) {

    fenotips <- fenotips[,modules,drop =F]
    
    fenotips <- as.data.frame(fenotips)
    
    fenotips <- fenotips[rownames(genes), drop = F,]  # Alinear fenotipos con genes
    
    fenotips <- as.data.frame(fenotips)
    
    #fenotips <- fenotips[, colSums(is.na(fenotips)) < nrow(fenotips)]
    
    fenotips[] <- lapply(fenotips, function(x) {
        if(is.character(x)) {
            return(as.numeric(factor(x)))
        } else {
            return(as.numeric(x))
        }
    })
    
    sampleTree2 = hclust(dist(genes), method = "average")  # Re-clustering de muestras
    
    traitColors = numbers2colors(fenotips, signed = FALSE)  # Convertir fenotipos en colores
    
    # Plot del dendrograma de clustering con heatmap de fenotipos y etiquetas
    plotDendroAndColors(sampleTree2, traitColors,
                        groupLabels = names(fenotips)[1:5], 
                        main = "Sample dendrogram and trait heatmap")
}
