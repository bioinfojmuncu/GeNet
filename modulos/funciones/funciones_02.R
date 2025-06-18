
# Función para realizar el análisis de topología de red
sft <- function(genes, input_to, input_by, input_networkType) {
  powers = c(c(1:10), seq(from = 10 + input_by, to=input_to, by=input_by))  # Secuencia de potencias
  sft = pickSoftThreshold(genes, 
                          powerVector = powers,  # Vector de potencias
                          networkType = input_networkType,  # Tipo de red (unsigned, signed, signed hybrid)
                          verbose = 5)  # Nivel de detalle en la salida
  return(list(sft, powers))  # Devuelve lista con resultados y potencias evaluadas
}

#Función para plotear la combinación de independencia de escala y conectividad media
plot_combined <- function(input_power, sft_obj, powers_obj, dir_plot_path) {

  # Plot del índice de ajuste de topología libre de escala en función del poder de soft-thresholding
  png(paste0(dir_plot_path,"/power_plot_1.png"),width = 1600,height = 700)
    
  plot(sft_obj$fitIndices[,1], -sign(sft_obj$fitIndices[,3])*sft_obj$fitIndices[,2],
       xlab = "Soft Threshold (power)", ylab = "Scale Free Topology Model Fit,signed R^2", type = "n",
       main = paste("Scale independence"))

  text(sft_obj$fitIndices[,1], -sign(sft_obj$fitIndices[,3])*sft_obj$fitIndices[,2],
       labels = powers_obj, col = "red")

  # Línea roja para el valor de R^2 especificado por input_power
  value <- NULL
  if (sum(sft_obj$fitIndices$Power == input_power) == 0) {
    value <- input_power - (input_by - 1)
    if (sum(sft_obj$fitIndices$Power == input_power) == 1) {
      #print("FOUND")
    } else {
     # print("NOT FOUND")
    }
    #print(value)
  } else if (sum(sft_obj$fitIndices$Power == input_power) == 1) {
    value <- input_power
    #print(value)
  }

  valor_seleccionado <- sft_obj$fitIndices %>%
    filter(Power == value) %>%
    pull(SFT.R.sq)
  abline(h = valor_seleccionado, col = "red")
  
  dev.off()
  
  png(paste0(dir_plot_path,"/power_plot_2.png"),width = 1600,height = 700)
  
  # Mean connectivity en función del poder de soft-thresholding
  plot(sft_obj$fitIndices[,1], sft_obj$fitIndices[,5],
       xlab = "Soft Threshold (power)", ylab = "Mean Connectivity", type = "n",
       main = paste("Mean connectivity"))
  text(sft_obj$fitIndices[,1], sft_obj$fitIndices[,5], labels = powers_obj, col = "red")
  dev.off()
  
  imgs <- list.files(paste0(dir_plot_path), pattern="power_plot", full.names = TRUE)

  return(imgs)
}

# Función para combinar etiquetas a colores para graficar
mergedColores <- function(net) {
  mergedColores <- labels2colors(net$colors)
  return(mergedColores)
}

# Función para obtener etiquetas de módulos de red
getModuleLabels <- function(net) {
  moduleLabels <- net$colors
  return(moduleLabels)
}

# Función para obtener el árbol de genes de una red
geneTree_function <- function(net, n_plot) {
  geneTree = net$dendrograms[[n_plot]]
  return(geneTree)
}

# Función para plotear un dendrograma de red
dendro_plot <- function(net) {
  mergedColors <- mergedColores(net)  # Obtener colores de etiquetas
  
  # Plotear dendrograma con colores de módulos
  plot <- plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                              "Module colors",
                              dendroLabels = FALSE, hang = 0.03,
                              addGuide = TRUE, guideHang = 0.05)
  
  return(plot)
}
