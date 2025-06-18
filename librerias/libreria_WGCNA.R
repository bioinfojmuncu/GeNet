
library(grid)
library(shiny)
source("./modulos/modulos/module_WGCA_1.R")  
source("./modulos/modulos/module_WGCA_2.R")  
source("./modulos/modulos/module_WGCA_3.R")  
source("./modulos/modulos/module_WGCA_6.R")  
source("./modulos/modulos/Download_informe.R")
source("./modulos/funciones/funciones_01.R")
source("./modulos/funciones/funciones_02.R")
source("./modulos/funciones/funciones_generales.R")

library(gridExtra)  
library(slickR)     
library(ggplot2)    
library(igraph)     

library(knitr)
library(DT)   

library(shiny)     
library(bs4Dash)   
library(shinycssloaders)  # Agrega animaciones de carga en componentes de Shiny
library(shinyjs)   # Permite interacciones avanzadas con elementos HTML y JavaScript
library(shinyWidgets)  # Proporciona widgets avanzados para mejorar la UI en Shiny
library(shinymanager)  # Permite gestionar autenticación y control de acceso en Shiny
library(shinybusy)


library(WGCNA)   
library(dplyr)   
library(parallel)

library(DBI)      
library(RSQLite)  
