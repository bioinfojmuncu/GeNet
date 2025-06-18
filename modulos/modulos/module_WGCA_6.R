
ui_06 <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        # Modificadors ####
        selectizeInput(ns("module_colors"),      
                       label = "Select module to plot network",    
                       choices = colnames(ns("module_colors")),   
                       multiple = TRUE),
        numericInput(ns("threshold_input"), label = 'Define thresfold vertex (0-1)', value = 0.01, min = 0, max = 1, step = 0.01),
        numericInput(ns("size_vertex"), label =  "Define size of vertex", min = 0,value = 5),
        
        # Boto per crear es plot ####
        actionButton(ns("create_igraph"),label = "Generate Igraph plot")
    )
}

body_06 <- function (id){
    ns <- NS(id)
    
    tagList(
        # Plot output cytoscape net ####
        fluidRow(
            box(collapsible = T, collapsed = T,title = "Plot Cytoscape net",solidHeader = T,status = "navy", shinycssloaders::withSpinner(plotOutput(ns("igraph_plot"))),width = 9)
        ),
        # Visualizar pdf pases WGNA ####
        fluidRow(box(collapsible = T, collapsed = T, title = "WGNA PDF", solidHeader = T, status = "navy", shinycssloaders::withSpinner(uiOutput(ns("pdf_viewer"))), width = 9,height = 1500)
        )
    )
}

server_06 <- function(id){
    moduleServer(
        id,
        function(input, output, session) {
            # Cargar variables de los otros scripts ####
            
            module_colors_names <- reactive(session$userData$module_colors_names())
            genes <- reactive(session$userData$genes())
            fenotips <- reactive(session$userData$fenotips())
            genes_filtrats <- reactive(session$userData$genes_filtrats())
            fenotips_filtrats <- reactive(session$userData$fenotips_filtrats())
            MEs <- reactive(session$userData$module_eigengenes())
            moduleColors <- reactive(session$userData$moduleColors())
            geneTree <- reactive(session$userData$geneTree())
            moduleLabels <- reactive(session$userData$moduleLabels())
            net <- reactive(session$userData$net())
            dir_plot <- reactive(session$userData$dir_plot())
            #dir_plot <- directorio
            network_type <- reactive(session$userData$network_type())
            power <- reactive(session$userData$power())
            tom_type <- reactive(session$userData$tom_type())
            button_clear_content <- reactive(session$userData$button_clear_content())
            
            plot_trigger <- reactiveVal(0)
            
            # Neteja contingut dataset plots ####
            
            observeEvent(button_clear_content(), {
                output$igraph_plot <- NULL
            })
            
            
            #Actualizar input colores
            
            observe(
                if(is.null(fenotips()) == F && is.null(genes()) == F){
                    new_choices <- c("ALL", module_colors_names())
                    
                    updateSelectInput(session, "module_colors",
                                      choices = c(new_choices))
                }
            )
            
            
            # Crear net Igraph ####
            net_graph <- reactive({
                # Recalculate topological overlap if needed
                set.seed(1)
                TOM = TOMsimilarityFromExpr(genes_filtrats(),
                                            power = power(),
                                            nThreads = round(detectCores() -2),
                                            TOMType = tom_type(),
                                            networkType = network_type())
                
                
                #TOM = as.matrix(get(load("./savedTOM-block.1.RData")))
                
                TOM = 1 - TOM
                
                
                # Convierte el objeto phylo a un dataframe
                #  df <- as.data.frame(TOM)
                # 
                # write.csv(df,"TOM.csv")
                # Select modules
                if("ALL" %in% input$module_colors){
                    modules <- module_colors_names()
                }else{
                    modules <- c(input$module_colors)
                }
                
                
                
                # Optionally, you can save the dissimilarity matrix for later use
                # Select module probes
                probes <- names(genes_filtrats())
                inModule <- is.finite(match(moduleColors(), modules))
                
                modProbes <- probes[inModule]
                print(length(inModule) == length(genes_filtrats()) )
                # Select the corresponding Topological Overlap
                
                modTOM <- TOM[inModule, inModule]
                dimnames(modTOM) = list(modProbes, modProbes)
                adj <- modTOM
                ## substituir 0.01 per threshold input
                adj[adj > input$threshold_input] <- 1
                adj[adj != 1] <- 0
                network <-  graph.adjacency(adj)
                network <- igraph::simplify(network)
                
                V(network)$color <- moduleColors()[inModule]
                
                # remove unconnected nodes
                
                network <- delete.vertices(network, degree(network)==0)
                igraph <- plot.igraph(network,
                                      layout=layout.fruchterman.reingold(network),
                                      edge = 0.01,
                                      edge.arrow.size = 0.1,
                                      #vertex.label = "",
                                      #vertex.label = labels(),
                                      vertex.label.cex = 0.5,
                                      vertex.size = input$size_vertex)
                igraph 
            })
            
            # Mostrar net Igraph plot ####
            observeEvent(input$create_igraph, {
                
                isolate(
                    output$igraph_plot <- renderPlot(
                        isolate({
                            png(filename = paste0(dir_plot(),"/net_igraph.png"),width = 1600,height = 700)
                            net_graph()
                            dev.off()
                            img <- png::readPNG(paste0(dir_plot(),"/net_igraph.png"))
                            par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                            plot.new()
                            plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                            rasterImage(img, 0, 0, 1, 1)
                        }
                        )
                    )
                )
            })
            
            # Descargar net Igrph plot #### 
            
            output$download_net_plot <- downloadHandler(
                filename = function() {
                    "net_Igraph_plot.png"
                },
                content = function(file) {
                    # Guarda el gráfico en un archivo temporal
                    png(file)
                    print(net_graph())
                    dev.off()
                }
            )
            
            
            # Renderizar el PDF en el uiOutput ####
            output$pdf_viewer <- renderUI({
                tags$iframe(
                    style = "width:1090px; height:800px; overflow:auto;",
                    src = "WGNA_pasos_6.pdf")
            })
            
            
        }
    )
}