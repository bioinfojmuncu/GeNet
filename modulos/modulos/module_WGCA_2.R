# Construccio de la red de gens ,identificacio dels diferents moduls i Construccio del dendograma ####
# Apartat 2.c de la guia de WGNA
# Llibreries i funcions a utilitzar desde altres scripts

#### Variables globales ####
sft_obj <- reactiveVal(NULL)
plot_power_obj <- reactiveVal(NULL)
net_obj <- reactiveVal(NULL)

ui_02 <- function(id){
    ns <- NS(id)  # Namespace para evitar conflictos de identificadores
    
    tagList(
        # Contenedor principal usando box() de Shinydashboard
        box(
            solidHeader = TRUE,
            width = 12,
            collapsed = TRUE,
            status = "gray",
            background = "gray",
            title = "Parameters independence and connectivity",  # Título del contenedor
            # Contenido del contenedor
            tagList(
                
                numericInput(inputId = ns("to"), min = 0, value = 50, label = "Define maximum x value"),
                numericInput(inputId = ns("by"), min = 0, value = 2, label = "Define stepped value (from 1 to 10 is stepped by 1)"),
                selectInput(inputId = ns("network_type"), choices = c("signed", "unsigned", "signed hybrid"), label = "Network type"),
            )),
        
        # Botón para crear el plot de escala de independencia y conectividad media
        actionButton(inputId = ns("create_power_plot"), label = "Independence and connectivity plots"),
        hr(),  # Línea horizontal como separador
        
        box(
            solidHeader = TRUE,
            width = 12,
            collapsed = TRUE,
            status = "gray",
            background = "gray",
            title = "Parameters Topological Overlap Matrix (TOM)",  # Título del contenedor
            # TOM type
            tagList(
                selectInput(inputId = ns("Tom_type"), choices = c("none", "unsigned", "signed", "signed Nowick", "unsigned 2", "signed 2", "signed Nowick 2"), 
                            label = "Type of TOM"),
                # PAM ?
                selectInput(inputId = ns("pam_respects_dendro"), choices = c("TRUE", "FALSE"), label = "PAM dendogram"),
                # NumericInput para seleccionar el nivel de poder
                numericInput(inputId = ns("power"), min = 1, value = 10, label = 'Define power')
            )
        ),
        
        
        # Botón para crear el plot del dendrograma
        actionButton(inputId = ns("create_dendogram_plot"), label = "TOM plot"),
        hr()
        
    )
}


body_02 <- function (id){
    ns <- NS(id)  # Namespace para evitar conflictos de identificadores
    
    tagList(
        # Row fluid para el output del plot de escala de independencia y conectividad media
        fluidRow(
            box(
                collapsible = TRUE,
                collapsed = TRUE,
                title = "Independence and connectivity",
                solidHeader = TRUE,
                status = "navy",
                slickROutput(ns("power_plot")),  # Spinner mientras se carga el plot
                width = 9
            )
        ),
        # Row fluid para el output del plot del dendrograma
        fluidRow(
            box(
                collapsible = TRUE,
                collapsed = TRUE,
                title = "Topological Overlap Matrix",
                solidHeader = TRUE,
                status = "navy",
                plotOutput(ns("dendogram_plot")),  # Spinner mientras se carga el plot
                width = 9
            )
        ),
        # Row fluid para visualizar el power recomendado
        fluidRow(
            box(
                collapsible = TRUE,
                collapsed = FALSE,
                title = "Power recomended (FAQs WGCNA)",
                solidHeader = TRUE,
                status = "navy",
                shinycssloaders::withSpinner(uiOutput(ns("jpg_power"),height = "100%",height = "100%")),  
                width = 9,
                height = 200
            )
        ),
        # Row fluid para visualizar el PDF de WGNA
        fluidRow(
            box(
                collapsible = TRUE,
                collapsed = TRUE,
                title = "WGNA PDF",
                solidHeader = TRUE,
                status = "navy",
                shinycssloaders::withSpinner(uiOutput(ns("pdf_viewer"), width = "100%")),  
                width = 9,
                height = 1500
            )
        )
    )
}

server_02 <- function(id){
    moduleServer(
        id,
        function(input, output, session) {

            #### Cargar variables del módulo module_WGCA_1 ####
            
            genes <- reactive(session$userData$genes())
            genes_filtrats <- reactive(session$userData$genes_filtrats())
            fenotips <- reactive(session$userData$fenotips())
            fenotips_filtrats <- reactive(session$userData$fenotips_filtrats())
            min_size <- reactive(session$userData$min_size())
            dir_plot <- reactive(session$userData$dir_plot())
            button_clear_content <- reactive(session$userData$button_clear_content())
            
            #### Neteja contingut dataset plots ####
            
            observeEvent(button_clear_content(), {
                output$power_plot <- NULL
                output$dendogram_plot <- NULL
            })

            # Actualización de input basado en condiciones

            observe({
                if (!is.null(fenotips()) && !is.null(genes())) {
                    n_samples <- dim(genes_filtrats())[1]
                    power <- NULL
                    if (input$network_type %in% c("unsigned", "singed hybrid")) {
                        if (n_samples < 20) {
                            power <- 10
                        } else if (n_samples >= 20 && n_samples < 30) {
                            power <- 9
                        } else if (n_samples >= 30 && n_samples < 40) {
                            power <- 8
                        } else if (n_samples >= 40 && n_samples < 60) {
                            power <- 7
                        } else if (n_samples > 60) {
                            power <- 6
                        }
                    }

                    if (input$network_type == "signed") {
                        if (n_samples < 20) {
                            power <- 20
                        } else if (n_samples >= 20 && n_samples < 30) {
                            power <- 18
                        } else if (n_samples >= 30 && n_samples < 40) {
                            power <- 16
                        } else if (n_samples >= 40 && n_samples < 60) {
                            power <- 14
                        } else if (n_samples > 60) {
                            power <- 12
                        }
                    }
            
            updateSelectInput(session, "power", selected = power)
                }
            })
            
            # Renderizar el plot de escala de independencia y conectividad media
            
            # Create power plot and connective median ####
            
            observeEvent(input$create_power_plot, {
                req(genes_filtrats())
                withProgress(message = "Creating plot...", {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    
                    incProgress(0.50, detail = "Creating plots...")
                    
                    sft_obj(sft(genes = genes_filtrats(),input_to = input$to,input_by = input$by,input_networkType = input$network_type))
                    
                    incProgress(0.50, detail = "Combining plots...")
                    
                    plot_power_plot <- reactive({
                        plot_power_plot <-  plot_combined(
                            input_power = input$power,
                            sft_obj = sft_obj()[[1]], # Retorna un valor numeric que es el que te recomana la funcio pickSoftThreshold
                            powers_obj = sft_obj()[[2]], # Retorna una taula de valors de powers, a la qual cadascuna se li corresponen una serie de valors
                            dir_plot_path = dir_plot()
                        )
                        # 2 minuts en executar grafica
                        #plot_power_obj <- recordPlot()
                        return(plot_power_plot)
                    })
                    
                    plot_power_obj(plot_power_plot())
                    remove_modal_spinner()
                })
            })
            
            observeEvent(input$create_power_plot, {
                output$power_plot <- renderSlickR({isolate({
                    req(plot_power_obj())
                    slickR(plot_power_obj(),height = 400, width = 700)
                })})
            })
            
            # Creación de la red de genes
            # net <- reactive({
            #     req(genes_filtrats() & input$create_dendogram_plot)
            #     soft_threshold <- sft(genes = genes_filtrats(),
            #                           input_to = input$to,
            #                           input_by = input$by,
            #                           input_networkType = input$network_type)[[1]]
            #     
            #     net <- blockwiseModules(
            #         genes_filtrats(),
            #         nThreads = round(detectCores() -2),
            #         power = input$power,
            #         TOMType = input$Tom_type,
            #         networkType = input$network_type,
            #         pamRespectsDendro = FALSE,
            #         pamStage = TRUE,
            #         minModuleSize = min(20, ncol(genes_filtrats) / 2),
            #         reassignThreshold = 0,
            #         mergeCutHeight = 0.25,
            #         numericLabels = TRUE,
            #         saveTOMs = TRUE,
            #         saveTOMFileBase = "savedTOM",
            #         verbose = 3,
            #         maxBlockSize = 46000
            #     )
            #     
            #     
            #     return(net)
            # })
            
            observeEvent(input$create_dendogram_plot, {
                withProgress(message = "Creating plot...", {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    
                    
                    incProgress(1, detail = "Creating net...")
                    
                     
                         
                         net_obj(blockwiseModules(
                             genes_filtrats(),
                             nThreads = round(detectCores() -2),
                             power = input$power,
                             TOMType = input$Tom_type,
                             networkType = input$network_type,
                             pamRespectsDendro = FALSE,
                             pamStage = TRUE,
                             minModuleSize = min(20, ncol(genes_filtrats()) / 2),
                             reassignThreshold = 0,
                             mergeCutHeight = 0.25,
                             numericLabels = TRUE,
                             saveTOMs = TRUE,
                             saveTOMFileBase = "savedTOM",
                             verbose = 3,
                             maxBlockSize = 46000
                         ))
                         
                         net_plot <- reactive({
                             net_plot <- dendro_plot(net_obj())
                             return(net_plot)
                         })
                    
                        
                        png(filename = file.path(dir_plot(), "dendogram_cluster.png"), width = 1600, height = 700)
                        net_plot()
                        dev.off()
                    
                    remove_modal_spinner()
                })
            })
            
            
            # module_eigengenes <- reactive({
            #     req(genes_filtrats())
            #     req(net())
            #     mergedcolors <- mergedColores(net())
            #     MEs0 <- moduleEigengenes(genes_filtrats(), mergedcolors)$eigengenes
            #     
            #     MEs <- orderMEs(MEs0)
            #     #save(MEs,file = "./objects/modules_MES.RData" )
            #     return(MEs)
            # })
            
           


            # Renderizar el plot del dendrograma de cluster
            observeEvent(input$create_dendogram_plot, {
                output$dendogram_plot <- renderPlot(isolate({
                    img <- png::readPNG(paste0(dir_plot(),"/dendogram_cluster.png"))
                    par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                    rasterImage(img, 0, 0, 1, 1)
                }))
            })

            # Renderizar el PDF en uiOutput
            output$pdf_viewer <- renderUI({
                tags$iframe(
                    style = "width:1090px; height:800px; overflow:auto;",
                    src = "WGNA_pasos_2.pdf"
                )
            })

            # Renderizar la imagen en uiOutput
            output$jpg_power <- renderUI({
                tags$iframe(
                    style = "width:800px; height:170px; overflow:auto;",
                    src = "recommended_power.jpg"
                )
            })
            
            
            # Guardar la estructura del árbol de genes de la red
            geneTree <- reactive({
                req(net_obj())
                geneTree <- geneTree_function(net_obj(), 1)
                return(geneTree)
            })
            
            # Guardar las etiquetas de los módulos de la red
            moduleLabels <- reactive({
                req(net_obj())
                moduleLabels <- getModuleLabels(net_obj())
                return(moduleLabels)
            })

            # Guardar variables finales para módulos futuros
            session$userData$net <- reactive ({
                req(net_obj())
                net <- net_obj()
                return(net)
            })

            session$userData$geneTree <- reactive({
                geneTree <- geneTree()
                return(geneTree)
            })

            session$userData$moduleLabels <- reactive({
                moduleLabels <- moduleLabels()
                return(moduleLabels)
            })

            session$userData$power <- reactive({
                power <-  input$power
                return(power)
            })

            session$userData$network_type <- reactive({
                network_type <-  input$network_type
                return(network_type)
            })

            session$userData$tom_type <- reactive({
                tom_type <-  input$Tom_type
                return(tom_type)
            })

            # session$userData$Module_eigengenes <- reactive({
            #     req(module_eigengenes())
            #     Module_eigengenes <- module_eigengenes()
            #     return(Module_eigengenes)
            # })

            session$userData$by <- reactive({
                by <- input$by
                return(by)
            })

            # session$userData$pam_respects_dendro <- reactive({
            #     pam_respects_dendro <- input$pam_respects_dendro
            #     return(pam_respects_dendro)
            # })
            
        }
    )
}
