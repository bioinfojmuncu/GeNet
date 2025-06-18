# Identificació de gens amb una mayor significancia a partir de un fenotip ####
# Llibreries i funcions a utilitzar desde altres scripts
# Variables globals #

module_eigengenes_obj <- reactiveVal(NULL)
moduleTraitCor_obj <- reactiveVal(NULL)
moduleTraitValues_obj <- reactiveVal(NULL)
moduleTrait_obj <- reactiveVal(NULL)

module_colors_names_obj <- reactiveVal(NULL)
point_plot <- reactiveVal(NULL)

ui_03 <- function(id){
    ns <- NS(id)
    
    tagList(
        
        # Modificadors de les variables dels moduls del heatmap####
        
        box(solidHeader = T, width = 12, collapsed = T,
            status =  "gray",
            background = "gray",title = "Module-trait relationship parameters",
            #selectInput(inputId = ns("module_traits"), label = "Selecciona els moduls que vols visualitzar", choices = ns("module_traits"), multiple = T)
            pickerInput(
                inputId = ns("module_traits"),
                label = "Select module to plot",
                choices =  ns("module_traits"),  # Puedes especificar las opciones que necesites
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,  # Habilita las opciones de seleccionar todo / eliminar todo
                    `live-search` = TRUE   # Habilita búsqueda en la lista de opciones
                )
            )
        ),
        
        actionButton(inputId = ns("create_module_trait"), label = "Module-trait relationship plot"),
        hr(),
        
        # Modificadors de les variables point plot ####
        
        box(solidHeader = T, width = 12, collapsed = T,
            status =  "gray",
            background = "gray",title = "Module Membership vs Gene significance parameters",
            selectInput(inputId = ns("fenotip_columna"), label = "Select phenotype", choices = colnames(ns("fenotip_columna"))),
            checkboxInput(inputId = ns("color_point"), label = "Force black colour to points"),
            selectInput(inputId = ns("module_colors"), label = "Select module", choices =  colnames(ns("module_colors")))),
        
        # Botons per crear es plots ####
        
        
        actionButton(inputId = ns("create_point_plot"), label = "Module Membership vs Gene significance"),
        hr()
    )
}

body_03 <- function (id){
    ns <- NS(id)
    
    tagList(
        # Plot output module trait ####
        fluidRow(
            box(collapsible = T, collapsed = T,title = "Plot module trait",solidHeader = T,status = "navy", plotOutput(ns("module_trait_plot")),width = 9)
        ),
        # Plot output point plot ####
        fluidRow(
            box(collapsible = T, collapsed = T,title = "Plot point",solidHeader = T,status = "navy", plotOutput(ns("point_plot")),width = 9)
        ),
        # Visualizar pdf pases WGNA ####
        fluidRow(box(collapsible = T, 
                     collapsed = T,
                     title = "WGNA PDF", 
                     solidHeader = T, 
                     status = "navy",  
                     shinycssloaders::withSpinner(uiOutput(ns("pdf_viewer"))),
                     width = 9,
                     height = 1500)
        )
    )
}

server_03 <- function(id){
    moduleServer(
        id,
        function(input, output, session) {

            
            fenotips <- reactive(session$userData$fenotips())
            
            genes <- reactive(session$userData$genes())
            
            genes_filtrats <- reactive(session$userData$genes_filtrats())
            
            userFile_fenotips <- reactive(session$userData$userFile_fenotips())
            
            
            fenotips_filtrats <- reactive(session$userData$fenotips_filtrats())
            
            net <- reactive(session$userData$net())
            
           # module_eigengenes <- reactive(session$userData$module_eigengenes())
            
            dir_plot <- reactive(session$userData$dir_plot())
            
            button_clear_content <- reactive(session$userData$button_clear_content())
            
            # Neteja contingut dataset plots ####
            
            observeEvent(button_clear_content(), {
                
                output$module_trait_plot <- NULL
                output$point_plot <- NULL
            })
            
            # Moduls a mostrar ####
   

                 module_eigengenes <- reactive({
                     #req(genes_filtrats())
                     mergedcolors <- mergedColores(net())
                     MEs0 <- moduleEigengenes(genes_filtrats(), mergedcolors)$eigengenes

                     MEs <- orderMEs(MEs0)
                     #save(MEs,file = "./objects/modules_MES.RData" )
                     return(MEs)
                 })

                 #module_eigengenes_obj(module_eigengenes())
      
      

            
            observe({
                req(module_eigengenes())
                updatePickerInput(session, "module_traits",
                                  choices = names(module_eigengenes()),
                                  selected = names(module_eigengenes()))
            })
            
            # Output module trait ####
        
            observeEvent(input$create_module_trait, {
                
                #withProgress(message = "Creating module trait plot", value = 0, {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    
                    # incProgress(0.25, detail = "Creating modules...")
                    # 
                    # # Ens permet determinar si dues variables tenen una relacio lineal positiva, negativa o si no tenen correlacio
                    
                   # incProgress(0.25, detail = "Pearson Correletion...")
                    
                    #module_eigengenes_obj(module_eigengenes())
                    
                    moduleTraitCor <- reactive({
                        
                        fenotips_filtrats <- fenotips_filtrats()
                        
                        fenotips_filtrats[] <- lapply(fenotips_filtrats, function(x) {
                            if (is.character(x)) {
                                as.numeric(as.factor(x))
                            } else {
                                x
                            }
                        })
                        
                        columnes_seleccionades <- input$module_traits
                        MEs <- module_eigengenes()[,columnes_seleccionades]
                        
                        save(fenotips_filtrats,file = "./objects/modules_fen.RData" )
                        
                        moduleTraitCor <- cor(MEs, fenotips_filtrats, use = "p")
                        return(moduleTraitCor)
                    })
                    
                    moduleTraitCor_obj(moduleTraitCor())
                    
                    #incProgress(0.25, detail = "P-values from Pearson Correletion...")
                    
                    # Obtencio dels p-values de la correlacio de Pearson ####
                    
                    moduleTraitValues <- reactive ({
                        req(genes_filtrats())
                        req(moduleTraitCor_obj())
                        
                        nSamples <- nrow(genes_filtrats())
                        moduleTraitPvalue <- corPvalueStudent(moduleTraitCor_obj(), nSamples)
                        return(moduleTraitPvalue)
                    })
                    
                    moduleTraitValues_obj(moduleTraitValues())
                    
                    #incProgress(0.25, detail = "Creating module Trait plot")
                    
                    module_trait_obj <- reactive({
                        
                        # Se mostraran les seves correlacions i els seus p-values ####
                        
                        textMatrix <-  paste(signif(moduleTraitCor(), 2), "\n(",
                                             signif(moduleTraitValues(), 1), ")", sep = "")
                        
                        columnes_seleccionades <- input$module_traits
                        yLabels <- module_eigengenes()[,columnes_seleccionades, drop = F]
                        ySymbols <- module_eigengenes()[,columnes_seleccionades, drop = F]
                        
                        # Se mostraran els valors de les seves correlacions  ####
                        module_trait_obj <- labeledHeatmap(
                            Matrix = moduleTraitCor(),
                            xLabels = names(fenotips_filtrats()),
                            #yLabels = names(module_eigengenes()),
                            #ySymbols = names(module_eigengenes()),
                            yLabels = names(yLabels),
                            ySymbols = names(ySymbols),
                            colorLabels = FALSE,
                            colors = redWhiteGreen(50),
                            textMatrix = textMatrix,
                            setStdMargins = FALSE,
                            cex.text = 0.5,
                            zlim = c(-1,1),
                            main = paste("Module-trait relationships"),
                            font.lab.x = 1,
                            font.lab.y = 1)
                        
                        
                        module_trait_obj <- recordPlot()
                        
                        return(module_trait_obj)
                    })
                    
                    png(filename = file.path(dir_plot(), "module_trait.png"), width = 1600, height = 700)
                    module_trait_obj()
                    dev.off()
                    
                    moduleTrait_obj(module_trait_obj())
                    remove_modal_spinner()
               # })
            })
            
            
            
            observeEvent(input$create_module_trait,{
                req(moduleTrait_obj())
                output$module_trait_plot <- renderPlot(isolate({
                    tryCatch({
                        img <- png::readPNG(source = file.path(dir_plot(), "module_trait.png"))
                        par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                        plot.new()
                        plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                        rasterImage(img, 0, 0, 1, 1)
                    },
                    error = function(e) {
                        plot.new()
                        print(e)
                        showModal(
                            modalDialog(
                                title =   c("El siguiente error a provocado que no se crease el plot, compruebe los parametros y el dataset"),
                                paste("El siguiente error: ",e),
                                easyClose = TRUE,
                                footer = tagList(
                                    modalButton("Cerrar")
                                )
                            )
                        )
                    }

                    )
                }))
            })
            
            
            # Descargar module trait plot ####
            
            output$download_module_traite_plot <- downloadHandler(
                filename = function() {
                    "module_trait_plot.png"
                },
                content = function(file) {
                    # Guarda el gráfico en un archivo temporal
                    png(file)
                    print(module_trait_obj())
                    dev.off()
                }
            )
            
            
            # Actualitzar input per seleccionar el nom de la  columna dels fenotips ####
            
            observe(
                if(is.null(fenotips()) == F && is.null(genes()) == F){
                    fenotips <- fenotips_filtrats()
                    
                    columnas <- names(fenotips)
                    
                    # Iterar sobre cada columna
                    for (i in seq_along(columnas)) {
                        # Comprobar si la columna nomes conte NA
                        if(sum(is.na(fenotips[[columnas[i]]])) == nrow(fenotips)){
                            fenotips <- subset(fenotips, select = -columnas[i])
                        }
                    }
                    updateSelectInput(session, "fenotip_columna",
                                      choices = colnames(fenotips))
                    
                }
            )
            
            
            module_colors_names <- reactive({
                req(module_eigengenes())
                module_eigengenes <- module_eigengenes()
                modNames <- substring(names(module_eigengenes), 3)
                return(modNames)
            })
            
            observe(
                if(is.null(fenotips()) == F && is.null(genes()) == F && is.null(module_colors_names()) == F){
                    updateSelectInput(session, "module_colors",
                                      choices = module_colors_names())
                }
            )
            
            observeEvent(input$create_point_plot, {
               withProgress(message = "Creating point plot...", value = 0, {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    
                   incProgress(1, detail = "Creating point plot...")
                
                # Point plot object ####
                
                point_plot_obj <- reactive({
                    
                    # Variable que conte la columna selecionada dels fenotips #####
                    
                    fenotips_filtrats <- fenotips_filtrats()
                    fenotips_filtrats[] <- lapply(fenotips_filtrats, function(x) {
                        if (is.character(x)) {
                            as.numeric(as.factor(x))
                        } else {
                            x
                        }
                    })
                    
                    columna <- fenotips_filtrats[,input$fenotip_columna]
                    
                    modNames <- substring(names(module_eigengenes()), 3)
                    
                    
                    geneModuleMembership <- as.data.frame(cor(genes_filtrats(), module_eigengenes(), use = "p"))
                    
                    nSamples <- nrow(genes_filtrats())
                    
                    MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))
                    
                    names(geneModuleMembership) <- paste("MM", modNames, sep="")
                    
                    names(MMPvalue) <- paste("p.MM", modNames, sep="")
                    
                    geneTraitSignificance <- as.data.frame(cor(genes_filtrats(), columna, use = "p"))
                    
                    
                    GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))
                    
                    names(geneTraitSignificance) <- paste("GS.", names(columna), sep="")
                    names(GSPvalue) <- paste("p.GS.", names(columna), sep="")
                    
                    module <- input$module_colors
                    module_plot <- input$module_colors
                    
                    column <- match(module, modNames)
                    
                    moduleColors <- mergedColores(net())
                    moduleGenes =  moduleColors==module
                    
                    if(input$color_point == F){
                        point_plot_obj <- verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                                                             abs(geneTraitSignificance[moduleGenes, 1]),
                                                             xlab = paste("Module Membership in", module, "module"),
                                                             ylab = "Gene significance for body columna",
                                                             main = paste("Module membership vs. gene significance\n"),
                                                             cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module_plot)
                        point_plot_obj <- recordPlot()
                    }
                    
                    if(input$color_point == T){
                        point_plot_obj <- verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                                                             abs(geneTraitSignificance[moduleGenes, 1]),
                                                             xlab = paste("Module Membership in", module, "module"),
                                                             ylab = "Gene significance for body columna",
                                                             main = paste("Module membership vs. gene significance\n"),
                                                             cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = "black")
                        point_plot_obj <- recordPlot()  
                    }
                    
                    return(point_plot_obj)
                })
                
                png(filename = file.path(dir_plot(), "point_plot.png"), width = 1600, height = 700)
                point_plot_obj()
                dev.off()
                
                
                remove_modal_spinner()
                })
            })
            
            observeEvent(input$create_point_plot,{
              output$point_plot <- renderPlot(isolate({
                  img <- png::readPNG(source = file.path(dir_plot(), "point_plot.png"))
                  par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                  plot.new()
                  plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                  rasterImage(img, 0, 0, 1, 1)
              }))
            })

            # Descargar point plot ####
            
            output$download_point_plot <- downloadHandler(
                filename = function() {
                    "point_plot.png"
                },
                content = function(file) {
                    # Guarda el gráfico en un archivo temporal
                    png(file)
                    print(point_plot_obj())
                    dev.off()
                }
            )
            
            # Renderizar el PDF en el uiOutput ####
            output$pdf_viewer <- renderUI({
                tags$iframe(
                    style = "width:1090px; height:800px; overflow:auto;",
                    src = "WGNA_pasos_3.pdf")
            })
            
            
            
            # Guardar variables para proximos modulos ####
            
            session$userData$moduleColors <- reactive ({
                req(net())
                moduleColors <- mergedColores(net())
                return(moduleColors)
            })
            
            session$userData$module_colors_names <- reactive({
                req(module_colors_names())
                module_colors_names <- module_colors_names()
                return(module_colors_names)
            })
            
            }
        
    )
}