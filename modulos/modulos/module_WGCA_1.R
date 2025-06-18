# Data input cleaning apartat 1 de la guia de WGNA #####

print(paste("Directori modul 1",getwd()))

# Variables globals ####
dataset_expressio_diferencial <- reactiveVal(NULL)
dataset_fenotips <- reactiveVal(NULL)

result_cluster <- reactiveVal(NULL)
genes <- reactiveVal(NULL)
cluster_obj <- reactiveVal(NULL)

samples_keep_obj <- reactiveVal(NULL)
cluster_combined_obj <- reactiveVal(NULL)

ui_01 <- function(id_module) {
    use_busy_spinner(spin = "trinity-rings")
    ns <- NS(id_module)
    
    tagList(
        ### Box expressió diferencial ####
        box(solidHeader = T, width = 12, collapsed = T,
            status = "gray", background = "gray",
            fileInput(ns("file_expressio_diferencial"), label = "Upload  csv or Excel"),
            options_csv_ui(id_quote = ns("quote_genes"), id_sep = ns("sep_genes"), id_dec = ns("dec_genes")),
            numericInput(inputId = ns("sheet_number"), label = "Sheet Excel", min = 0, value = 1),
            numericInput(inputId = ns("number_visualize_columns"), label = "Number of columns to view", min = 1, value = 10),
            numericInput(ns("row_names_genes"), label="Select column with rownames", value = 0, step = 1, min = 0),
            checkboxInput(ns("input_header_genes"),label = 'Check header names'),
            title = "Upload expression file"),
        
        ### Box phenotip ####
        box(solidHeader = T, width = 12, collapsed = T,
            status = "gray", background = "gray",
            fileInput(ns("file_fenotips"), label = "Upload  csv or Excel"),
            options_csv_ui(id_quote = ns("quote_fenotips"), id_sep = ns("sep_fenotips"), id_dec = ns("dec_fenotips")),
            numericInput(inputId = ns("sheet_number_feno"), label = "Sheet Excel", min = 0, value = 1),
            numericInput(inputId = ns("number_visualize_columns_fenotips"), label = "Number of columns to view", min = 1, value = 10),
            numericInput(ns("row_names_feno"),  label="Select column with rownames", value = 0, step = 1, min = 0),
            checkboxInput(ns("input_header_feno"),label = 'Check header names'),
            title = "Upload phenotype"),
        
        # Data exemples
        actionButton(ns("loadExampleButton"), "Load example"),
        
        actionButton(ns("clearButton"), "Clean data"),
        hr(),
        
        # Paràmetres ####
        box(solidHeader = T, width = 12, collapsed = T,
            status = "gray", background = "gray",
            numericInput(ns("delimiter_cut_plot"), label = "Cluster cutoff", min = 0, value = 7),
            numericInput(ns("min_size"), label = 'Minimum length to be considered a cluster', value = 1, min = 1, step = 1),
            numericInput(ns("number_clust"), label = "Number of cluster", value = 1, min = 1, step = 1),
            title = "Cluster parameters"),
        
        actionButton(inputId = ns("create_plot"), label = "Create plot cluster"),
        hr(),
        # Modificadores Dendro plot
        box(solidHeader = T, width = 12, collapsed = T,
            status = "gray", background = "gray",
            selectInput(inputId = ns("modules"), label = "Select traits to view", choices = ns("modules"), multiple = T),
            title = "Dendogram parameters"),
        
        
        
        # Botones para ejecutar plots
        
        
        actionButton(inputId = ns("create_dendro_plot"), label = "Create plot dendrogram"),
        hr()
    )
}


body_01<- function(id) {
    
    ns <- NS(id)
    
    # Taula introduida expressio diferencial ####
    tagList(
        fluidRow(
            box(collapsible = T, collapsed = T,title = "Expression data",solidHeader = T,status = "navy",dataTableOutput(ns("table_introduced_expressio_diferencial")),width = 9)
        ),
        # Taula introduida fenotips ####
        fluidRow(
            box(collapsible = T, collapsed = T,title = "Phenotype data",solidHeader = T,status = "navy", DT::dataTableOutput(ns("table_introduced_fenotips")),width = 9)
        ),
        # Plot cluster ####
        fluidRow(box(collapsible = T, collapsed = T,title = "Plot cluster",solidHeader = T,status = "navy",
                     #shinycssloaders::withSpinner(
                     plotOutput(outputId = ns("cluster_plot")
                                     ), width =9)
                     #,width = 9)
        ),
        # Dendro plot ####
        fluidRow(box(collapsible = T, collapsed = T,title = "Dendrogram plot",solidHeader = T,status = "navy",
                     plotOutput(ns("dendro_plot")),width = 9)
        )
        # fluidRow(box(collapsible = T, collapsed = T,title = "Dendrogram plot",solidHeader = T,status = "navy", 
        #              shinycssloaders::withSpinner(imageOutput(ns("dendro_plot"))),width = 9)
        # )
        ,
        # Visualizar pdf pases WGNA ####
        fluidRow(box(collapsible = T,
                     collapsed = T, 
                     title = "WGNA PDF", 
                     solidHeader = T, 
                     status = "navy",
                     shinycssloaders::withSpinner(uiOutput(ns("pdf_viewer"))),
                     width = 9,height = 1500)
        )
    )
}


server_01 <- function(id,directori) {
    moduleServer(
        id,
        function(input, output, session) {
            
            # Validar archivos CSV de expresión diferencial y fenotipos
            userFile <- reactive({
                validate_csv(input$file_expressio_diferencial)
            })
            
            userFile_fenotips <- reactive({
                validate_csv(input$file_fenotips)
            })
            
            # Crear carpetes per guardar plots i altres objectes
            
            #directori <- getwd()
            unlink(directori, recursive = T)
            dir.create(paste0(directori))
            
            dir <- paste0(directori,"/WGCNA")
            unlink(dir, recursive = T)
            dir.create(path = paste0(dir))
            
            
            dir_plot <- paste0(directori, "/WGCNA/plots")
            unlink(dir_plot, recursive = T)
            dir.create(path = paste0(dir_plot))
            
            # Rowname seleccionat per se primera taula
            
            rownames_selected_genes <- reactive({
                if (input$row_names_genes == 0) {
                    return(NULL)
                } else {
                    return(input$row_names_genes)  # NO uses as.numeric aquí si las rownames son texto
                }
            })
            
            # Lletgir dataset expressio diferencial ####
            
            #observeEvent(userFile(), {
            observe(
                if(is.null(userFile()$datapath)==F) {
                    tryCatch({
                        dataset_expressio_diferencial(
                            if(grepl(".csv$", input$file_expressio_diferencial$datapath) == T){
                                read.csv(file = userFile()$datapath,
                                         quote = input$quote_genes,
                                         sep = input$sep_genes,
                                         dec = input$dec_genes,
                                         comment.char = "#",
                                         stringsAsFactors = F,
                                         row.names = rownames_selected_genes(),
                                         check.names = input$input_header_genes)}
                            else if(grepl(".xlsx$", input$file_expressio_diferencial$datapath) == T || grepl(".xls$", input$file_expressio_diferencial$datapath) == T){
                                readxl::read_excel(path = userFile()$datapath, sheet = input$sheet_number)
                            }else{
                                
                            }
                            
                        )}, error = function(e) {
                            print(e)
                            
                            dataset_expressio_diferencial(data.frame(Error = c(paste(e)),
                                                                     Solution = c("Change the parameters of the options csv(quote, sep, dec)")))
                            
                            
                        })
                    
                }
            )
           # })
            
            # Rowname seleccionat per se primera taula
            
            rownames_selected_feno <- reactive({
                if (input$row_names_feno == 0) {
                    return(NULL)
                } else {
                    return(input$row_names_feno)  # NO uses as.numeric aquí si las rownames son texto
                }
            })
            
            
            # Lletgir dataset fenotips ####
            
            #observeEvent(userFile_fenotips(), {
            observe(
                if(is.null(userFile_fenotips()$datapath)==F) {
                    
                    tryCatch({
                        dataset_fenotips(
                            if(grepl(".csv$", input$file_fenotips$datapath) == T){
                                read.csv(file = userFile_fenotips()$datapath,
                                         quote = input$quote_fenotips,
                                         sep= input$sep_fenotips,
                                         dec = input$dec_fenotips,
                                         comment.char = "#",
                                         stringsAsFactors = F,
                                         row.names = rownames_selected_feno(),
                                         check.names = input$input_header_feno)}
                            else if(grepl(".xlsx$", input$file_fenotips$datapath) == T || grepl(".xls$", input$file_fenotips$datapath) == T){
                                readxl::read_excel(path = userFile_fenotips()$datapath, sheet = input$sheet_number_feno)
                            }else{
                                
                            }
                            
                        ) }, error = function(e) {
                            print(e)
                            
                            dataset_fenotips(data.frame(Error = c(paste(e)),
                                                        Solution = c("Change the parameters of the options csv(quote, sep, dec)")))
                            
                            
                        })
                    
                    
                }
            )
               # })
            
            # Carregar el dataset de exemple ####
            
            observeEvent(input$loadExampleButton, {
                
                ## Expressio diferencial ####
                
                shinyjs::runjs('Shiny.setInputValue("file_expressio_diferencial", null);')
                
                dataset_expressio_diferencial(NULL)
                
                dataset_expressio_diferencial(read.csv(file = "./taules/matriu_expressio.csv",     
                                                       quote = "\"",
                                                       row.names = 1))
                
                
                # Fenotips ####
                
                shinyjs::runjs('Shiny.setInputValue("file_fenotips", null);')
                
                dataset_fenotips(NULL)
                
                dataset_fenotips(read.csv(file = "./taules/fenotips3.csv",                                      
                                          comment.char = "#",
                                          stringsAsFactors = F,
                                          row.names = 1))
                
            })
            
            # Neteja contingut dataset carregat ####
            print(paste("dir_feno",getwd()))
            observeEvent(input$clearButton, {

                ## Expressio diferencial ####
                shinyjs::runjs('Shiny.setInputValue("file_expressio_diferencial", null);')

                dataset_expressio_diferencial(NULL)
                reset("file_expressio_diferencial")

                # Fenotips ####

                shinyjs::runjs('Shiny.setInputValue("file_fenotips", null);')

                dataset_fenotips(NULL)
                reset("file_fenotips")
                
                # WGCNA 1 plots#
                
                output$cluster_plot <- NULL
                output$dendro_plot <- NULL
            })

            # Actualizar input moduls a selecionar

            observe(
                updateSelectInput(session, "modules",
                                  choices = names(dataset_fenotips()),
                                  selected = names(dataset_fenotips())[1])
            )

            # Visualització taules introduides ####
            # Para evitar que se escojan mas columnas de las que hay (para la matriz de expression)
            observeEvent(userFile(), {
                if(is.null(dataset_expressio_diferencial()) == F){
                    if(input$number_visualize_columns > length(colnames(dataset_expressio_diferencial()))){
                        updateNumericInput(session, "number_visualize_columns", value = length(colnames(dataset_expressio_diferencial())))
                    }}
            })
            
            # Para eviatr que se escojan mas columnas de las que hay (para los fenotipos)
            
            observe(
                if(is.null(dataset_fenotips()) == F){   
                    if(input$number_visualize_columns_fenotips > length(colnames(dataset_fenotips()))){
                        
                        updateNumericInput(session, "number_visualize_columns_fenotips", value = length(colnames(dataset_fenotips())))
                    }
                }
            )
            output$table_introduced_expressio_diferencial <-  DT::renderDataTable(
                datatable_mod(dataset_expressio_diferencial()
                              [,1:input$number_visualize_columns]
                )
            )

            output$table_introduced_fenotips <-  DT::renderDataTable(
                {datatable_mod(data.frame(dataset_fenotips()))}
            )
            
            # Create Cluster plot ####

            observeEvent(input$create_plot, {
                req(dataset_expressio_diferencial())
                withProgress(message = "Creating cluster plot", value = 0, {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    incProgress(0.33, detail = "Validating genes...")
                    
                    genes(validate_genes(dataset_expressio_diferencial()))
                    
                    
                    incProgress(0.33, detail = "Creating cluster...")
                    
                    cluster_plot <- reactive({
                        cluster_plot <- cluster(genes(),delimiter = input$delimiter_cut_plot, minsize = input$min_size, dir_plot = dir_plot)
                        return(cluster_plot)
                    })
                    
                    incProgress(0.33, detail = "Saving cluster...")
                    
                    png(filename = file.path(dir_plot, "plot_cluster.png"), width = 1600, height = 700)
                    cluster_plot()
                    dev.off()
                    
                    cluster_obj(cluster_plot())
                    remove_modal_spinner()
                })
            })
            
            observeEvent(input$create_plot,{
                output$cluster_plot <- renderPlot({
                    isolate({
                        img <- png::readPNG(paste0(dir_plot,"/plot_cluster.png"))
                        par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                        plot.new()
                        plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                        rasterImage(img, 0, 0, 1, 1)
                    })
                })
            })
            
            # Create Dendogram plot ###
            
            observeEvent(input$create_dendro_plot, {
                req(dataset_fenotips())
                req(dataset_expressio_diferencial)
                withProgress(message = "Creating dendogram plot", value = 0, {
                    show_modal_spinner(
                        spin = "trinity-rings",
                        color = "blue",
                        text = "Please wait..."
                    )
                    
                    incProgress(0.33, detail = "Validating genes...")
                    
                    genes(validate_genes(dataset_expressio_diferencial()))
                    
                    incProgress(0.33, detail = "Selecting samples to keep...")
                    
                    samples_keep_obj(samples_keep(genes(),
                                 delimiter = input$delimiter_cut_plot,
                                 minsize = input$min_size,
                                 number_clust = input$number_clust))
                    
                    cluster_combined_plot <- reactive({
                        cluster_combined_plot <- cluster_combined(samples_keep_obj(),dataset_fenotips(),input$modules)
                        return(cluster_combined_plot)
                        })
                    
                    png(filename = file.path(dir_plot, "dendro_plot.png"), width = 1600, height = 700)
                    cluster_combined_plot()
                    dev.off()
                    
                    cluster_combined_obj(cluster_combined_plot())
                    remove_modal_spinner()
                })
            })

            # Dendro plot ####

            observeEvent(input$create_dendro_plot,{

                output$dendro_plot <- renderPlot(
                    isolate({
                        img <- png::readPNG(paste0(dir_plot,"/dendro_plot.png"))
                        par(mar = c(0, 0, 0, 0))  # Ajusta los márgenes del plot
                        plot.new()
                        plot.window(xlim = c(0, 1), ylim = c(0, 1))  # Define las coordenadas de la ventana del plot
                        rasterImage(img, 0, 0, 1, 1)
                    })

                    )

            })

            # Renderizar el PDF en el uiOutput ####
            output$pdf_viewer <- renderUI({
                tags$iframe(
                    style = "width:1090px; height:800px; overflow:auto;",
                    src = "WGNA_pasos_1.pdf")
            })

         
            # Descargar cluster plot ####

            output$download_cluster_plot <- downloadHandler(
                filename = function() {
                    "cluster_plot.png"
                },
                content = function(file) {
                    # Guarda el gráfico en un archivo temporal
                    png(file)
                    print(cluster(genes(),delimiter = input$delimiter_cut_plot, minsize = input$min_size))
                    dev.off()
                }
            )

            # Guardar variables finals per proxims moduls ####

            session$userData$genes <- reactive({
                req(genes())
                genes <- genes()
                return(genes)
            })

            session$userData$fenotips <- reactive({
                req(dataset_fenotips())
                fenotips <- dataset_fenotips()
                return(fenotips)
            })

            session$userData$fenotips_filtrats <- reactive({
                fenotips_filtrats <- dataset_fenotips()[rownames(session$userData$genes_filtrats()),]
                if (!is.null(fenotips_filtrats) && is.matrix(fenotips_filtrats) || is.data.frame(fenotips_filtrats)) {
                    fenotips_filtrats <- fenotips_filtrats[, colSums(is.na(fenotips_filtrats)) < nrow(fenotips_filtrats)]
                }
                return(fenotips_filtrats)
            })

            session$userData$genes_filtrats <- reactive({
                req(samples_keep_obj())
                genes_filtrats <- samples_keep_obj()
                return(genes_filtrats)
            })

            session$userData$min_size <- reactive ({
                min_size <- input$min_size
                return(min_size)
            })

            # Guardar variables per els reports ####

            #Expressio diferencial variables ###

            session$userData$file_expressio_diferencial <- reactive({
                file_expressio_diferencial <- input$file_expressio_diferencial$datapath
                return(file_expressio_diferencial)
            })

            session$userData$quote_genes <- reactive({
                quote_genes <- input$quote_genes
                return(quote_genes)
            })

            session$userData$sep_genes <- reactive({
                sep_genes <- input$sep_genes
                return(sep_genes)
            })

            session$userData$dec_genes <- reactive({
                dec_genes <- input$dec_genes
                return(dec_genes)
            })

            session$userData$dir_plot <- reactive({
                dir_plot <- dir_plot
                return(dir_plot)
            })
            # Fenotips variables ###

            session$userData$file_fenotips <- reactive({
                file_fenotips <- input$file_fenotips$datapath
                return(file_fenotips)
            })

            session$userData$quote_fenotips <- reactive({
                quote_fenotips <- input$quote_fenotips
                return(quote_fenotips)
            })

            session$userData$sep_fenotips <- reactive({
                sep_fenotips <- input$sep_fenotips
                return(sep_fenotips)
            })

            session$userData$dec_fenotips <- reactive({
                dec_fenotips <- input$dec_fenotips
                return(dec_fenotips)
            })

            # Modificadors cluster

            session$userData$delimiter_cut_plot <- reactive({
                delimiter_cut_plot <- input$delimiter_cut_plot
                return(delimiter_cut_plot)
            })

            session$userData$min_size <- reactive({
                min_size <- input$min_size
                return(min_size)
            })

            session$userData$number_clust <- reactive({
                number_clust <- input$number_clust
                return(number_clust)
            })

            # Modificadors dendro plot

            session$userData$modules <- reactive({
                modules <- input$modules
                return(modules)
            })
            
            # Boton clear data
            
            session$userData$button_clear_content <- reactive({
                button_clear_content <- input$clearButton
                return(button_clear_content)
            })

         }
     )
} 