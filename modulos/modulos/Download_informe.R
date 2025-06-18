ui_download_informe <- function(id_modulo) {
    ns <- NS(id_modulo)
    tagList(
        textInput(ns("informe_name"), "Name of report"),
        #textInput(ns("informe_title"), "Introdueix el titol per l'arxiu que te vas a desarregar"),
        downloadButton(ns("download_informe_pdf"), "Download report"))
}

server_download_informe <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            informe_name <- reactive({
                informe_name <- input$informe_name
                return(informe_name)
            })
            
            observe(print("nombre archivo"))
            observe(print(informe_name()))
            # Parte del servidor que se va activar, cuando se pulse el boton de descargar informe, en los diferntes tipos de archivo
            
            # VARIABLES module_WGCA_1.R ####

            ## Per la lectura de la matriu de expresio introduida ####
            dataset_expressio_diferencial <- reactive(session$userData$file_expressio_diferencial())
            quote_genes <- reactive(session$userData$quote_genes())
            sep_genes <- reactive(session$userData$sep_genes())
            dec_genes <- reactive(session$userData$dec_genes())
            
            ## Per la lectura dels fenotips introduits ####
            file_fenotips <- reactive(session$userData$file_fenotips())
            quote_fenotips <- reactive(session$userData$quote_fenotips())
            sep_fenotips <- reactive(session$userData$sep_fenotips())
            dec_fenotips <- reactive(session$userData$dec_fenotips())
            
            ## La ruta on estan descarregats els plots, per carregarlos ####
            dir_plot <- reactive(session$userData$dir_plot())
            #dir_plot <- directorio
            ## Variables cluster plot ####
            
            genes <- reactive(session$userData$genes())
            delimiter_cut_plot <- reactive(session$userData$delimiter_cut_plot())
            min_size <- reactive(session$userData$min_size())
            number_clust <- reactive(session$userData$number_clust())
            
            ## Variables dendro plot ####
            
            modules <- reactive(session$userData$modules())
            
            ## Variables power plot ####
            
            by <- reactive(session$userData$by())
            to <- reactive(session$userData$to())
            pam_respects_dendro <- reactive(session$userData$pam_respects_dendro())
            tom_type <- reactive(session$userData$tom_type())
            network_type <- reactive(session$userData$network_type())
            power <- reactive(session$userData$power())
            
            output$download_informe_pdf <- downloadHandler(
                filename = function() {
                    paste(informe_name(),".html")  
                },
                # Para la salida en html, usa "report.html"
                 #filename = paste(informe_name(),".html"),
                content = function(file) {
                    # Copia el reporte a un directorio temporal antes de porcesarlo, en
                    # caso de que no tengamos permiso de escritura en el directorio actual
                    # puede ocurrir un error
                    tempReport <- file.path('./reports/report_html.Rmd')
                    file.copy("report_html.rmd", tempReport, overwrite = TRUE)
                    
                    # configurar los parametros para pasar al documento .Rmd
                    params <- list(
                        title1 = paste0(input$informe_name),
                        ## Per la lectura de la matriu de expresio introduida ####
                        file_expressio_diferencial = dataset_expressio_diferencial(),
                        quote_genes = quote_genes(),
                        sep_genes = sep_genes(),
                        dec_genes = dec_genes(),
                        
                        ## Per la lectura dels fenotips ####
                        file_fenotips = file_fenotips(),
                        quote_fenotips = quote_fenotips(),
                        sep_fenotips = sep_fenotips(),
                        dec_fenotips = dec_fenotips(),
                        
                        ## La ruta on estan descarregats els plots, per carregarlos ####
                        dir_plot = paste0(dir_plot()),
                        
                        ## Variables cluster plot ####
                        genes = genes(),
                        delimiter_cut_plot = delimiter_cut_plot(),
                        min_size = min_size(),
                        number_clust = number_clust(),
                        
                        ## Variables dendro plot ####
                        
                        modules = modules(),
                        
                        ## Variables power plot ####
                        
                         by = by()
                        # to = to(),
                        # pam_respects_dendro = pam_respects_dendro(),
                        # tom_type = tom_type(),
                        # network_type = network_type(),
                        # power = power()
                        
                    )
                    
                    #Compilar el documento con la lista de parametros, de tal manera que se
                    #evalue de la misma manera que el entorno de la aplicacion.
                    rmarkdown::render(
                        tempReport, output_file = file, 
                        params = params, 
                        envir = new.env(parent = globalenv())
                    )
                }
            )
        }
    )
}