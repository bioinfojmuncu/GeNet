# Función modificada de datatable con opciones extendidas para personalización y descarga de datos
datatable_mod <- function(
    df,  # DataFrame que se mostrará en la tabla
    to_print = FALSE,  # Habilitar botón de impresión
    to_copy = FALSE,   # Habilitar botón de copiar
    to_search = TRUE,  # Habilitar barra de búsqueda
    to_entries = TRUE, # Habilitar selección de número de entradas por página
    to_download_all = TRUE,  # Habilitar descarga de todos los datos
    to_download_current = FALSE,  # Habilitar descarga de la página actual
    to_download_format = c('csv', 'excel', 'pdf'),  # Formatos disponibles para descarga
    to_filter = 'none',  # Ubicación del filtro (top, bottom, none)
    to_edit = FALSE,  # Permitir edición de datos en la tabla (experimental)
    to_colvis = FALSE  # Habilitar el botón de visibilidad de columnas
) {
  
  # Determinar la configuración de los botones de extensión (Buttons)
  if (to_search + to_entries == 2) {
    to_s_e <- 'Blfrtip'  # Configuración completa (Search + Entries)
  } else {
    if (to_search) {
      to_s_e <- 'Bfrtip'  # Solo búsqueda habilitada
    }
    if (to_entries) {
      to_s_e <- 'Blrtip'  # Solo selección de entradas habilitada
    }
    if (to_search + to_entries == 0) {
      to_s_e <- 'Brtip'   # Ninguna opción habilitada
    }
  }
  
  # Construir la tabla con configuración personalizada
  datatable(
    df,
    editable = to_edit,  # Habilitar la edición de datos (experimental)
    filter = to_filter,  # Ubicación del filtro (top, bottom, none)
    selection = "none",  # Deshabilitar selección de filas (por ahora)
    escape = FALSE,      # Permitir HTML en los datos (peligroso si no se sanitiza)
    extensions = "Buttons",  # Habilitar la extensión de botones
    options = list(
      scrollX = TRUE,   # Habilitar desplazamiento horizontal
      autoWidth = FALSE,  # Deshabilitar ajuste automático del ancho de las columnas
      dom = to_s_e,     # Configuración del DOM (posición de elementos)
      buttons = list(
        ifelse(to_colvis, I('colvis'), ''),  # Botón de visibilidad de columnas
        ifelse(to_search, 'search', ''),     # Botón de búsqueda
        ifelse(to_copy, 'copy', ''),         # Botón de copiar
        ifelse(to_print, 'print', ''),       # Botón de impresión
        if (to_download_current) {           # Opciones de descarga de la página actual
          list(
            extend = 'collection',
            buttons = 
              lapply(to_download_format, function(format) {
                list(
                  extend = format,
                  filename = "page",
                  title = 'page',
                  exportOptions = list(columns = ":visible", modifier = list(page = "current"))
                )
              }),
            text = 'Download current page'
          )
        } else { '' },
        if (to_download_all) {                # Opciones de descarga de todos los datos
          list(
            extend = 'collection',
            buttons = 
              lapply(to_download_format, function(format) {
                list(
                  extend = format,
                  filename = "data",
                  title = 'data',
                  exportOptions = list(columns = ":visible", modifier = list(page = "all"))
                )
              }),
            text = 'Download all data'
          )
        } else { '' }
      ),
      lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All'))  # Opciones de selección de entradas por página
    ),
    class = "display"  # Clase CSS para la tabla
  )
}

datatable_mod3 <- function(df,filename){datatable(
    df,
    extensions = "Buttons",
    filter = "top",
    selection = "none", #this is to avoid select rows if you click on the rows
    
    options = list(
        
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'Blrtip', # the important thing is that there is the l to allow for the lengthMenu 
        # https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
        
        buttons = list(
            
            # insert buttons with copy and print
            # colvis includes the button to select and view only certain columns in the output table
            # from https://rstudio.github.io/DT/extensions.html 
            I('colvis'), 'copy',
            
            # code for the first dropdown download button
            # this will download only the current page only (depends on the number of rows selected in the lengthMenu)
            # using modifier = list(page = "current")
            # only the columns visible will be downloaded using the columns:":visible" option from:
            # https://stackoverflow.com/questions/72317260/how-to-download-only-the-selected-columns-in-a-dataframe-using-colvis-from-dt-in/72317607#72317607
            list(
                extend = 'collection',
                buttons = list(
                    list(extend = "csv", filename = filename,exportOptions = list(
                        columns = ":visible",modifier = list(page = "current"))
                    ),
                    list(extend = 'excel', filename = filename, title = NULL, 
                         exportOptions = list(columns = ":visible",modifier = list(page = "current"))),
                    list(extend = 'pdf', filename = filename, title = NULL, 
                         exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                
                text = 'Download current page'),
            
            # code for the  second dropdown download button
            # this will download the entire dataset using modifier = list(page = "all")
            list(
                extend = 'collection',
                buttons = list(
                    list(extend = "csv", filename = filename,exportOptions = list(
                        columns = ":visible",modifier = list(page = "all"))
                    ),
                    list(extend = 'excel', filename = filename, title = NULL, 
                         exportOptions = list(columns = ":visible",modifier = list(page = "all"))),
                    list(extend = 'pdf', filename = filename, title = NULL, 
                         exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                text = 'Download all data')
        ),
        # add the option to display more rows as a length menu
        lengthMenu = list(c(5,10, 30, 50, -1),
                          c('5','10', '30', '50', 'All'))
    ),
    class = "display"
)
}

