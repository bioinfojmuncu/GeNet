
# Configuración inicial y carga de bibliotecas
options(shiny.maxRequestSize = 30 * 1024^2)  # Configura el tamaño máximo de solicitud para Shiny

source("./librerias/libreria_WGCNA.R")  # Carga de funciones específicas desde un archivo externo


library(shinymanager)

base_de_dades <- "../users_applications_database/database.sqlite"
res <- read_db_decrypt(base_de_dades, passphrase = "passphrase_wihtout_keyring")  # posa la teva frase real

options(shiny.trace = FALSE)
options("shinymanager.application" = "Biograph")

con <- dbConnect(RSQLite::SQLite(),base_de_dades)




header <- dashboardHeader(title = "GeNet", titleWidth = 250)

# Barra lateral: 
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebarMenu",
        # Navegació pestanyes
        menuItem("Upload and clean data", tabName = "cleaning", icon = icon("database")),
        menuItem("Network step", tabName = "network", icon = icon("circle-nodes")),
        menuItem("Module–trait association", tabName = "dendro", icon = icon("code-fork")),
        menuItem("Igraph plot", tabName = "igraph", icon = icon("diagram-project")),
        ui_download_informe("download_informe")
        
    ),
    width = 250  # Amplada
)

# Cos de l'aplicació
body <- dashboardBody(
    useShinyjs(),  
    tags$head(
        # toggleOpacityAndPointerEvents: 
        # Aquesta funció canvia l'opacitat i els esdeveniments del punter d'un element amb la classe .mfb-component__wrap en funció del valor de isVisible
        
        # keypress event handler: Aquesta funció detecta quan es prem la tecla Enter (codi 13) i 
        # fa clic al botó de login amb l'ID shinymanager-auth-submit si hi ha una finestra modal visible.
        # També afegeix logs a la consola per ajudar a diagnosticar problemes
        tags$script(HTML("
        // Funció per cambiar opacitat i events del ratolí
        function toggleOpacityAndPointerEvents(isVisible) {
          var element = document.querySelector('.mfb-component__wrap');
          if (isVisible) {
            element.style.opacity = '1';  // Opacitat al 100%
            element.style.pointerEvents = 'auto';  // Activar punter
          } else {
            element.style.opacity = '0';  // Opacitat al 0%
            element.style.pointerEvents = 'none';  // Desactivar punter
          }
        }
      ")),
        
        tags$script(HTML("


$(document).on('keypress', function(e) {
 console.log('Tecla premuda: ' + e.which);
if(e.which == 13 && $('.modal-content:visible').length > 0) {
 console.log('Intentant fer clic al botó de login');
 $('.modal-content:visible #shinymanager-auth-submit').click();
}
});


"))
        
    ),
    tabItems(
        tabItem(tabName = "cleaning", body_01("ui_01")),  # Contingut "Data input cleaning"
        tabItem(tabName = "network", body_02("ui_02")),   # Contingut "Network analysys"
        tabItem(tabName = "dendro", body_03("ui_03")),    # Contingut "DendroPlot"
        tabItem(tabName = "igraph", body_06("ui_06"))     # Contingut "Network Igraph"
    )
)

# Definició  barra de control
controlbar <- dashboardControlbar(
    id = "controlbar",
    width = 250,
    collapsed = FALSE,
    overlay = TRUE,
    pinned = TRUE,
    
    # Menús condicionales para la barra de control basados en la pestaña seleccionada
    conditionalPanel(
        condition = "input.sidebarMenu=='cleaning'",
        controlbarMenu(
            controlbarItem(title = "Upload and clean data", icon = icon("broom"), ui_01("ui_01"))
        )
    ),
    
    conditionalPanel(
        condition = "input.sidebarMenu=='network'",
        controlbarMenu(
            controlbarItem(title = "Network analysis", icon = icon("magnifying-glass-chart"), ui_02("ui_02"))
        )
    ),
    
    conditionalPanel(
        condition = "input.sidebarMenu=='dendro'",
        controlbarMenu(
            controlbarItem(title = "Dendrogram plot", icon = icon("code-fork"), ui_03("ui_03"))
        )
    ),
    conditionalPanel(
        condition = "input.sidebarMenu=='igraph'",
        controlbarMenu(
            controlbarItem(title = "Net igraph plot", icon = icon("diagram-project"), ui_06("ui_06"))
        )
    )
)


ui <-  dashboardPage(
    header,
    sidebar,
    controlbar = controlbar,
    body
)

ui <- secure_app(ui,
                 enable_admin = T, # Para poder acceder a las opciones de admin, como el acceso a la base de datos SQLlite
                 choose_language = c("en",'es',"ca"),
                 tags_bottom = tags$div(
                     tags$div(
                         tags$image(src = 'Logo-IdISBa-2024.jpg',width = '100px'),
                         style = "text-align: center; margin: 0 auto; margin-bottom: 40px;"
                     ),
                     tags$p(
                         tags$a(
                             href = "https://www.idisba.es/cat/Serveis-de-Suport/Plataformes-Cientificot%C3%A8cniques/Gen%C3%B2mica-i-Bioinform%C3%A0tica",
                             target="_blank", "Plataforma Genòmica i Bioinformàtica"
                         )
                     ) ,
                     tags$p(
                         "For any question, please  contact ",
                         tags$a(
                             href = "mailto:josep.muncunill@ssib.es?Subject=Shiny%20aManager",
                             target="_blank", "Josep Muncunill"
                         )
                     )
                 ),
                 
                 # Script de JavaScript para detectar Enter
                 tags$script(HTML("
$(document).on('keypress', function(e) {
  console.log('Tecla premuda: ' + e.which);
  if(e.which == 13 && $('.modal-content:visible').length > 0) {
    console.log('Intentant fer clic al botó de login');
    $('.modal-content:visible #shinymanager-auth-submit').click();
  }
});

"))
                 
                 
                 ,background  = "linear-gradient(90deg, rgba(19,125,121,1) 0%, rgba(0,115,103,1) 35%, rgba(35,91,168,1) 100%)"
)



server <- function(input, output, session) {
    
    res_auth <- secure_server(
        check_credentials = check_credentials(
            base_de_dades,
            passphrase = "passphrase_wihtout_keyring"
        ), timeout = 30  # Temps inactivitat
    )
    
    observeEvent(res_auth$user, {
        
        
        # obtenir la finesrta seleccionada
        current_tab <- reactive({
            paste0(input$sidebarMenu)
        })
        
        # Ruta base on es vol crear la carpeta
        directorio_base <- "./"  
       
        # Concatenem el directori base amb el nom d'usuari
        
        directori <- paste0(directorio_base, res_auth$user)
        server_01("ui_01", directori)
        server_02("ui_02")
        server_03("ui_03")
        server_06("ui_06")
        server_download_informe("download_informe")

    })
    
    session$onSessionEnded(function() {
        DBI::dbDisconnect(con)
    })
}


shinyApp(ui = ui, server = server)