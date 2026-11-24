# Configuraciones iniciales ----

Sys.setenv(TZ = "America/Bogota")
options(encoding = "UTF-8", shiny.autoreload = FALSE)

# Librerías ----

library(shiny)
library(bs4Dash)

# Versiones y recursos compartidos ----

.ruta_app <- normalizePath(if (file.exists("global.R")) "." else "APP", mustWork = TRUE)
.raiz_crm <- dirname(.ruta_app)
.url_compartido <- paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                          "6c29066361d10ae8330df96ede83586efdc864d6")
.compartido_local <- Sys.getenv("CRM_COMPARTIDO_DIR")
.logo_crm <- paste0(.url_compartido, "/img/logo2.png")
if (nzchar(.compartido_local)) {
  if (!file.exists(file.path(.compartido_local, "img", "logo2.png")) ||
      !file.exists(file.path(.compartido_local, "Styles", "style.css"))) {
    stop("CRM_COMPARTIDO_DIR debe señalar un checkout de Compartido.", call. = FALSE)
  }
  shiny::addResourcePath("crm-marca", file.path(.compartido_local, "img"))
  .logo_crm <- "crm-marca/logo2.png"
}

.incluir_recurso <- function(recurso, incluir) {
  ruta <- if (nzchar(.compartido_local)) file.path(.compartido_local, recurso) else
    paste0(.url_compartido, "/", recurso)
  tryCatch(shiny::tags$head(incluir(ruta)), error = function(e) {
    message("Recurso de Compartido no disponible: ", recurso)
    NULL
  })
}

# Impresiones ----

.titulo <- "CRM Diferenciados"
badge_estado <- "staging"

# Datos ----

source(file.path(.raiz_crm, "DataPrep.R"), local = TRUE)

# Sources ----

for (.archivo in c("config/vistas.R", "compartido/componentes.R", "datos/publicacion_ui.R",
                   "modules/vista.R", "modules/productos/datos.R", "modules/productos/cestas.R",
                   "modules/productos/modulo.R", "modules/gestion/datos.R", "modules/gestion/modulo.R",
                   "modules/operacion/constantes.R", "modules/operacion/funciones.R", "modules/operacion/modulo.R",
                   "modules/orientacion.R", "modules/indicadores_mercado/constantes.R",
                   "modules/indicadores_mercado/datos.R", "modules/indicadores_mercado/modulos.R",
                   "modules/encabezado/00_header.R", "modules/encabezado/01_tabla_indicadores.R",
                   "modules/encabezado/02_notificaciones_tareas.R")) {
  source(file.path(.ruta_app, "misc", .archivo), local = TRUE)
}

.recursos_compartidos <- shiny::singleton(shiny::tagList(
  tags$span(hidden = NA, `data-rf-interactions` = "true"),
  if (.disponible_api("racafeForms", "racafeFormsDependencies")) racafeForms::racafeFormsDependencies(),
  if (.disponible_api("racafeModulos", "MenuAccionesDependencies")) racafeModulos::MenuAccionesDependencies(),
  .incluir_recurso("Styles/style.css", shiny::includeCSS),
  .incluir_recurso("Scripts/clipboard.js", shiny::includeScript),
  .incluir_recurso("Scripts/forms.js", shiny::includeScript),
  .incluir_recurso("Scripts/controlbar.js", shiny::includeScript),
  .incluir_recurso("Scripts/interactions.js", shiny::includeScript),
  .incluir_recurso("Styles/crm-nacional.css", shiny::includeCSS),
  tags$head(tags$link(rel = "stylesheet", href = "crm.css"), tags$script(src = "crm.js"))
))

# Interfaz ----

for (.archivo in c("header.R", "sidebar.R", "body.R", "controlbar.R", "footer.R", "preloader.R")) {
  source(file.path(.ruta_app, "misc", "ui", .archivo), local = TRUE)
}
rm(.archivo)
