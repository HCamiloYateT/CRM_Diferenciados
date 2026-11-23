# Dependencias y carga explícita ----

library(shiny)
library(bs4Dash)

.raiz_crm <- normalizePath(if (file.exists("../DataPrep.R") && file.exists("global.R")) ".." else ".", mustWork = TRUE)
.ruta_app <- file.path(.raiz_crm, "APP")
.compartido <- paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                      "6c29066361d10ae8330df96ede83586efdc864d6")
.compartido_local <- Sys.getenv("CRM_COMPARTIDO_DIR")
.logo_crm <- paste0(.compartido, "/img/logo2.png")
if (nzchar(.compartido_local)) {
  if (!file.exists(file.path(.compartido_local, "img", "logo2.png")) ||
      !file.exists(file.path(.compartido_local, "Styles", "style.css"))) {
    stop("CRM_COMPARTIDO_DIR debe señalar un checkout de Compartido.", call. = FALSE)
  }
  shiny::addResourcePath("crm-marca", file.path(.compartido_local, "img"))
  .logo_crm <- "crm-marca/logo2.png"
}
.titulo <- "CRM Diferenciados"

source(file.path(.raiz_crm, "DataPrep.R"), local = TRUE)
for (.archivo in c("config/vistas.R", "datos/publicacion_ui.R",
                   "ui/componentes.R", "modules/vista.R", "modules/productos/datos.R", "modules/productos/cestas.R",
                   "modules/productos/modulo.R", "modules/orientacion.R", "modules/indicadores_mercado/constantes.R",
                   "modules/indicadores_mercado/datos.R", "modules/indicadores_mercado/modulos.R", "ui/header.R", "ui/sidebar.R", "ui/controlbar.R",
                   "ui/body.R", "ui/footer.R", "ui/preloader.R")) {
  source(file.path(.ruta_app, "misc", .archivo), local = TRUE)
}
rm(.archivo)
