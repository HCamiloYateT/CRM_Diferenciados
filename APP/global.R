# Dependencias y carga explícita ----

library(shiny)
library(bs4Dash)

.raiz_crm <- normalizePath(if (file.exists("misc/persistencia/rds.R")) ".." else ".", mustWork = TRUE)
.ruta_app <- file.path(.raiz_crm, "APP")
.compartido <- paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                      "492901a0943893e4a464ab87d7c18700499a1814")
.compartido_local <- Sys.getenv("CRM_COMPARTIDO_DIR")
.logo_crm <- paste0(.compartido, "/img/logo2.png")
.estilos_crm <- paste0(.compartido, "/Styles/style.css")
if (nzchar(.compartido_local)) {
  if (!file.exists(file.path(.compartido_local, "img", "logo2.png")) ||
      !file.exists(file.path(.compartido_local, "Styles", "style.css"))) {
    stop("CRM_COMPARTIDO_DIR debe señalar un checkout de Compartido con img/ y Styles/.", call. = FALSE)
  }
  shiny::addResourcePath("crm-marca", file.path(.compartido_local, "img"))
  shiny::addResourcePath("crm-estilos", file.path(.compartido_local, "Styles"))
  .logo_crm <- "crm-marca/logo2.png"
  .estilos_crm <- "crm-estilos/style.css"
}
.titulo <- "CRM Diferenciados"

for (.archivo in c("persistencia/rds.R", "datos/dataprep_pedidos.R", "config/vistas.R", "datos/publicacion_ui.R",
                   "ui/componentes.R", "modules/vista.R", "ui/header.R", "ui/sidebar.R", "ui/controlbar.R",
                   "ui/body.R", "ui/footer.R", "ui/preloader.R")) {
  source(file.path(.ruta_app, "misc", .archivo), local = TRUE)
}
rm(.archivo)
