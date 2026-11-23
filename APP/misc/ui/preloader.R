# Precarga inicial y actualización de datos ----

.preloader_crm <- function() {
  if (.disponible_api("racafeShiny", "racafePreloader")) {
    return(racafeShiny::racafePreloader("inicio", "Cargando CRM Diferenciados…",
                                       logo = .logo_crm))
  }
  NULL
}

.overlay_crm <- function() {
  div(id = "crm-carga", class = "crm-carga", hidden = NA,
      div(class = "crm-carga-card", role = "status", img(src = .logo_crm, alt = "Racafé"),
          div(class = "spinner-border text-danger", `aria-hidden` = "true"), "Cargando publicación…"))
}
