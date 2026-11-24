# Preloader corporativo de inicio ----

preloader_inicio <- if (.disponible_api("racafeShiny", "racafePreloader")) {
  racafeShiny::racafePreloader("inicio", "Cargando aplicación…")
} else NULL

# Actividad durante la actualización de la publicación ----

.overlay_crm <- div(
  id = "crm-carga", class = "crm-carga", hidden = NA,
  div(class = "crm-carga-card", role = "status", img(src = .logo_crm, alt = "Racafé"),
      div(class = "spinner-border text-danger", `aria-hidden` = "true"), "Cargando publicación…")
)
