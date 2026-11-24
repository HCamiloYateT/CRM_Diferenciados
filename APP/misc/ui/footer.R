# Footer ----

footer <- bs4DashFooter(
  left = tags$div(tags$span(class = paste0("app-badge app-badge--", badge_estado), toupper(badge_estado))),
  right = tags$img(src = paste0(dirname(.logo_crm), "/logo.png"), alt = "Racafé", style = "height:30px;"),
  fixed = TRUE
)
