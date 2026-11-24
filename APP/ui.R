# Ensamble del dashboard ----

sidebar <- .sidebar_crm()
body <- .body_crm()

bs4DashPage(
  title = .titulo, dark = NULL, scrollToTop = TRUE, help = NULL,
  preloader = preloader_inicio,
  header = header,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer
)
