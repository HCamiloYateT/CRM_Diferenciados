# Guía de trabajo

- Trabajar en `desarrollo-inicial` con historial independiente y preservar cambios locales.
- Usar R, Shiny y bs4Dash y revisar las bibliotecas corporativas antes de crear componentes.
- Usar UTF-8, nombres y comentarios claros, y el estilo Racafé de R/Shiny.
- Mantener separados los datos operacionales y la gestión manual auditada.
- Conservar el código de línea original, la clave de origen y la granularidad.
- Documentar procedencia y estado de aprobación de reglas y supuestos.
- Excluir credenciales y datos operacionales del control de versiones.
- Usar persistencia RDS centralizada. El ETL publica exclusivamente en el dominio operacional.
- Validar en proporción al cambio y documentar resultados y pendientes.
