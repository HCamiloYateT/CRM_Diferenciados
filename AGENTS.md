# Guía de trabajo

- Cambiar solo los scripts necesarios, manteniendo el estilo Racafé y los contratos de entrada y salida.
- Mantener `DataPrep.R` autocontenido. Consultar fuentes autorizadas en modo lectura y publicar solo datos operacionales.
- Preservar la línea original 20000, las llaves por compañía y el grano de pedido y línea.
- Reutilizar componentes corporativos y preservar los cambios existentes. Excluir credenciales y datos de Git.
- Validar de forma focalizada con parseo R y una comprobación del flujo afectado cuando corresponda.
- Cerrar cada cambio con un PR.
