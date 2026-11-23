# Fase 2 | Modelo de datos de CRM Diferenciados

**Estado al 23/11/2026.** Diseño versionado y DDL sin ejecutar. Las decisiones aprobadas el 23/11/2026 prevalecen sobre el [diagnóstico de Fase 1](../diagnostico/01-diagnostico.md). Los nombres y tipos del esquema interno son propuestas de diseño. Las claves y cardinalidades de origen requieren exportaciones de evidencia.

- [Modelo conceptual y lógico, contratos y decisiones](modelo.md)
- [Diccionario físico por campo](diccionario.md)
- [Exportaciones para validar los contratos](exportaciones.md)
- [DDL y orden de los scripts](../../sql/README.md)

## Validación de esta entrega

Revisión estática de las declaraciones DDL y del diccionario, incluidas claves locales y compatibilidad estructural de las FK con sus claves destino. Revisión de alcance 20000, ausencia de cargas y separación de hechos temporales. Pendientes la ejecución en un ambiente de prueba autorizado, la verificación de compatibilidad con el motor elegido y las reconciliaciones sobre exportaciones.
