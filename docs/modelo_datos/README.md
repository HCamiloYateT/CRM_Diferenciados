# Fase 2 | Modelo de datos de CRM Diferenciados

**Estado al 23/11/2026.** Diseño versionado y persistencia RDS. Las decisiones aprobadas el 23/11/2026 prevalecen sobre el [diagnóstico de Fase 1](../diagnostico/01-diagnostico.md). Los nombres y tipos del esquema interno son propuestas de diseño. Las claves y cardinalidades de origen requieren exportaciones de evidencia.

- [Modelo conceptual y lógico, contratos y decisiones](modelo.md)
- [Diccionario relacional histórico por campo](diccionario.md) como antecedente de contratos RDS.
- [Exportaciones para validar los contratos](exportaciones.md)
- [Arquitectura, API y despliegue RDS](persistencia_rds.md)

## Validación de esta entrega

La arquitectura RDS reemplaza el DDL de persistencia propia. Los scripts SQL previos se conservan en el [commit histórico](https://github.com/HCamiloYateT/CRM_Diferenciados/tree/6c0e7b0/sql). Las reconciliaciones de origen siguen pendientes de las exportaciones.
