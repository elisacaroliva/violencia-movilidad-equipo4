# Insumos de la v5

| Carpeta | Archivo | Fuente | Uso |
|---|---|---|---|
| `data/raw/emovi/` | entrevistado_2023.dta, hogar_2023.dta | CEEY, ESRU-EMOVI 2023 | Principal |
| `data/raw/sesnsp/` | IDEFC_MAnterior.csv | SESNSP, incidencia estatal, metodología anterior 1997-2017 | Principal |
| `data/raw/conapo/` | 00_Pob_Mitad_1950_2070.csv | CONAPO, población estatal | Robustez (construcción v4) |
| `data/raw/sesnsp_municipal/` | IDM_MAnterior_ago2024.csv (y su .zip) | SESNSP, "2011-2017 (Fuero común - Delitos). Incidencia delictiva municipal", enlazado en gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva. Descargado el 3 nov 2026. | Módulo municipal |
| `data/raw/conapo_municipal/` | proy_pob_mpo_conapo.csv | Proyecciones municipales CONAPO 2010-2030, versión ordenada en github.com/segasi/conapo_proyecciones. Descargado el 3 nov 2026. | Módulo municipal |
| `data/raw/emovi_2017/` | **entrevistado_2017.dta (falta)** | CEEY, ESRU-EMOVI 2017, a petición en ceey.org.mx | Módulo de ciclo de vida |

**Población municipal oficial.** Si se consigue el archivo oficial de CONAPO, basta con colocarlo en `data/raw/conapo_municipal/`, ya sea en formato largo (`cve_mpo`, `mpo`, `sexo`, `año`, `valor`) o directo (`CLAVE`, `pob`), y borrar el actual. El script 11 reconoce los dos formatos.

**EMOVI 2017.** Al colocar `entrevistado_2017.dta`, el script 12 se ejecuta solo. Usa las variables p05, p06, factor, p23, Estado y los reactivos listados en el script.
