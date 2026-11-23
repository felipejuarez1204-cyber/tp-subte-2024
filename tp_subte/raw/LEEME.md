# Datos crudos

Acá va el archivo crudo de la base **Subte: viajes molinetes 2024** (Buenos Aires Data, GCBA – SBASE):
https://data.buenosaires.gob.ar/dataset/subte-viajes-molinetes

El archivo tiene 11.440.440 filas y **pesa demasiado para GitHub** (el límite es de 100 MB por archivo), por eso no está subido al repositorio.

- Para correr el análisis **no hace falta**: las tablas ya agregadas están en `input/` y los scripts 02 a 06 usan solo esas tablas.
- Para repetir la agregación desde cero, descargá el CSV del portal, copialo en esta carpeta (`raw/`) y corré `script/01_agregar_datos.R`.
