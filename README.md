## Introducción

El siguiente software permite guardar registros sobre los casos de muerte por COVID-19 en Puerto Rico, el cual nos permite mostrar las tasas de casos de covid por municipio separados por rango de fechas, categorizados entre hombres o mujeres. Para esta implementación se ha usado el entorno de desarrollo integrado de R Studio, con lenguaje R, usando librerías adecuadas como Shiny, el cual nos permite desplegar una aplicación web, y utilizar un servidor local que pueda funcionar desde R Studio. 

## Informes

Reporte de la Investigación: [aquí](https://docs.google.com/document/d/1q5d7PPDcDYLT32XyMfwETtbtld25Komi4JDmD3Yrz-8/edit?usp=sharing).

Este proyecto fue basado en el siguiente proyecto original:
[aquí](https://github.com/rafalab/pr-covid/).

Junto a la charla presentada al PRPHT está [aquí](https://rafalab.github.io/pr-covid/prpht.html).

## Datos

Todos los datos están en el directorio `data`

* Archivos que empiezan con `20_0615` son los datos de los laboratorios moleculares
* `Bioportal molecular tests 7-14-2020.xlsx` contiene todas las pruebas en la base de datos del Departamento de salud. No incluye identificador de personas así no se puede remover duplicados.
* `Bioportal tests by collected data_Refresh.xlsx` contiene los totales para cada día.
* `Dash Total.xlsx`contine datos de hospitalizaciones.
* `TestingByDate.csv` datos de Massachusetts. Para obtener los datos de Massachusetts descargué este [archivo zip](https://www.mass.gov/doc/covid-19-raw-data-june-30-2020/download)

unzip y saque el archivo TestingByDate.csv

Los datos extraidos de los datos cruedos están en el directorio `rdas`

## Codigo

* Para extaer los datos de Puerto Rico pueden correr el archivo wrangle-YYYY-MM-DD.R. 

* `prpht.Rmd` creo la [presentación at PRPHT](https://rafalab.github.io/pr-covid/prpht.html).

* `wrangle-bioportal-data-2020-07-14.R` crea un resumen diario de positivos y pruebas.

## Créditos

* Créditos a Rafael Irizarry, PhD, dado que esta investigación fue basada en su proyecto.

* Créditos Humberto Ortiz Zuazaga, PhD, por asignar el proyecto de investigación.
