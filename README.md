# Estimación en tablas de contingencia con marginales dadas
Mi nombre es Sergio Prieto García y soy licenciado en Sociología. Recientemente, he finalizado el Máster en Técnicas Estadísticas en la Universidad de A Coruña. Me complace presentarles en este repositorio el resultado de mi trabajo fin de máster titulado "Estimación en tablas de contingencia con marginales dadas".

El objetivo de este proyecto ha sido aplicar los conocimientos adquiridos durante el Máster en Técnicas Estadísticas en un contexto práctico. Para ello, he desarrollado un análisis exhaustivo en el campo de la demoscopia.

En este repositorio, encontrarán el documento oficial de mi trabajo fin de máster en formato PDF, el cual detalla en profundidad los fundamentos teóricos, la metodología empleada y los resultados obtenidos. Además, he proporcionado una serie de archivos que contienen el código desarrollado en R, el lenguaje de programación utilizado para llevar a cabo el análisis.

# Descripción del proyecto

La estimación de las probabilidades conjuntas en tablas de contingencia basándose únicamente en las frecuencias marginales representa un desafío ampliamente investigado. Bien es cierto que, en la actualidad, existen varios métodos que calculan las probabilidades de las celdas de una tabla de contingencia. Sin embargo, en estos métodos se deben cumplir restricciones para las distribuciones marginales, disponiendo únicamente de las frecuencias de la tabla de contingencia.

El objetivo de este TFM es llevar a cabo una visión general de los métodos ya existentes, proponer un algoritmo EM y finalmente aplicarlo en un problema de estimación de la probabilidad de transferencia de votos entre las distintas opciones políticas en dos elecciones consecutivas en España. En este caso, únicamente se disponen de las frecuencias de voto para cada opción política a nivel de los colegios electorales, siendo las probabilidades conjuntas y condicionales, en principio, desconocidas. Por lo tanto, esta investigación aborda un problema mucho más desafiante y posiblemente sin precedentes que los ya existentes, que es la estimación de las probabilidades de celda cuando solo se han observado las frecuencias marginales pero no las frecuencias de las propias celdas.

# Objetivos 

Los objetivos de este TFM se distribuyen en un objetivo general, eje de la investigación, y una serie de objetivos específicos, complementarios al general.

-   *Objetivo General*
    ----------------
  - Estimar las probabilidades de celda de una tabla de contingencia dadas únicamente sus frecuencias marginales

-   *Objetivos específicos*
    ---------------------
  - Justificar el uso de las probabilidades condicionales para estimar las probabilidades conjuntas
  - Contrastar la hipótesis de que las probabilidades condicionales de transferencia de voto, no dependen de la provincia, circunscripción electoral o mesa electoral
  - Diseñar un algoritmo de esperanza-maximización (EM) para la estimación de las probabilidades condicionales
  - Estudiar la convergencia de las probabilidades condicionales a lo largo de las iteraciones del algoritmo y su tiempo de ejecución
  - Estimar mediante Monte Carlo el error cuadrático medio (MSE) de estimación de las probabilidades condicionales para cada conjunto de datos simulados


# Estructura del repositorio
- **PrietoGarcia-Sergio-TFM.pdf**: El documento oficial de mi trabajo fin de máster, titulado "Estimación en tablas de contingencia con marginales dadas", en formato PDF.
- **/Codigo**: Una carpeta que contiene los archivos de código desarrollados en R para llevar a cabo el análisis estadístico relacionado con la estimación en tablas de contingencia con marginales dadas.
- **/Datos**: Una carpeta opcional que contiene los conjuntos de datos utilizados en el proyecto, en caso de que sean necesarios para reproducir los resultados. Son 2 vectores de datos que contienen los errores cuadráticos para cada uno de los conjuntos de datos simulados en el proyecto. Se almacenan en este repositorio debido a la lenta ejecución del código.

## Recomendaciones y Correcciones

Si encuentras algún error gramatical, de expresión o cualquier otra sugerencia de mejora en el documento o en el código, te agradecería que lo compartieras conmigo. Puedes abrir un **issue** en este repositorio o contactarme directamente para que pueda realizar las correcciones necesarias.

Además, quiero mencionar que próximamente se lanzará una nueva versión del documento PDF con los errores gramaticales y de expresión corregidos. Estoy comprometido/a en mantener y mejorar la calidad de este trabajo, por lo que tus comentarios serán de gran ayuda.

Agradezco de antemano tu contribución y ayuda para mejorar este proyecto.

## Derechos de Autor y Citas

Este trabajo, titulado "Estimación en tablas de contingencia con marginales dadas", es propiedad de Sergio Prieto García y está protegido por derechos de autor. Todos los derechos están reservados.

Si deseas utilizar o hacer referencia a este trabajo en tus propios proyectos, se requiere una adecuada atribución y citación. A continuación, se proporciona un ejemplo de cómo citar este trabajo:

Prieto García, S. (2023). Estimación en tablas de contingencia con marginales dadas. Universidade de A Coruña.


Es importante tener en cuenta que cualquier uso no autorizado, reproducción o modificación de este trabajo sin el consentimiento expreso del autor está prohibido y puede estar sujeto a acciones legales.

Si estás interesado en utilizar este trabajo para fines comerciales, académicos o cualquier otro propósito, te recomiendo que te pongas en contacto conmigo para discutir los detalles y obtener el permiso correspondiente.

Recuerda que el respeto y la integridad de los derechos de autor son fundamentales para fomentar la colaboración y la creatividad en la comunidad. Agradezco de antemano cualquier reconocimiento y atribución adecuada a este trabajo si decides utilizarlo como referencia.
