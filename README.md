# R Shiny Final Project, Spring 2023✨
[Link to final shiny app](https://mehussey.shinyapps.io/FinalProject/)

## Assignment Description📝
To bring the entire course together students will create an App with an interactive map as the central focus.


- One leaflet map🗺️

  - The app should have at least two different types of layers
points/markers, lines, heatmap, polygons etc.
  - Leaflet map must make use of LeafletProxy for updating layers (observers)
One (1) datatable,

- Two (2) interactive charts or graphs (plotly)📊

- Three (3) input commands

- One (1) downloadHandler that allows users to get the raw data they are viewing.

Students who use one or more API's to feed either their map or data displayed will receive up to 20 Bonus points on the assignment.

Applications should be deployed and working on shinyapps.io.

## The data🧠
For this assignment, I used New York City historic shooting data. Because of the size of the data, I filtered to include only the years 2019-2021. The data was found on New York City Open Data and can be found [here](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8). Each row represents a shooting that has occured. 

I also used a borough boundaries geoJSON file to add the Polygon layer to my leaftlet map. That data can be found [here](https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm). 
