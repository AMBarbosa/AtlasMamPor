# Atlas of Mammals in Portugal

This repo contains the data and code behind the terrestrial interactive maps of the Portuguese mammal atlas ([Bencatel et al. 2019](https://atlasmamiferosportugal.wordpress.com)) and major datasets published since ([Grilo et al. 2022]( https://doi.org/10.1002/ecy.3654)). If you have R installed with the 'shiny' package and the app's dependencies (currently 'terra', 'leaflet' and 'leaflet.extras'), you can **run the app in your computer** by typing:

```{r, eval=FALSE}
shiny::runGitHub("AtlasMamPor", "AMBarbosa")
```

You can also [**use the app online**](https://ambiogeo.shinyapps.io/atlasmampor/). You'll be able to interactively create maps such as this one on wolf occurrence records in Portugal:

![](imagens/example.JPG)

These materials are available under a **Creative Commons Attribution-ShareAlike license (CC BY-SA 4.0)**. If you use mammal occurrence data contained in these files, remember to **cite**:

**REFERENCES**

Bencatel J., Sabino-Marques H., Álvares F., Moura A.E. & Barbosa A.M. (2019) *Atlas de Mamíferos de Portugal*, 2ª edição. Universidade de Évora, Évora. URL: https://atlasmamiferosportugal.wordpress.com

Grilo et al. (2022) Mammals in Portugal: A data set of terrestrial, volant, and marine mammal occurrences in Portugal. Ecology, 103: e3654. URL: https://doi.org/10.1002/ecy.3654
