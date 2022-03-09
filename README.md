# Ecology-Dashboard

This shiny dashboard was created with ecology students in mind, allowing them to visualise and statistically analyse ecological data, particularly fatty acid related data. So far, it includes basic statistical analysis such as ANOVA, some visualisation options, including multivariate scaling methods like PCA and nMDS.

Further, there are utility functions which turn "wide" data (multiple responses per individual, e.g. multiple fatty acids) into "long" data (one response per individual), which is needed for some of the visualisation or analysis options, as well as a "response to biomass" transformer that takes the responses of gas chromatography - flame ionisation detection (GC-FID) or gas chromatography - mass spectrometry (GC-mS) and transforms them into biomass data.

This project is still WIP, and tailored to the requirements and wishes of the ecology group at the Humboldt-University to Berlin.

Current Tasks include expansion of plots/models and better method to generate tables and results pages.