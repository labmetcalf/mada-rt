# Estimating R<sub>t</sub> for Covid-19 in Madagascar

Here we estimate the time varying effective reproductive number of R<sub>t</sub> for Covid-19 in Madagascar. 

## Methods

We use the methods described in [here](https://epiforecasts.io/covid/), using the R packages [`epiNow`](https://epiforecasts.io/EpiNow/), [`epiSoon`](https://github.com/epiforecasts/EpiSoon), and [`epiEstim`](), code available [here](https://cran.r-project.org/web/packages/EpiEstim/index.html). 

The pipeline for running national and regional Rt estimates is adapted from [here](https://github.com/epiforecasts/covid-global/blob/69ebf0325476f7dc565267a2416d76b0ee4320d6/update_nowcasts.R)

There are a number of limitations and uncertainties in these methods and in estimating R<sub>t</sub>, many of which are reviewed [here](https://www.medrxiv.org/content/10.1101/2020.06.18.20134858v2?rss=1). 

