# An introduction to Item Response Theory Models with R

Welcome to this introductory course to Item Response Theory (IRT) models! The aim of this course is to give an overview on IRT models, with a focus on IRT models for dichotomous responses. 

According to Item Response Theory (IRT) models, the observed response of a person to an item is a function of the characteristics of the person (i.e., the latent trait) and of the characteristics of the items, as described by both person's and item parameters. Different IRT models are available according to the number of parameters used for describing the functioning of the items and to the nature of the analyzed responses.
This course provides an introduction to IRT models for dichotomous (e.g., "true" vs. "false") and polytomous responses (e.g., agreement on a 5-point Liker-type scale), with a main focus on the former ones. 
The assumptions for the application of the models, the evaluation of the fit of the models, and the implications and meaning of the item parameters will be supported by their guided application to simulated data with the [`TAM`](https://cran.r-project.org/web/packages/TAM/index.html) package in `R`.
Moreover, the potential of these models for a thorough investigation of the item functioning will be presented under two different perspectives. Firstly, the methods for detecting the differential item functioning are presented. Then, the item information function is illustrated, which allows for obtaining a measure of the precision with which the items assess different levels of the latent trait. 
This information can also be exploited for the development of short test forms.

## Course structure 

- Introduction to IRT models [PDF](intro/intro.pdf)
- Estimating the models, testing the assumptions, and choosing the best model [HTML](Slides/model-estimation.html) [R code](Slides/Code-model-estimation.R)
- Item time: Item fit,  and differential item functioning [HTML](Slides/item-time.html) [R code](Slides/Code-item-time.R)
- [Visual IRT](irt-visual.Rmd)
- [Data](https://drive.google.com/drive/folders/1EF4cN4yiB3Fft_y2Mh4r0Mclxc6bTRdf?usp=sharing)
- [Reference list](Reference-list.pdf)


