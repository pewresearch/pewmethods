---
output:
  word_document: default
  html_document: default
---
# pewmethods

An R package developed by the Pew Research Center Methods team for working with survey data.

## About this package

The Methods team at Pew Research Center is proud to announce the release of version 1.0 of **pewmethods**, an R package containing various functions that we use in our day-to-day work with survey data. The package was envisioned as a way to reuse and maintain code and share it internally with other researchers around the Center. But since many of the problems that these functions were designed to solve are not unique to Pew Research Center projects, we are making the pewmethods package publicly available for other researchers who might find it useful, too. 

Survey data is a bit different from other kinds of data in that it frequently needs to be weighted to represent a larger population. The pewmethods package can help you go through the steps for creating basic survey weights, as well as display weighted estimates of categorical variables, not to mention the various cleaning, recoding, combining and collapsing tasks in between. In this way, our package complements the excellent **survey** package as well as the **tidyverse** set of R packages, which are great for data manipulation.

An overview of what's in the **pewmethods** package and how it works, along with example applications, can be found by calling ```vignette("pewmethods")``` after installation.  

## Requirements 

R 3.5 or greater

## Instructions

1. Install and load the **devtools** package.

2. Install **pewmethods** via:  

```install_github("pewresearch/pewmethods", build_vignettes = TRUE)```

## Use Policy

In addition to the [license](https://github.com/pewresearch/pewmethods/blob/master/LICENSE), Users must abide by the following conditions:

* User may not use the Center's logo.  

* User may not use the Center's name in any advertising, marketing or promotional materials.  

* User may not use the licensed materials in any manner that implies, suggests, or could otherwise be perceived as attributing a particular policy or lobbying objective or opinion to the Center, or as a Center endorsement of a cause, candidate, issue, party, product, business, organization, religion or viewpoint.  

## Issues and Pull Requests
This code is provided as-is for use in your own projects. You are free to submit issues and pull requests with any questions or suggestions you may have. We will do our best to respond within a 30-day time period.  

## Recommended Package Citation
Pew Research Center, 2020, "pewmethods" Available at: github.com/pewresearch/pewmethods. 

## About Pew Research Center

Pew Research Center is a nonpartisan fact tank that informs the public about the issues, attitudes and trends shaping the world. We conduct public opinion polling, demographic research, content analysis and other data-driven social science research. We value independence, objectivity, accuracy, rigor, humility, transparency and innovation. We do not take policy positions. Pew Research Center is a subsidiary of The Pew Charitable Trusts, its primary funder.

## Contact
For all inquiries, please email info@pewresearch.org. Please be sure to specify your deadline, and we will get back to you as soon as possible. This email account is monitored regularly by Pew Research Center Communications staff.
