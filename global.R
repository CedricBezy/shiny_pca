
# rm(list = ls())

## data.frame
library(dplyr)
library(tidyr)
library(tibble)

## Acp
library(FactoMineR)

## plots
library(ggplot2)
library(RColorBrewer)
library(scales)
library(plotly)

# shiny
library(shiny)
library(shinythemes)
library(shinyjs)
library(datasets)

# codes sources
source('code/func_moda.R')
source('code/make_acp.R')

# data
data("diamonds")
data("iris")
data("mtcars")
data("rock")

diamonds <- diamonds %>%
    tibble::add_column(id = paste0("D", 1:nrow(diamonds)), .before = 1) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("id")

diamonds <- diamonds %>%
    tibble::add_column(id = paste0("D", 1:nrow(diamonds)), .before = 1)
summary(diamonds)

# dims
diamonds <- diamonds[1:2000,]
iris <- iris[1:150,]
mtcars <- mtcars[1:32,]
rock <- rock[1:48,]


