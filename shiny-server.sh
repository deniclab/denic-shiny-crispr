#!/bin/bash

R -e "shiny::runApp('app.R', port=3838, host='0.0.0.0')"
