
# decide-shiny

This repository is part of the [starboard/decide](https://meliza.org/starboard) project, which is a set of hardware and software for running behavioral experiments. This component is a [shiny app](https://www.rstudio.com/products/shiny/) that consumes trial data from [django-decide-host](https://github.com/melizalab/django-decide-host) to generate useful plots for tracking a subject's progress in training or testing.

This code is heavily customized for the Meliza Lab and for a specific experiment, but could serve as a useful starting point for your own analyses.

## Quick start

Install dependencies

``` R
install.packages(c("shiny", "tidyverse", "jsonlite", "httr", "ggplot2", "gtools"))
```

Run the app

``` R
library(shiny)
runApp("appdir/track_oddball")
```

This should open up a browser tab pointing to the server.

## Deploying as a Docker container

Create image:
``` shell
docker build -t melizalab/decide-shiny .
```

#### Development
(updates automatically based on changes to local repo)
``` shell
docker run --name=shiny_app --rm -p 3838:3838 --user shiny -v `pwd`:/srv/shiny-server/ melizalab/decide-shiny
```
Navigate to http://localhost:3838/appdir/track_oddball/

#### Production
``` shell
docker run --name=shiny_app --rm -p 3838:3838 --user shiny melizalab/decide-shiny
```
Navigate to http://localhost:3838/track_oddball/
