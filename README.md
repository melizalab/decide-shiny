
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

This should open up a browser tab pointing to the application.

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

To deploy the application, we'll use [shiny server](https://shiny.rstudio.com/articles/shiny-server.html) in a Docker container. Using Docker allows us to package all the required dependencies and isolate the server from the host system. Shiny Server should spawn processes as needed, but it's not expected that there will be a heavy load. The server should be hosted behind an nginx server, which provides encryption and authentication for connections.

There should be an image on the Docker hub that you can use by simply running:

``` shell
docker run --name=shiny_app -p 3838:3838 --user shiny -d --restart unless-stopped dmeliza/decide-shiny
```

Navigate to http://localhost:3838/track_oddball/

You can run this container as an nginx reverseproxy.
