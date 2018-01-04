# denic-shiny-crispr
R/Shiny visualization of CRISPR screening data from Shoemaker et al. 2017

__Manuscript authors__: Christopher J. Shoemaker, Tina Q. Huang, Nicholas R. Weir, Nicole Polyakov, Vladimir Denic  

__Shiny app authors__: Nicholas R. Weir and Christopher J. Shoemaker

## To access web-hosted version of this app:
Navigate to [crispr.deniclab.com](http://crispr.deniclab.com).

## To run the app locally on your computer:
- Install [RStudio](www.rstudio.com)
- Ensure you have all necessary R libraries installed (see requirements.txt)
- Clone the repository
- Open app.R in RStudio and click "Run App" at the top

## To build the app as a Docker container:

- Install [Docker](https://docs.docker.com/engine/installation/)
- For Debian and RHEL based Linux systems, you can install Docker by following these steps:

`$ curl -fsSL https://get.docker.com/ | sh`

`$ sudo systemctl start docker`

Verify that it's running:

`$ sudo systemctl status docker`

Make sure that docker will start on boot:

`$ sudo systemctl enable docker'

Add your username to the docker group to avoid typing "sudo" before docker commands:

`$ sudo usermod -aG docker $(whoami)

- To build and run the container on most any system, follow the steps below. You will need to have Docker and git installed.

First, clone the repo to your machine:

`$ git clone https://github.com/deniclab/denic-shiny-crispr`

Then, to build the container, run:

`$ docker build -t denic-shiny-crispr .`

Finally, run the container:

`$ docker run -p 80:3838 denic-shiny-crispr`

_Questions or problems?_ nweir [at] fas d ot harvard dot edu, or submit an issue.
