docker pull tdenecker/monet
echo docker run --rm -p 3838:3838 -v %CD%:/srv/shiny-server tdenecker/monet >> MONET.bat
