docker pull tdenecker/monet
echo docker stop MONET >> MONET.bat
echo docker run --rm -p 3838:3838 -v %CD%:/srv/shiny-server -v %CD%\log:/var/log/shiny-server --name MONET tdenecker/monet > MONET.bat
