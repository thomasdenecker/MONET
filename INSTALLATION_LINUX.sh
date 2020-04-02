#!/bin/bash

docker pull tdenecker/monet

BASEDIR=$(pwd)
echo "$BASEDIR"

echo '#!/bin/bash' > $BASEDIR/MONET.sh
echo 'docker run --rm -p 3838:3838 -v' $BASEDIR':/srv/shiny-server tdenecker/monet' >> $BASEDIR/MONET.sh

chmod +x $BASEDIR/MONET.sh
