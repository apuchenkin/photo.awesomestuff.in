#/bin/bash

pushd static/gallery
gm \
mogrify \
-output-directory ../thumb \
-create-directories \
-resize 320x200 \
*/*.jpg
popd 
