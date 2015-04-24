#/bin/bash

pushd static/src

gm \
mogrify \
-output-directory ../thumb \
-create-directories \
-resize 320x200 \
*/*.jpg

gm \
mogrify \
-output-directory ../gallery \
-create-directories \
-resize x1080 \
*/*.jpg

popd 
