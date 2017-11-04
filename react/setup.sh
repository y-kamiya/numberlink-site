#!/bin/bash

docker-compose run node npm install

rm public/assets/css/bootstrap-arrows.css
ln -s $(pwd)/libs/bootstrap-arrows/css/bootstrap-arrows.css public/assets/css/bootstrap-arrows.css

rm public/assets/js/bootstrap-arrows.js
ln -s $(pwd)/libs/bootstrap-arrows/js/bootstrap-arrows.min.js public/assets/js/bootstrap-arrows.js

rm public/assets/css/bootstrap.css
ln -s $(pwd)/node_modules/bootswatch/cerulean/bootstrap.min.css public/assets/css/bootstrap.css

rm public/assets/js/jquery.js
ln -s $(pwd)/node_modules/jquery/dist/jquery.min.js public/assets/js/jquery.js

rm public/assets/js/bootstrap.js
ln -s $(pwd)/node_modules/bootstrap/dist/js/bootstrap.min.js public/assets/js/bootstrap.js

docker-compose run node npm run build
