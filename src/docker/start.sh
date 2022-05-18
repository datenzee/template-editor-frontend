#!/bin/sh

# create config
config=/usr/share/nginx/html/config.js
echo -n "window.templateEditor={dswApiUrl:'"$DSW_API_URL"',teApiUrl:'"$TE_API_URL"'}" > ${config}

# check if customizations exist
if [[ $(find /src/scss/customizations -name "*.scss" | xargs cat | wc -l) -gt 0 ]]; then
  # regenerate styles
  echo '$fa-font-path: "";' >> /src/scss/customizations/_variables-app.scss
  find /usr/share/nginx/html -name "*.css" -exec sassc -I /src -t compressed /src/scss/main.scss {} \;
fi

# start nginx
nginx -g 'daemon off;'
