#!/bin/sh

#Get IP for Jupyter in Docker
chmod 777 getmyIP.py
python3 getmyIP.py

#Build image smbionet

#PUSH=true
PUSH=false

mvn clean package


build() { # $1: directory, $2: image_name
  cd $1
  docker build -t $2 .
  if [ "$PUSH" = "true" ]; then docker push $2; fi
  cd ..
}

# Build docker images
echo "Building locals resources"

build smbionetjava  totembionet/smbionet-document
build save  totembionet/save-document