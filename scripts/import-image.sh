#! /usr/bin/env bash

# Import an image into Digital Ocean from an URL. The URL here is from an S3
# bucket where we have uploaded the image.

doctl compute image create curiosity \
  --region ams3 \
  --image-url https://hypered.ams3.digitaloceanspaces.com/curiosity.qcow2.gz \
  --image-description 'Curiosity, a prototype for Smart'
