#! /usr/bin/env bash

# Note that the image name is actually an image ID (returned after the image
# has been imported), and not the image name chosen at import time.

doctl compute droplet create curiosity-1 \
  --region ams3 \
  --image 110021225 \
  --size s-1vcpu-1gb \
  --ssh-keys 98:9a:9b:21:66:1e:b2:7f:35:58:d7:ea:ca:3e:64:bd \
  --wait


# Assign the Droplet (using its ID) to the 'curiosity' project
# doctl projects resources assign 19c42aa1-1f81-407c-9d17-13960cc7a9ea --resource=do:droplet:303249027
