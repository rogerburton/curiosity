---
title: Curiosity
---


# Virtual machine image

Curiosity is packaged as a virtual machine image. The image contains everything
(e.g. the web application, a reverse proxy, helper programs and documentation).

The same image can be built for different environments. For instance we use KVM
for local development and the virtual machine available at
[`smartcoop.sh`](//smartcoop.sh) for demonstration purpose is
[deployed](/documentation/deployment) at DigitalOcean.

## Built with Nix

The build system used for Curiosity is [Nix](https://nixos.org/). It ensures
that the complete system, or its different parts, can be built reliably from
source.
