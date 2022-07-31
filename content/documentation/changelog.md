---
title: Curiosity
---

# Changelog

## 2022-07-29

This is the initial entry in this changelog. This is mostly a setup phase where
we tried to have a basis onto which we can implement future "business" feature
requests. URLs and links are the one existing when this entry is written, and
may become obsolete as the project evolves.

- Project
  - Setup repositories: `smart-design-hs`, `commence`, and `curiosity` (the
    last one is the main repository, the other are supporting libraries).
  - Buy and setup `smartcoop.sh`.
  - Rent a VM at DigitalOcean. 
  - Automate the deployment of `smart-design-hs` to `design.smartcoop.sh`.
  - Scripts to deploy to our virtual machine image to `smartcoop.sh`. See demo
    environment below.
  - A local Nix shell is available with our binaries, Bash completion, and man
    pages to interact with them.
  - Expose useful things in `default.nix`, e.g. the Haddock documentation.

- Web site structure
  - [`/`](/): Landing page and homepage (i.e. when the user is logged in).
  - [`/about`](/about): About page.
  - [`/documentation`](/documentation): A set of Markdown files.
    - We made it possible to drop the `.html` from URLs.
  - `/views`: Shows our data in HTML. This uses the data from `/data`. See
    documentation.
  - `/forms`: Shows our HTML forms. See documentation.
  - `/messages`: Shows our feedback messages, upon user actions. See
    documentation.
  - `/data`: Shows our example JSON data. See documentation.
  - The complete internal state is available at `/state` and `/state.json`.
  - `robots.txt`, `humans.txt`, `security.txt` are present.
  - This re-uses the Smart design system.
  - This changelog.

- Basic user management
  - User sign up (username, password, email address). No validation yet.
    No confirmation email.
  - User login. No logout yet.
  - User profile page that shows the above information, plus a display name.
  - Edit user profile page. This actually only changes the password, where it
    should be the display name instead.

- Command-line interfaces
  - `cty serve` is the web application.
  - `cty` is a command-line application designed to perform most of the same
    actions available on the web site. It can run those actions eitheir against
    its own local state, or the state of a running application.
  - `cty run` runs a script containing REPL commands.
  - `cty-sock` is intended as a target of `cty`, without running a web
    interface.
  - `cty parse` parses REPL commands.
  - `cty repl` allows to run multiple commands in a single session. This mostly
    avoids repeating the `cty` program name.
  - `cty-shell` is a small program running behind SSH, for the `curiosity`
    user. It exposes only some Curiosity-related command.
  - The syntax used in the REPL is the same as the `cty` commands themselves.
  - The complete internal state is available with `cty state`.

- Virtual machine image
  - Create a `configuration.nix` file to specify the content of our image.
  - The VM contains our binaries.
  - The VM contains our man-pages.
  - Bash completion for our binaries is setup within the VM.
  - The VM runs `cty serve`.
  - The VM runs `Nginx` as a reverse-proxy, in front of `cty serve`.
  - When running localy, ports 22 and 80 are forwareded to the host.
  - Create a `curiosity` user on the VM to be able to run `cty`, and in
    particular interact with `cty-sock`. This is currently wired to `cty parse`
    instead. TODO Document this and other things.
  - SSH configuration (using `cty-shell` and `ForceCommand`) to expose the
    above `curiosity` user.

- Demo environment
  - `smartcoop.sh` exposes a virtual machine (using the image defined above)
    running at DigitalOcean.
