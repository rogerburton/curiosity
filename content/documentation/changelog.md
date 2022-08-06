---
title: Curiosity
---

# Changelog

## 2022-08-06

- Introduce a `Login` data type, instead of re-using the `Credentials` one. The
  user profile has been enriched with some fields. Those are supposed to be
  checkd or verified, leading to different access rights. They are visible in the
  user profile view, and additional example data are provided. See
  [PR-42](https://github.com/hypered/curiosity/pull/42) and a [new
  documentation page](/documentation/objects/users).
- Streamline some `cty` usage: some commands have nicer outputs or error
  reporting, and now support an explicit `--memory` option to not read or write
  any file, or talk to a UNIX-domain socket. See
  [PR-43](https://github.com/hypered/curiosity/pull/43).
- Beginning of a test suite for the library code. See
  [PR-44](https://github.com/hypered/curiosity/pull/44).

## 2022-08-02

- Expose the [Haddock documentation](/haddock/), add a favicon. See
  [PR-41](https://github.com/hypered/curiosity/pull/41).

## 2022-08-01

- Fix the User Id generation, used in the signup form and the `cty user create`
  command. See [PR-35](https://github.com/hypered/curiosity/pull/35).
- Add a blocklist for usernames. This is interesting because it shows how to
  use hard-coded data both in some business-logic and show those same data in
  the documentation. See [PR-36](https://github.com/hypered/curiosity/pull/36).
- The `smartcoop.sh` domain is now available in HTTPS. See
  [PR-37](https://github.com/hypered/curiosity/pull/37).

## 2022-07-31

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
  - User login.
  - User logout.
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
