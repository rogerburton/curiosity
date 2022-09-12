---
title: Curiosity
---

# Changelog

## 2022-09-12

Any user or business unit has a public profile page visible at `/username` or
`/unitname`. The code was mostly done in the past, but it was difficult to
combine it with serving the static assets or documentation. This is now done.
[PR-73](See https://github.com/hypered/curiosity/pull/73).

A long-standing issue related to the log file was resolved. It appeared when
the log file was supposed to be rotated (after growing past some limit), but
the number of rotated log files to be kept was set to zero. Increasing that
number solved the problem. See
[PR-74](https://github.com/hypered/curiosity/pull/74).

## 2022-09-09

Start "user actions", with the email address to be verified as a first action.

- This change the rather crude homepage to a nicer datagrid (to display user
  profiles with an email address waiting to be verified).
- The datagrid has links to let user perform actions.
- A dedicated page to actually perform the action is added. (This acts as a
  confirmation page.)
- See [PR-61](https://github.com/hypered/curiosity/pull/61).

First attempt at creating an EDSL (Embedded domain-specific language). When
`cty run` was demonstrated, it was asked if it supported `if/then/else`
statements (it doens't). In the case something more powerful than `cty run` is
needed, one option would be to offer an EDSL. The "E" means that the "language"
is provided within a host language, in our case Haskell.

- An example script using the EDSL is [visible in the
  PR](https://github.com/hypered/curiosity/pull/64/files#diff-e1ec2e690f8583bcd58ec147dcad8962ab1492d6ddc33e6f4fa444a0b9e8c267R61-R76).
- It might be useful in the future to describe business rules a clearly as
  possible, while making them computer-readable (and executable).
- See [PR-64](https://github.com/hypered/curiosity/pull/64).

Add the main concepts of the prototype: legal entities, business units,
employment contracts, and invoices.

- Add HTML views, with exemple JSON files.
- Update the documentation to refer to them.
- Begin a form to create a contract. The state of the form is kept within the
  server, which allows us to have multiple pages if necessary. This is used
  here to add/edit/delete expenses as part of the main contract. (The form is not
  yet complete.)
- Introduce the idea of a set, called `state-0`, of well-known example objects
  that can be manipulated by test scenarios. See the
  [documentation](https://smartcoop.sh/documentation/scenarios).
- See [PR-63](https://github.com/hypered/curiosity/pull/63),
  [PR-68](https://github.com/hypered/curiosity/pull/68), and
  [PR-72](https://github.com/hypered/curiosity/pull/72).

Start to expose some data as UBL (in JSON). An example of legal entity can be
viewed as a UBL `PartyLegalEntity`.

- The structure of that representation is taken from
  [issue 39](https://github.com/hypered/curiosity/issues/39).
- See the example in the
  [documentation](https://smartcoop.sh/documentation/ubl).
- See [PR-65](https://github.com/hypered/curiosity/pull/65).

Add [Brotli compression](https://en.wikipedia.org/wiki/Brotli) to Nginx. This
was done as part of improvements to the [web.dev](https://web.dev/measure/)
metrics but was not yet merged.

- See [PR-66](https://github.com/hypered/curiosity/pull/66).

Some improvements to the code.

- See [PR-67](https://github.com/hypered/curiosity/pull/67) and
  [PR-58](https://github.com/hypered/curiosity/pull/58).

An auto-reload mechanism used during development is added.

- When used, a change to the source code causes an open web page to be
  automatically reloaded.
- See [PR-69](https://github.com/hypered/curiosity/pull/69).

Improve the `cty` command-line.

- Some features supported by `cty` are now available in `cty run` or `cty repl`
  and vice versa.
- In particular, `cty run` supports `run`ning scripts (i.e. a script can call
  other scripts). This is used in a script to set the system to `state-0`,
  mentioned above.
- See [PR-70](https://github.com/hypered/curiosity/pull/70).

Add a script to find broken links on [`smartcoop.sh`](https://smartcoop.sh).

- See [PR-71](https://github.com/hypered/curiosity/pull/71).

## 2022-08-23

A regular user (by opposition to a system user) is now available in the
virtual machine image.

- The user has access to the Curiosity binaries, Bash completion and man
  pages.
- The default options for `cty serve` and the options for the `cty serve`
  service are changed to no longer overlap (e.g. the default listen port is
  9000, but the service is configured to use 9100).
- There is now a `play.smartcoop.sh` domain, and Nginx is configured to
  forward its requests to port 9000. I.e. when the user decides to run
 `cty serve`, it's possible to access it on that new domain.
- `jq` is also available to further process some `cty` output.
- See [PR-52](https://github.com/hypered/curiosity/pull/52/) and following
  commits.

`cty run` and related commands are improved to be easier to use and more
useful, in particular to write golden tests.

- Scenarios can contain comments, starting with the `#` character.
- The output of `cty run` now shows the input line numbers and the commands
  being run.
- `cty user get` accepts a `--short` option to make its output less verbose.
- `cty run` understands a `reset` command, to make the state empty.
- Internally, new users are appended at the end of the list of users (instead
  of at the beginning).  This makes for a nicer default when displaying the
state.
- A new `cty users` command is added to filter users.
- `cty run` understands an `as` command, to change the default user for the
  next commands in the script.
- Enable some completion: for the filenames for `cty run`, and for user IDs
  (the later is really just a helper, and is not dependant on an actual state).
- See [PR-53](https://github.com/hypered/curiosity/pull/53/).

Introduce [golden
testing](https://ro-che.info/articles/2017-12-04-golden-tests), based on the
behavior of `cty run`.

- The underlying code of `cty run` can be used with the `tasty-golden` library.
- Scripts under the `scenarios/` directory now have an expected output (using
  the `.golden` extension).
- A new test program runs all scenarios, ensuring their output match the golden
  files.
- See [PR-54](https://github.com/hypered/curiosity/pull/54/).

Add concepts of business entities, legal entities, invoices, and employment
contract.

- Those contain only an ID for now.
- Commands to create instances of those concepts are added to `cty`, e.g.
  `cty invoice`.
- See [PR-56](https://github.com/hypered/curiosity/pull/56/).

Various improvements to the web pages, before we can expose additional features
already present in the CLI (e.g. set an email address as verified, or create a
new invoice).

- Add a dividing line in the user menu.
- Show the logged in username in the user menu.
- Fix the hamburger menu on the Markdown-based pages.
- The whole settings pages (including the main navigation header) are
  scrollable.
- Rename "logout" to "sign out".
- Add a side menu to the settings page.
- Make the main navigation header mobile friendly.
- See [PR-57](https://github.com/hypered/curiosity/pull/57/).

Improve some metrics, as measured by [web.dev](https://web.dev/measure/). The
first measures were 68, 93, 100, 85 (Performance, Accessibility, Best
Practices, SEO). Now they are 97, 100, 100, 100.

- Some static assets are now served directly by Nginx instead of its
  `cty serve` backend.
- Enable gzip compression.
- Remove unused JS and CSS files.
- Add a "description" meta tag and improve contrast of the footer link (this
  improves the SEO score).
- See also the previous PR.

## 2022-08-16

- Various elements usually required of any business application:
  - Add a simple "right" to user profiles to allow to "verify" email addresses.
    Without the right, the user cannot do the operation.
  - Start the concept of "queues", where actions waiting to be taken, either by
    users or by the system can be seen.
  - Add an operation to run the waiting actions.
  - Improve some `cty` aspects.
  - The above is mostly done on the CLI level, except for `/` route, now
    demonstrating the waiting actions (user email verification for now).
  - Add public profile view, and an "introspection" view.
  - See [PR-50](https://github.com/hypered/curiosity/pull/50/).
- Improve JSON generation by hiding null fields. See
  [PR-46](https://github.com/hypered/curiosity/pull/46).
- Improve the handling of data store transactions. See
  [PR-47](https://github.com/hypered/curiosity/pull/47).
- Improve build times by better filtering what is source code or not. See
  [PR-48](https://github.com/hypered/curiosity/pull/48).
- Offer a Docker image. See
  [PR-49](https://github.com/hypered/curiosity/pull/49).


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
