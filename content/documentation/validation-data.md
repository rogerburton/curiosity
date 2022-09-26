---
title: Curiosity
---


# Validation data

Validation data are driving the system behavior, similarly to its source code
(validation data can actually be hard-coded within the source code, but not
necessarily). They are considered part of the system (i.e. a change in the
source code or the validation data would both result in a new system version
number).

## Users

A list of usernames that cannot be used:

<!--# include virtual="/partials/username-blocklist" -->

The list [as JSON](/partials/username-blocklist.json).

## Simple contracts

Roles

:   Roles mentioned in contracts come from a closed list:

    <!--# include virtual="/partials/roles" -->

    The list [as JSON](/partials/roles.json).

Countries

:   Countries mentioned in contracts come from a closed list:

    <!--# include virtual="/partials/countries" -->

    The list [as JSON](/partials/countries.json).

# See also

- [state-0](/documentation/state-0)
- [Validation rules](/documentation/validation)
- [Live data](/documentation/live-data)
