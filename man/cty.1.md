% CTY(1) Version 0.0 | Curiosity documentation

NAME
====

**cty** — interacts with a local Curiosity server or a state file.

SYNOPSIS
========

| **cty** (\[**-s**|**--state** _FILEPATH_] | \[**-t**|**--socket** _FILEPATH_]) (COMMAND | ID)
| **cty** \[**-h**|**--help**]

DESCRIPTION
===========

**cty** is the main server-side program to interact with a Curiosity server. It
can also be used to interact with a state file.

Options
-------

-h, --help

:   Prints brief usage information.

FILES
=====

*state.json*

:   The default state file name. Also by default, a state file is used.

BUGS
====

See GitHub Issues: <https://github.com/hypered/curiosity/issues>

CURIOSITY
=========

Part of the curiosity(7) suite

SEE ALSO
========

**curiosity(7)**
