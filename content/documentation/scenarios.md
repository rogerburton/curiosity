---
title: Curiosity
---

# Test scenarios

One the main goal of Curiosity is to precisely describe its behavior in terms
of test scenarios. To do so, Curiosity supports running scripts: they are
textual sequences of commands that mimic closely what can be done through the
web interface.

As part of Curiosity's test suite, a collection of scenarios are run and their
results are compared to known sources of truth, called "golden files". This
makes sure that, as scenarios are created, and as Curiosity evolves, the system
continues to work as expected, and few bugs can be introduced inadvertantly.

# Example data

Entering enough data into the system to allow to execute (possibly manually,
through the web interface) a specific test scenario can be tedious if the
system is initially empty. Fortunately, scripts can also be used to generate
the necessary data and set the system state as desired.

A specific set of example data is available with Curiosity, called
[`state-0`](/documentation/state-0). (We envision that additional set of data
could be created in the future.)

# Validation data

The behavior of the system (in particular regarding its validation rules) is
often dependent on static data: data that are considered part of the system
definition. Those data can be for instance hard-coded in the source code. The
data driving the system are visible and explained in the documentation in the
[validation data page](/documentation/validation-data).
