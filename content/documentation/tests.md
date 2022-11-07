---
title: Curiosity
---

# Tests

Testing an application is an important process to ensure it behaves as
intended. This can be done manually or with an automated test suite, and
various ways of writing a test suite are possible.

Curiosity uses an approach where scripts understood by its
[CLI](/documentation/clis) are run, and their output compared to [golden
files](https://ro-che.info/articles/2017-12-04-golden-tests).

All the scenarios are located in the [`/scenarios`
directory](https://github.com/hypered/curiosity/tree/main/scenarios) within the
GitHub repository. They are also [listed](/documentation/scenarios#scenarios)
in the documentation from where they can be run and explored.

Because commands understood by `cty run` map to operations that can be carried
away by the web interface and vice versa, such scripts provide a
straightforward, but precise way to describe and run scenarios. And as we will
see, by making sure their output match expected results, they can also be used
to ensure the system behaves as intended.

In the rest of this section, we go into some details of their usage.

# `cty run` scripts

Let's use the scenario called
[`0.txt`](https://github.com/hypered/curiosity/blob/main/scenarios/0.txt),
reproduced here:

<pre><code><!--# include virtual="/scenarios/0.txt" --></code></pre>

It can be executed by the `cty run` command:

<pre><code>$ cty run scenarios/0.txt
<!--# include virtual="/scenarios/0.golden" --></code></pre>

In the web interface, we can also show the same execution, but with additional
links to intermediate states:

<!--# include virtual="/partials/scenarios/0" -->

Let's comment what's happening. Each command in the source script is visible in
the output of `cty run`, prefixed with its line number. Because the second line
in the script is a comment (and thus there is nothing the execute), we can see
that the ouput jumps from line 1 to line 3.

After each command, `cty run` displays the output of that particular command,
exactly in the same way the same command run with `cty` would do. Those lines
are not prefixed by any number.

In this particular script, each command produces only one line of output, but
some commands produce more.

Note: It is important to understand that all the output of a single command
correspond to state changes that happen atomically: either the command produces
some effect and eveything is reflected in that output, or if the system crash
in the middle of the command, then no change at all will be applied to the
state.

Finally, in the web rendering of the output, after each command, there is a
link to explore the full state of the system, exactly as it was right after
that command.

Note: When run in the web interface, the script is provided its own state and
it doesn't affect the main state of the web application.

Note: The `quit` command is not necessary to end a script, but it can be useful
to be inserted within a script when working on it, to temporarily "skip" the
rest of the script.

# Golden files

In the [source
directory](https://github.com/hypered/curiosity/tree/main/scenarios), for each
script (ending with a `.txt` file extension), there is a corresponding golden
file (ending with a `.golden` file extension).

The content of a golden file is supposed to be exactly the same as the ouptut
of running the corresponding script; i.e. the content of
[`0.golden`](https://github.com/hypered/curiosity/blob/main/scenarios/0.golden)
should be the same as what you can see above.

To make sure this is the case, especially as the project evolves, an [automated
test](https://github.com/hypered/curiosity/blob/main/tests/run-scenarios.hs)
lists all the scenarios, executes them, and checks their output against the
golden files.

Note: In the above example, we see that creating a new User object is reflected
by the display of its ID. We also see that if necessary, we can use additional
command to "read" interesting bits of data from the system, ensuring that they
are as we expect them, e.g. that the ID `USER-1` is indeed associated to the
`alice` username.

# Happy paths and alternate paths

The above scenario is very short and has also another characteristic: it is
called a [happy path](https://en.wikipedia.org/wiki/Happy_path); everything
goes smoothly and no validation rule fails.

In contrast, if we also want to make sure the system behaves as intended in
some other conditions, we can also explore alternative path.

We now show a scenario that fails a validation rule regarding the [valid
usernames](/documentation/validation-data#users).

The scenario is called
[`1.txt`](https://github.com/hypered/curiosity/blob/main/scenarios/1.txt),
reproduced here:

<pre><code><!--# include virtual="/scenarios/1.txt" --></code></pre>

Again, it can be executed by the `cty run` command:

<pre><code>$ cty run scenarios/1.txt
<!--# include virtual="/scenarios/1.golden" --></code></pre>

And can be displayed in the web interface as a nice table:

<!--# include virtual="/partials/scenarios/1" -->

This scenario checks that the username `about` cannot be used when creating a
new user. Indeed the golden file ensures that the `UsernameBlocked` error
condition is reported.

Note: Curiosity shows user profiles at the `/<username>` route (e.g. see
[Alice's profile](/alice)), so a username like `about` would clash with the
real [About](/about) page. Even if such a page didn't exist, we would still
need to prevent users from creating pages that may look like official Curiosity
pages when they are not.

# Coverage

# See also

- [Scenarios](/documentation/scenarios)
