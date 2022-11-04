---
title: Curiosity
---

# Queues

In the [tests documentation page](/documentation/tests), we explain how
Curiosity is using textual scripts and golden files to describe and tests the
system's behavior. There, we can see how user commands affect the system by
running them, and observing (parts of) the resulting state.

An important part of the system behavior that we want to describe and test
relates to automated processes: actions that the system can take autonomously,
e.g. sending a confirmation email after a user has registered, or sending a
reminder after some period of time.

To make it possible to also describe and test that part of the system, we
expose the concepts of _queues_ and _steps_.

Queues, short for "task queues", are similar to to-do lists: they contain a
list of tasks that can be taken off the queue and processed. We can image such
queues for tasks that can be acted upon by humans, but here we're interested in
tasks that the system can performs.

Most of the time, queues are explicit construct into which tasks are written
too. In Curiosity, we're being a bit more flexible and also visualise objects
waiting for some actions as queues as well.

For instance, sending an email is really adding a task "send email" to a
specific queue called "email-to-send". The command `cty queue emails-to-send`
shows the content of that queue.

On the other hand, user profiles with unverified email adresses don't have a
corresponding specific queue within the system, but can still be visualised
with the `cty queue user-email-addr-to-verify`.

To process queues, a command `cty step` is provided. Together with the the
above commands, we can devise scenarios to describe and test automated
processes.

Consider the following scenario:
[`signup-actions.txt`](https://github.com/hypered/curiosity/blob/main/scenarios/signup-actions.txt),
reproduced here:

<pre><code><!--# include virtual="/scenarios/signup-actions.txt" --></code></pre>

Here is how it looks like when run with `cty run`:

<pre><code>$ cty run scenarios/1.txt
<!--# include virtual="/scenarios/signup-actions.golden" --></code></pre>

And here it is as a nice table:

<!--# include virtual="/partials/scenarios/signup-actions" -->

This scenario shows that initially, queues are empty (there is no tasks that
the system can take autonomously), and that creating a new user also creates
some tasks: an email address has to be verified, and an email has to be sent.

It also shows that it is possible to execute those tasks with the `step --all`
command, or show what would be executed (but without actually doing so) with
the `--dry` option. Notice, by visiting the "View" links, how the email starts
with a state `EmailTodo`, then `EmailDone`.

Note: with `cty run`, automated processes must be explicitely run with the
`step` command. Within the web server,
["threads"](https://en.wikipedia.org/wiki/Thread_(computing)) are taking care
of those processes. They can be disabled, and the `step` command can then be
used too.

# See also

- [Tests](/documentation/tests)
- [Time](/documentation/time)
