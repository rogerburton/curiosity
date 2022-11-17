---
title: Curiosity
---

# Time

In the [tests documentation page](/documentation/tests), we explain how
Curiosity is using textual scripts and golden files to describe and tests the
system's behavior. In the [queues documentation page](/documentation/queues),
we show how the `queues` and `step` commands can be used to describe and test
automated processes, by observing task queues and "stepping" through them.

Some tasks however have to be delayed or be enqueued only in the future, e.g.
when it's time to send a reminder to someone, after some time has passed.

Again, with the goal of having scenarios that can describe and test a wide
range of behaviours, it is necessary to simulate the advance of time.

Here we show how the `time` command can be used to observe and manipulate a
simulated time.

Consider the following scenario:
[`time.txt`](https://github.com/hypered/curiosity/blob/main/scenarios/time.txt),
reproduced here:

<pre><code><!--# include virtual="/scenarios/time.txt" --></code></pre>

Here is how it looks like when run with `cty run`:

<pre><code>$ cty run scenarios/user-signup--blocked.txt
<!--# include virtual="/scenarios/time.golden" --></code></pre>

And here it is as a nice table:

<!--# include virtual="/partials/scenarios/time" -->

This scenario shows that initially, the time is set to 00:00:00 UTC on 1
January 1970. Internally, the time is recorded as [Epoch
time](https://en.wikipedia.org/wiki/Unix_time), and its initial value is 0.

Then it shows that `time --step` and `time --step --minute` can be used to
advance the time, either by one second, or until the "next minute".

As often in our scenarios, we issue a command to observe (part of) the current
state: `time`.

# See also

- [Tests](/documentation/tests)
- [Queues](/documentation/queues)
