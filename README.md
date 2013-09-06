Literate Erlang
===============

A compiler for Literate Erlang - inspired by Literate CoffeeScript.

Why?
----

The Erlang/OTP environment for building distributed reliable software is not having the success it should have - for a number of reasons.

One of them is a lack of focus on growing the community - activities and infrastructure that help new users produce working code in Erlang/OTP quickly and pleasurably.

Compare the slow growth of Erlang with the explosive growth of Node.js - with 30,000 packages available from its repositories. (Needless to say, Erlang doesn't have a package/repository manager, and worse, the community doesn't appear to think that that is a problem.)

New users from a Ruby/Java/C background often struggle with the unfamiliar Prolog-like syntax of Erlang.

The purpose of Literate Erlang is to support a more prolix and explanatory style of writing Erlang for use in public repositories like GitHub (as they used to say on the BBC, "other version control systems and services are available...").

This should help new users join Erlang projects and start contributing.

Implementation
--------------

The Literate Erlang, like Literate CoffeeScript is blocks of Markdown interpolated with indented blocks of Erlang. This particular dialect is aimed at GitHub-flavoured Markdown.

The Literate Erlang compiler is implemented as a ``rebar`` plugin. For more details of ``rebar`` and its role in Erlang development are available here. (FIX).

Literate Erlang files end with ``.erl.md`` and are stored in the ``/src`` directory as part of a normal Erlang/OTP file structure (FIX).

The compiler is implemented as a rebar pluging - the source code for it is in the directory ``/priv/rebar_plugins``. (HOW DO WE USE IT AS rebar dependency?)

The compiler works by transforming the Literate Erlang to plain erlang (it stashes the generated Erlang module in the ``src/.erl`` directory - and then compiling that.

Reverse Compiler
----------------

There is a reverse compiler implmented as a ``rebar`` plugin called ``markup``.

This turns all files ending ``.erl`` in the ``src/`` into markdown files which are placed in an `md/` directory. The reverse compiler is for people who wish to write literate erlang with their current IDE and toolsupport - but the publish the literate versions on GitHub.

Baby Steps
----------

This is the simplest possible implementation of a Literate Erlang compiler designed to establish the idea and **get it working on GitHub**. There are plenty of ways it could be improved.

* It has bugs - for instance (at the moment) you can't embed blocks of other code in the Erlang file (an example would be a javascript example in a module that parses JSON).
* it has no tooling - no SublimeText plugins or Emacs major modes - they would be nice
* it has no native Erlang markup compiler to turn it into html. There is an Erlang Markdown compiler (REF) which could be used to do that. It currently supports normal Markdown and not GitHub-flavoured markdown (or Markup, whatever happened to that?).

Contributing
------------

Contributing is easy, fork and fire away.

All contributors whose patches are accepted will receive one of these handsome T-shirts.