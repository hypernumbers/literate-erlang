<img src='https://raw.github.com/hypernumbers/literate-erlang/master/priv/images/literate-erlang.png' />

Literate Erlang
===============

A compiler for Literate Erlang - inspired by Literate CoffeeScript.

Why?
----

The purpose of this compiler is to help new users unfamiliar with Erlang to more easily understand Erlang code **on GitHub** by supporting a literate style.

This is intended to reduce the barriers to new users joining the community.

Implementation
--------------

The Literate Erlang, like Literate CoffeeScript is blocks of Markdown interpolated with indented blocks of Erlang. This particular dialect is aimed at GitHub-flavoured Markdown.

The Literate Erlang compiler is implemented as a ``rebar`` plugin. For more details of ``rebar`` and its role in Erlang development see https://github.com/rebar/rebar.

Literate Erlang files end with ``.erl.md`` or ``.hrl.md`` and are stored in the ``/src_md`` and ``/include_md`` directories alonside the ``/src`` and ``/include`` directories as part of a normal Erlang/OTP file structure.

Details of the normal erlang directory structure can be found in the Erlang Design principles document http://www.erlang.org/doc/design_principles/applications.html#id73971

The compiler is implemented as a rebar pluging - the source code for it is in the directory ``/priv/rebar_plugins``.

The compiler works by transforming the Literate Erlang to plain erlang and then compiling that.

Usage
-----

Write your literate-erlang files in the directory ``/src_md`` naming them like ``mymodule.erl.md``. Header files are written in ``/include_md`` and named ``myheder.hrl.md``.

Compile them to normal erlang and onto beam files using:
``rebar literate-compile``

Reverse Compiler
================

Why?
---

The tooling for literate Erlang is immature and will likely to remain that way for a long term. The reverse compiler enables authors of Erlang to write using their normal tools (ie emacs with erlang-mode or sublime-text with erlang mode) - but publish the results to their communities as Literate Erlang on GitHub.

Implementation
--------------

The reverse compiler implmented as a ``rebar`` plugin called ``markup``.

This turns all files ending ``.erl`` in the ``/src_md/.erl`` directory into markdown files which are placed in an `/src_md` directory and all ``.hrl`` files in ``include_md/.hrl`` directory into ``.hrl.md`` filed in ``/include_md``.

Usage
-----

The canonical version under version control is the markdown files in the ``/src_md`` and ``/include_md`` directories. So the first step is to compile them to erlang:
``rebar compile_literate``

Then copy the ``.erl`` files to the directory ``src_md/.erl`` and the `..hrl`` files to ``/include_md/.hrl`` and edit them there.

The production of working beam files is then:
``rebar markup``
``rebar literate_compile``

Baby Steps
==========

This is the simplest possible implementation of a Literate Erlang compiler designed to establish the idea and **get it working on GitHub**. There are plenty of ways it could be improved.

* It has bugs - for instance (at the moment) you can't embed blocks of other code in the Erlang file (an example would be a javascript example in a module that parses JSON).
* it has no tooling - no SublimeText plugins or Emacs major modes - they would be nice
* it has no native Erlang markup compiler to turn it into html. There is an Erlang Markdown compiler (https://github.com/erlware/erlmarkdown) which could be used to do that. It currently supports normal Markdown and not GitHub-flavoured markdown.

The markdown emacs mode results in code that looks like this:
<img src='https://raw.github.com/hypernumbers/literate-erlang/master/priv/images/emacs-tooling-literate-erlang.png' />

Sublime Text has a markdown mode which allows you to preview your markdown in a brower:
<img src='https://raw.github.com/hypernumbers/literate-erlang/master/priv/images/sublime-text-3-python-preview.png' />

Users on Windows/Macs can have this preview rendered by GitHub for a higher level of fidelity.

Contributing
------------

Contributing is easy, fork and fire away.
