literate.lerl by Gordon Guthrie <gordonguthrie@vixo.com>

This is a test file for writing literate erlang

Copyright Gordon Guthrie

It was created on the 2nd September 2013

```erlang
    -module(literate).
```

This module exports two functions  - one generates a timestamp in the form of integer seconds since the Unix epoch and the other converts a timestamp in that format into a conventional Erlang date-time value.

```erlang
    -export([
             timestamp_to_date/1,
             get_timestamp/0
            ]).
```

Let us define some constants that we will use

```erlang
    -define(MEGA, 1000000000000).
    -define(SEC,  1000000).
```

Finally the two functions

```erlang
    -spec get_timestamp() -> pos_integer().
    get_timestamp()->
        {Mega, Sec, Micro} = now(),
        ?MEGA * Mega + ?SEC * Sec + Micro.

    timestamp_to_date(Stamp) when is_list(Stamp) ->
        timestamp_to_date(list_to_integer(Stamp));
    timestamp_to_date(Stamp) when is_integer(Stamp) ->
        Mega = trunc(Stamp/?MEGA),
        Sec = trunc((Stamp - Mega * ?MEGA)/?SEC),
        Micro = Stamp - Mega * ?MEGA - Sec * ?SEC,
        {Mega, Sec, Micro}.

