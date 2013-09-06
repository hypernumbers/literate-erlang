%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is a rebar plugin for turning Erlang sourc
%%%            into literate Erlang
%%%
%%% @end
%%% Created :  2 Sep 2013 by gordon@vixo.com
-module(markup).

-export([
         markup/2
        ]).

markup(Config, _AppFile) ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    SrcDirs = get_src_dirs(ErlOpts),
    Files = lists:merge([filelib:wildcard(X ++ "/*.erl") || X <- SrcDirs]),
    FilterFun = fun(X) ->
                        not filelib:is_dir(X)
                end,
    Files2 = lists:filter(FilterFun, Files),
    [ok = markup_to_literate(X) || X <- Files2],
    ok.

markup_to_literate(File) ->
    CWD = rebar_utils:get_cwd(),
    {ok, Lines} = read_lines(CWD ++ "/" ++ File),
    Source = make_markdown_source(Lines),
    ok = write_source(Source, File).

make_markdown_source(Lines) ->
    make_md2(Lines, erlang, []).

make_md2([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
%%% preserve (and normalise) the erlang markups
%% order matters!
make_md2(["%%%```erlang" ++ _Rest | T], comment, Acc) ->
    make_md3(T, erlang, ["%%%```erlang\n" | Acc]);
make_md2(["%%```erlang" ++ _Rest | T], comment, Acc) ->
    make_md3(T, erlang, ["%%%```erlang\n" | Acc]);
make_md2(["%```erlang" ++ _Rest | T], comment, Acc) ->
    make_md3(T, erlang, ["%%%```erlang\n" | Acc]);
%% order matters!
make_md2(["%%%" ++ Rest | T], erlang, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md2(["%%" ++ Rest | T], erlang, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md2(["%" ++ Rest | T], erlang, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md2(["    " ++ Rest | T] = L, erlang, Acc) ->
    make_md3(L, markdown, Acc);
make_md2([H | T], erlang, Acc) ->
    make_md2(T, erlang, [H | Acc]).

make_md3([], _Type, Acc) ->
    lists:flatten(lists:reverse(["%%%```" |  Acc]));
make_md3(["\n" | T], markdown, Acc) ->
    make_md3(T, markdown, ["\n" | Acc]);
make_md3(["```" ++ _Rest | T], markdown, Acc) ->
    make_md2(T, erlang, ["\n" | Acc]);
%% order matters!
make_md3(["%%%" ++ Rest | T], markdown, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md3(["%%" ++ Rest | T], markdown, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md3(["%" ++ Rest | T], markdown, Acc) ->
    make_md3(T, markdown, [Rest | Acc]);
make_md3([H | T], markdown, Acc) ->
    make_md2(T, erlang, ["    " ++ H | Acc]).

read_lines(File) ->
    case file:open(File, read) of
        {error, Err} -> {error, Err};
        {ok, Id}     -> read_l2(Id, [])
    end.

read_l2(Id, Acc) ->
    case file:read_line(Id) of
        {ok, Data}   -> read_l2(Id, [Data | Acc]);
        {error, Err} -> {error, Err};
        eof          -> {ok, lists:reverse(Acc)}
    end.

get_src_dirs(ErlOpts) ->
    case proplists:get_value(src_dirs, ErlOpts) of
        undefined -> ["src"];
        SrcDirs   -> SrcDirs
    end.

write_source(Source, File) ->
    File2 = filename:basename(File) ++ ".md",
    Dir = filename:dirname(File) ++ "/../md/",
    ok = filelib:ensure_dir(Dir),
    ok = file:write_file(Dir ++ File2, Source).


