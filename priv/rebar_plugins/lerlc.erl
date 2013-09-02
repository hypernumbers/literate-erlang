%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is a rebar plugin for compiling literate erlang
%%%
%%% @end
%%% Created :  2 Sep 2013 by gordon@vixo.com
-module(lerlc).

-export([
         lerlc/2
        ]).

lerlc(Config, _AppFile) ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    SrcDirs = get_src_dirs(ErlOpts),
    CompilerOptions = get_compiler_options(ErlOpts),
    io:format("SrcDirs is ~p~n", [SrcDirs]),
    io:format("CompilerOptions is ~p~n", [CompilerOptions]),
    Files = [filelib:wildcard(X ++ "/*.lerl") || X <- SrcDirs],
    io:format("Files is ~p~n", [Files]),
    [ok = literate_compile(X) || X <- Files],
    ok.

literate_compile(File) ->
    CWD = rebar_utils:get_cwd(),
    io:format("CWD is ~p~n", [CWD]),
    {ok, Lines} = read_lines(CWD ++ "/" ++ File),
    Erlang = make_erlang(Lines),
    io:format("Erlang is ~p~n", [Erlang]),
    {ok, ErlTokens, _} = erl_scan:string(Erlang),
    io:format("ErlTokens is ~p~n", [ErlTokens]),
    {ok, ErlAbsForms} = erl_parse:parse_form(ErlTokens),
    io:format("ErlAbsForms is ~p~n", [ErlAbsForms]),
    %% fix up options, yeah
    ok = compile:forms(ErlAbsForms),
    ok.

make_erlang(Lines) ->
    make_erl2(Lines, comment, []).

make_erl2([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
make_erl2(["```erlang" ++ _Rest | T], comment, Acc) ->
    make_erl3(T, erlang, ["\n" | Acc]);
make_erl2([H | T], comment, Acc) ->
    make_erl2(T, comment, ["%%% " ++ H | Acc]).

make_erl3([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
make_erl3(["\n" | T], erlang, Acc) ->
    make_erl3(T, erlang, ["\n" | Acc]);
make_erl3(["    " ++ Rest | T], erlang, Acc) ->
    make_erl3(T, erlang, [Rest | Acc]);
make_erl3(["```" ++ _Rest | T], erlang, Acc) ->
    make_erl2(T, comment, ["\n" | Acc]);
%% Oops, not indented? lets comment out then
make_erl3(List, erlang, Acc) ->
    make_erl2(List, comment, Acc).

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
    io:format("ErlOpts is ~p~n", [ErlOpts]),
    case proplists:get_value(src_dirs, ErlOpts) of
        undefined -> ["src"];
        SrcDirs   -> SrcDirs
    end.

get_compiler_options(ErlOpts) ->
    proplists:delete(src_dirs, ErlOpts).
