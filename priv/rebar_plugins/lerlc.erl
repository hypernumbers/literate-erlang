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
    Files = [filelib:wildcard(X ++ "/*.erl.md") || X <- SrcDirs],
    [ok = literate_compile(X, CompilerOptions) || X <- Files],
    ok.

literate_compile(File, CompilerOptions) ->
    CWD = rebar_utils:get_cwd(),
    {ok, Lines} = read_lines(CWD ++ "/" ++ File),
    Source = make_erlang_source(Lines),
    ok = write_source_and_compile(Source, File, CompilerOptions).

make_erlang_source(Lines) ->
    make_erl2(Lines, comment, []).

make_erl2([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
make_erl2(["```erlang" ++ _Rest | T], comment, Acc) ->
    make_erl3(T, erlang, ["%%%```erlang\n" | Acc]);
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
    case proplists:get_value(src_dirs, ErlOpts) of
        undefined -> ["src"];
        SrcDirs   -> SrcDirs
    end.

get_compiler_options(ErlOpts) ->
    proplists:delete(src_dirs, ErlOpts).

write_source_and_compile(Source, File, CompilerOptions) ->
    File2 = filename:basename(filename:rootname(File)),
    Dir = filename:dirname(File),
    NewCompOpts = adjust_output_dirs(CompilerOptions, Dir),
    Dir2 = Dir  ++ "/.erl/",
    ok = filelib:ensure_dir(Dir2),
    ok = file:write_file(Dir2 ++ File2, Source),
    {ok, _} = compile:file(Dir2 ++ File2, NewCompOpts),
    ok.

adjust_output_dirs(CompilerOptions, Dir) ->

    case proplists:is_defined(outdirs, CompilerOptions) of
        false ->
            OutputDir = "ebin/",
            filelib:ensure_dir(OutputDir),
            [{outdir, OutputDir} | CompilerOptions];
        true  ->
            CompilerOptions
    end.

