%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2022. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(otp_SUITE).

-export([all/0, suite/0,
         init_per_suite/1,end_per_suite/1]).
-export([undefined_functions/1,deprecated_not_in_obsolete/1,
         obsolete_but_not_deprecated/1,call_to_deprecated/1,
         call_to_size_1/1,call_to_now_0/1,strong_components/1,
         erl_file_encoding/1,xml_file_encoding/1,
         runtime_dependencies_functions/1,
         runtime_dependencies_modules/1]).

-include_lib("common_test/include/ct.hrl").

-import(lists, [filter/2,foldl/3,foreach/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    [undefined_functions, deprecated_not_in_obsolete,
     obsolete_but_not_deprecated, call_to_deprecated,
     call_to_size_1, call_to_now_0, strong_components,
     erl_file_encoding, xml_file_encoding,
     runtime_dependencies_functions,
     runtime_dependencies_modules].

init_per_suite(Config) ->
    Server = start_xref_server(daily_xref, functions),
    [{xref_server,Server}|Config].

end_per_suite(Config) ->
    Server = proplists:get_value(xref_server, Config),
    catch xref:stop(Server),
    Config.

undefined_functions(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),

    %% Exclude calls from generated modules in the SSL application.
    ExcludeFrom = "SSL-PKIX|PKIX.*|ssl_pkix_oid",
    UndefS = xref_base:analysis(undefined_function_calls),
    Q = io_lib:format("Undef = ~s,"
                      "ExcludedFrom = ~p:_/_,"
                      "Undef - Undef | ExcludedFrom",
                      [UndefS,ExcludeFrom]),
    {ok,Undef0} = xref:q(Server, lists:flatten(Q)),

    Filters = [fun erts_filter/1,
               fun ssl_crypto_filter/1,
               fun eunit_filter/1,
               fun dialyzer_filter/1,
               fun wx_filter/1,
               fun diameter_filter/1],
    Undef = lists:foldl(fun(Filter, Acc) ->
                                Filter(Acc)
                        end, Undef0, Filters),

    case Undef of
        [] -> ok;
        _ ->
            Fd = open_log(Config, "undefined_functions"),
            foreach(fun ({MFA1,MFA2}) ->
                            ct:pal("~s calls undefined ~s",
                                   [format_mfa(Server, MFA1),
                                    format_mfa(MFA2)]),
                            io:format(Fd, "~s ~s\n",
                                      [format_mfa(Server, MFA1),
                                       format_mfa(MFA2)])
                    end, Undef),
            close_log(Fd),
            ct:fail({length(Undef),undefined_functions_in_otp})
    end.

ssl_crypto_filter(Undef) ->
    case {app_exists(crypto),app_exists(ssl)} of
        {false,false} ->
            filter(fun({_,{ssl,_,_}}) -> false;
                      ({_,{crypto,_,_}}) -> false;
                      ({_,{ssh,_,_}}) -> false;
                      ({_,{ssh_connection,_,_}}) -> false;
                      ({_,{ssh_sftp,_,_}}) -> false;
                      (_) -> true
                   end, Undef);
        {_,_} -> Undef
    end.

eunit_filter(Undef) ->
    filter(fun({{eunit_test,wrapper_test_exported_,0},
                {eunit_test,nonexisting_function,0}}) -> false;
              (_) -> true
           end, Undef).

dialyzer_filter(Undef) ->
    case app_exists(dialyzer) of
        false ->
            filter(fun({_,{dialyzer_callgraph,_,_}}) -> false;
                      ({_,{dialyzer_codeserver,_,_}}) -> false;
                      ({_,{dialyzer_contracts,_,_}}) -> false;
                      ({_,{dialyzer_cl_parse,_,_}}) -> false;
                      ({_,{dialyzer_timing,_,_}}) -> false;
                      ({_,{dialyzer_plt,_,_}}) -> false;
                      ({_,{dialyzer_succ_typings,_,_}}) -> false;
                      ({_,{dialyzer_utils,_,_}}) -> false;
                      (_) -> true
                   end, Undef);
        _ -> Undef
    end.

wx_filter(Undef) ->
    case app_exists(wx) of
        false ->
            filter(fun({_,{MaybeWxModule,_,_}}) ->
                           case atom_to_list(MaybeWxModule) of
                               "wx"++_ -> false;
                               _ -> true
                           end
                   end, Undef);
        _ -> Undef
    end.

diameter_filter(Undef) ->
    %% Filter away function calls that are catched.
    filter(fun({{diameter_lib,_,_},{erlang,convert_time_unit,3}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,monotonic_time,0}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,unique_integer,0}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,time_offset,0}}) ->
                   false;
              (_) -> true
           end, Undef).

erts_filter(Undef) ->
    filter(fun({_,{prim_socket,_,_}}) -> lists:member(prim_socket, erlang:pre_loaded());
              ({_,{prim_net,_,_}}) -> lists:member(prim_net, erlang:pre_loaded());
              ({_,{socket_registry,_,_}}) -> lists:member(socket_registry, erlang:pre_loaded());
              (_) -> true
           end, Undef).

deprecated_not_in_obsolete(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,DeprecatedFunctions} = xref:q(Server, "DF"),

    L = foldl(fun({M,F,A}=MFA, Acc) ->
                      case otp_internal:obsolete(M, F, A) of
                          no -> [MFA|Acc];
                          _ -> Acc
                      end
              end, [], DeprecatedFunctions),
    case L of
        [] -> ok;
        _ ->
            io:put_chars("The following functions have -deprecated() attributes,\n"
                         "but are not listed in otp_internal:obsolete/3.\n"),
            print_mfas(group_leader(), Server, L),
            Fd = open_log(Config, "deprecated_not_obsolete"),
            print_mfas(Fd, Server, L),
            close_log(Fd),
            ct:fail({length(L),deprecated_but_not_obsolete})
    end.

obsolete_but_not_deprecated(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,NotDeprecated} = xref:q(Server, "X - DF"),

    L = foldl(fun({M,F,A}=MFA, Acc) ->
                      case otp_internal:obsolete(M, F, A) of
                          no -> Acc;
                          _ -> [MFA|Acc]
                      end
              end, [], NotDeprecated),

    case L of
        [] -> ok;
        _ ->
            io:put_chars("The following functions are listed "
                         "in otp_internal:obsolete/3,\n"
                         "but don't have -deprecated() attributes.\n"),
            print_mfas(group_leader(), Server, L),
            Fd = open_log(Config, "obsolete_not_deprecated"),
            print_mfas(Fd, Server, L),
            close_log(Fd),
            ct:fail({length(L),obsolete_but_not_deprecated})
    end.

call_to_deprecated(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,DeprecatedCalls} = xref:q(Server, "strict(E || DF)"),
    foreach(fun ({MFA1,MFA2}) ->
                    io:format("~s calls deprecated ~s",
                              [format_mfa(MFA1),format_mfa(MFA2)])
            end, DeprecatedCalls),
    {comment,integer_to_list(length(DeprecatedCalls))++" calls to deprecated functions"}.

call_to_size_1(Config) when is_list(Config) ->
    %% Applications that do not call erlang:size/1:
    Apps = [asn1,compiler,debugger,kernel,observer,parsetools,
            runtime_tools,stdlib,tools],
    not_recommended_calls(Config, Apps, {erlang,size,1}).

call_to_now_0(Config) when is_list(Config) ->
    %% Applications that do not call erlang:now/1:
    Apps = [asn1,common_test,compiler,debugger,dialyzer,
            kernel,mnesia,observer,parsetools,reltool,
            runtime_tools,sasl,stdlib,syntax_tools,
            tools],
    not_recommended_calls(Config, Apps, {erlang,now,0}).

not_recommended_calls(Config, Apps0, MFA) ->
    Server = proplists:get_value(xref_server, Config),

    Apps = [App || App <- Apps0, is_present_application(App, Server)],

    Fs = [MFA],

    Q1 = io_lib:format("E || ~p : Fun", [Fs]),
    {ok,AllCallsToMFA} = xref:q(Server, lists:flatten(Q1)),

    Q2 = io_lib:format("E | ~p : App || ~p : Fun", [Apps,Fs]),
    {ok,CallsToMFA} = xref:q(Server, lists:flatten(Q2)),

    case CallsToMFA of
        [] ->
            ok;
        _ ->
            io:format("These calls are not allowed:\n"),
            foreach(fun ({MFA1,MFA2}) ->
                            io:format("~s calls non-recommended ~s",
                                      [format_mfa(MFA1),format_mfa(MFA2)])
                    end, CallsToMFA)
    end,

    %% Enumerate calls to MFA from other applications than
    %% the ones known not to call MFA:
    case AllCallsToMFA--CallsToMFA of
        [] ->
            ok;
        Calls ->
            io:format("~n~nThese calls are allowed for now:\n"),
            foreach(fun ({MFA1,MFA2}) ->
                            io:format("~s calls non-recommended ~s",
                                      [format_mfa(MFA1),format_mfa(MFA2)])
                    end, Calls)
    end,
    case CallsToMFA of
        [] ->
            SkippedApps = ordsets:subtract(ordsets:from_list(Apps0),
                                           ordsets:from_list(Apps)),
            case SkippedApps of
                [] ->
                    ok;
                _ ->
                    AppStrings = [atom_to_list(A) || A <- SkippedApps],
                    Mess = io_lib:format("Application(s) not present: ~s\n",
                                         [lists:join(", ", AppStrings)]),
                    {comment, Mess}
            end;
        _ ->
            ct:fail({length(CallsToMFA),calls_to_size_1})
    end.

is_present_application(Name, Server) ->
    Q = io_lib:format("~w : App", [Name]),
    case xref:q(Server, lists:flatten(Q)) of
        {ok,[Name]} -> true;
        {error,_,_} -> false
    end.

strong_components(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,Cs} = xref:q(Server, "components AE"),
    io:format("\n\nStrong components:\n\n~p\n", [Cs]),
    ok.

erl_file_encoding(_Config) ->
    Root = code:root_dir(),
    Wc = filename:join([Root,"**","*.erl"]),
    ErlFiles = ordsets:subtract(ordsets:from_list(filelib:wildcard(Wc)),
                                release_files(Root, "*.erl")),
    {ok, MP} = re:compile(".*lib/(ic)|(orber)|(cos).*", [unicode]),
    Fs = [F || F <- ErlFiles,
               filter_use_latin1_coding(F, MP),
               case epp:read_encoding(F) of
                   none -> false;
                   _ -> true
               end],
    case Fs of
        [] ->
            ok;
        [_|_] ->
            io:put_chars("Files with \"coding:\":\n"),
            [io:put_chars(F) || F <- Fs],
            ct:fail(failed)
    end.

filter_use_latin1_coding(F, MP) ->
    case re:run(F, MP) of
        nomatch ->
            true;
        {match, _} ->
            false
    end.

xml_file_encoding(_Config) ->
    XmlFiles = xml_files(),
    Fs = [F || F <- XmlFiles, is_bad_encoding(F)],
    case Fs of
        [] ->
            ok;
        [_|_] ->
            io:put_chars("Encoding should be \"utf-8\" or \"UTF-8\":\n"),
            [io:put_chars(F) || F <- Fs],
            ct:fail(failed)
    end.

xml_files() ->
    Root = code:root_dir(),
    AllWc = filename:join([Root,"**","*.xml"]),
    AllXmlFiles = ordsets:from_list(filelib:wildcard(AllWc)),
    TestsWc = filename:join([Root,"lib","*","test","**","*.xml"]),
    TestXmlFiles = ordsets:from_list(filelib:wildcard(TestsWc)),
    XmerlWc = filename:join([Root,"lib","xmerl","**","*.xml"]),
    XmerlXmlFiles = ordsets:from_list(filelib:wildcard(XmerlWc)),
    Ignore = ordsets:union([TestXmlFiles,XmerlXmlFiles,
                            release_files(Root, "*.xml")]),
    ordsets:subtract(AllXmlFiles, Ignore).

release_files(Root, Ext) ->
    Wc = filename:join([Root,"release","**",Ext]),
    filelib:wildcard(Wc).

is_bad_encoding(File) ->
    {ok,Bin} = file:read_file(File),
    case Bin of
        <<"<?xml version=\"1.0\" encoding=\"utf-8\"",_/binary>> ->
            false;
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"",_/binary>> ->
            false;
        _ ->
            true
    end.

%% Test runtime dependencies when using an Xref server running in
%% 'functions' mode.

runtime_dependencies_functions(Config) ->
    Server = proplists:get_value(xref_server, Config),
    runtime_dependencies(Server).

%% Test runtime dependencies when using an xref server running in
%% 'modules' mode. Note that more module edges can potentially be
%% found in this mode because the analysis is based on the BEAM
%% code after all optimizations. For example, an apply in the source
%% code could after optimizations be resolved to a specific function.
%%
%% It is important to test 'modules' because reltool runs xref in
%% 'modules' mode (the BEAM files to be released might not contain
%% debug information).

runtime_dependencies_modules(_Config) ->
    Server = start_xref_server(?FUNCTION_NAME, modules),
    try
        runtime_dependencies(Server)
    after
        catch xref:stop(Server)
    end.

runtime_dependencies(Server) ->
    %% Ignore applications intentionally not declaring dependencies
    %% found by xref.
    IgnoreApps = [diameter],

    %% Verify that (at least) OTP application runtime dependencies found
    %% by xref are listed in the runtime_dependencies field of the .app file
    %% of each application.
    {ok, AE} = xref:q(Server, "AE"),
    SAE = lists:keysort(1, AE),
    put(ignored_failures, []),
    {AppDep, AppDeps} = lists:foldl(fun ({App, App}, Acc) ->
                                            Acc;
                                        ({App, Dep}, {undefined, []}) ->
                                            {{App, [Dep]}, []};
                                        ({App, Dep}, {{App, Deps}, AppDeps}) ->
                                            {{App, [Dep|Deps]}, AppDeps};
                                        ({App, Dep}, {AppDep, AppDeps}) ->
                                            {{App, [Dep]}, [AppDep | AppDeps]}
                                    end,
                                    {undefined, []},
                                    SAE),
    [] = check_apps_deps([AppDep|AppDeps], IgnoreApps),
    case IgnoreApps of
        [] ->
            ok;
        _ ->
            Comment = lists:flatten(io_lib:format("Ignored applications: ~p "
                                                  "Ignored failures: ~p",
                                                  [IgnoreApps,
                                                   get(ignored_failures)])),
            {comment, Comment}
    end.

have_rdep(_App, [], _Dep) ->
    false;
have_rdep(App, [RDep | RDeps], Dep) ->		    
    [AppStr, _VsnStr] = string:lexemes(RDep, "-"),
    case Dep == list_to_atom(AppStr) of
        true ->
            %% io:format("~p -> ~s~n", [App, RDep]),
            true;
        false ->
            have_rdep(App, RDeps, Dep)
    end.

check_app_deps(_App, _AppFile, _AFDeps, [], _IgnoreApps) ->
    [];
check_app_deps(App, AppFile, AFDeps, [XRDep | XRDeps], IgnoreApps) ->
    ResOtherDeps = check_app_deps(App, AppFile, AFDeps, XRDeps, IgnoreApps),
    case have_rdep(App, AFDeps, XRDep) of
        true ->
            ResOtherDeps;
        false ->
            Failure = {missing_runtime_dependency, AppFile, XRDep},
            case lists:member(App, IgnoreApps) of
                true ->
                    put(ignored_failures, [Failure | get(ignored_failures)]),
                    ResOtherDeps;
                false ->
                    [Failure | ResOtherDeps]
            end
    end.

check_apps_deps([], _IgnoreApps) ->
    [];
check_apps_deps([{App, Deps}|AppDeps], IgnoreApps) ->
    ResOtherApps = check_apps_deps(AppDeps, IgnoreApps),
    AppFile = code:where_is_file(atom_to_list(App) ++ ".app"),
    {ok,[{application, App, Info}]} = file:consult(AppFile),
    case lists:keyfind(runtime_dependencies, 1, Info) of
        {runtime_dependencies, RDeps} ->
            check_app_deps(App, AppFile, RDeps, Deps, IgnoreApps)
            ++ ResOtherApps;
        false ->
            Failure = {missing_runtime_dependencies_key, AppFile},
            case lists:member(App, IgnoreApps) of
                true ->
                    put(ignored_failures, [Failure | get(ignored_failures)]),
                    ResOtherApps;
                false ->
                    [Failure | ResOtherApps]
            end
    end.

%%%
%%% Common help functions.
%%%

print_mfas(Fd, Server, MFAs) ->
    [io:format(Fd, "~s\n", [format_mfa(Server, MFA)]) || MFA <- MFAs],
    ok.

format_mfa({M,F,A}) ->
    lists:flatten(io_lib:format("~s:~s/~p", [M,F,A])).

format_mfa(Server, MFA) ->
    MFAString = format_mfa(MFA),
    AQ = "(App)" ++ MFAString,
    AppPrefix = case xref:q(Server, AQ) of
                    {ok,[App]} -> "[" ++ atom_to_list(App) ++ "]";
                    _ -> ""
                end,
    AppPrefix ++ MFAString.

open_log(Config, Name) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    RunDir = filename:dirname(filename:dirname(PrivDir)),
    Path = filename:join(RunDir, "system_"++Name++".log"),
    {ok,Fd} = file:open(Path, [write]),
    Fd.

close_log(Fd) ->
    ok = file:close(Fd).

app_exists(AppAtom) ->
    case code:lib_dir(AppAtom) of
        {error,bad_name} ->
            false;
        Path ->
            case file:read_file_info(filename:join(Path,"ebin")) of
                {ok,_} ->
                    true;
                _ ->
                    false
            end
    end.

start_xref_server(Server, Mode) ->
    Root = code:root_dir(),
    xref:start(Server, [{xref_mode,Mode}]),
    xref:set_default(Server, [{verbose,false},
                              {warnings,false},
                              {builtins,true}]),
    {ok,_Relname} = xref:add_release(Server, Root, {name,otp}),

    case code:lib_dir(erts) of
        {error,bad_name} ->
            %% This should not be possible since code_server always adds
            %% an entry for erts.
            ct:fail(no_erts_lib_dir);
        LibDir ->
            case filelib:is_dir(filename:join(LibDir, "ebin")) of
                false ->
                    %% If we are running the tests in the git repository,
                    %% the preloaded BEAM files for Erts are not in the
                    %% code path. We must add them explicitly.
                    Erts = filename:join([LibDir,"preloaded","ebin"]),
                    {ok,_} = xref:add_directory(Server, Erts, []);
                true ->
                    ok
            end
    end,
    Server.
