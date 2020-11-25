-module(aqlc_tcp).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, stop/1, send/2]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("aqlc.hrl").

-record(state, {socket}).

-spec start_link(address(), port_number()) -> {ok, pid()} | {error, term()}.
start_link(Address, Port) ->
    start_link(Address, Port, []).

-spec start_link(address(), port_number(), [connect_option()]) -> {ok, pid()} | {error, term()}.
start_link(Address, Port, Options) ->
    gen_server:start_link(?MODULE, {Address, Port, Options}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec send(pid(), iodata()) -> {ok, binary()} | {error, term()}.
send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

init({Address, Port, Options}) ->
    Timeout = proplists:get_value(timeout, Options, ?DEFAULT_CONNECT_TIMEOUT),

    case gen_tcp:connect(Address, Port, [binary, {active, false}], Timeout) of
        {ok, Socket} ->
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({send, Data}, _From, State = #state{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            Response = gen_tcp:recv(Socket, 0, ?DEFAULT_RECEIVE_TIMEOUT),
            {reply, Response, State};
        Error = {error, _Reason} ->
            {reply, Error, State}
    end;
handle_call(stop, _From, State = #state{socket = Socket}) ->
    ok = gen_tcp:close(Socket),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
