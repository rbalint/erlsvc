%%-
%% Copyright 2011 Yakaz. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%%    1. Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%% 
%%    2. Redistributions in binary form must reproduce the above
%%       copyright notice, this list of conditions and the following
%%       disclaimer in the documentation and/or other materials provided
%%       with the distribution.
%% 
%% THIS SOFTWARE IS PROVIDED BY YAKAZ ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL YAKAZ OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
%% OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
%% OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(erlsvc).

-include("erlsvc.hrl").

-export([
    run/0,
    run/1
  ]).

run() ->
    run(self).

run(Node_Name) ->
    %% We want to catch controller's exit reason.
    process_flag(trap_exit, true),
    %% Start the controller process. He's responsible for starting the
    %% worker and doing all the communication with the Perl side and the
    %% worker.
    Exit_Code = case erlsvc_controller:start_link(Node_Name) of
        {ok, Controller} ->
            receive
                {controller, done, Code} when is_integer(Code) ->
                    %% The controller finished is job and exited
                    %% normally, no matter the job's result.
                    Code;
                {'EXIT', Controller, Reason} ->
                    %% The controller creashed.
                    erlsvc_lib:report_error("Controller crashed:~n~p~n",
                      [Reason]),
                    ?EX_SOFTWARE
            end;
        {error, Reason} ->
            %% The controller couldn't be started.
            erlsvc_lib:report_error("Failed to start controller:~n~p~n",
              [Reason]),
            ?EX_CANTCREAT
    end,
    halt(Exit_Code).
