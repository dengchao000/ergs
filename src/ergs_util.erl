-module(ergs_util).

-export([get_app_env/2]).

get_app_env(Option, Default) ->
    case application:get_env(ergs, Option) of
	{ok, Value} ->
	    Value;
	_ ->
	    case init:get_argument(Option) of
		[[Value|_]] ->
		    Value;
		error ->
		    Default
	    end
    end.
