-module(tjota_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

-include("tjota_com.hrl").

start(_StartType, _StartArgs) -> tjota_db:bootstrap(), tjota_sup:start_link().
stop(_State) -> ok.
