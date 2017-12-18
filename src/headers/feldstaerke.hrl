-define(MFN, lists:flatten(io_lib:format("~s:~s", [?MODULE, ?FUNCTION_NAME]))).
-define(APPLICATION, feldstaerke).

-define(UNAUTHORIZED,   "unauthorized").
-define(AUTHORIZED,     "authorized").
-define(CHALLENGE_SENT, "challenge_sent").