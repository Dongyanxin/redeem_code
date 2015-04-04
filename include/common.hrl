%%%-------------------------------------------------------------------
%%% @author xin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 三月 2015 上午9:59
%%%-------------------------------------------------------------------
-author("xin").


-define(DEBUG(Message), common:debug(Message)).

-define(IsDEBUG, true).


-record(redeem_code, {r_code, create_date, use_date, uid, is_use=false, code_type=0}).