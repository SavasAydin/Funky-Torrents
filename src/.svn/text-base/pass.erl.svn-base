-module(pass).
-import(lists, [foreach/2]).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(users,{username,password}).


-export([hash/1]).


%%------------------------------------------------------------------------------
%% Authors: Ali Issa & Magnus Bergqvist
%% Hash is our own hash function for password protection. 
%% We wrote it just for fun.
%% hash/1 is the exported function which takes a string (List of ASCII numbers)
%% and passes it on to hash/5 together with four empty lists.
%%------------------------------------------------------------------------------


hash(Password)->
    hash(Password,[],[],[],[]).

hash([Sign|Password],[Number1|List1],[Number2|List2],[Number3|List3],Result)->
    hash(Password,List1,List2,List3,[Sign*Number1*Number2*Number3|Result]);
hash(Password,[],_List2,_List3,Result)->
    hash(Password,[802950047,928800781,1139351149,427169,737351,937613,343381],_List2,_List3,Result);
hash(Password,_List1,[],_List3,Result) ->
    hash(Password,_List1,[547,89,191,463],_List3,Result);
hash(Password,_List1,_List2,[],Result) ->
    hash(Password,_List1,_List2,[83,61,3499],Result);
 hash([],_,_,_,[H|T])-> mix(H,T).
mix(H,[Head|Tail])->
    mix(H*Head,Tail);
mix(H,[]) ->H.
%%------------------------------------------------------------------------------
%%Author:Ali Issa
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%%Creates a schema at a specified directory and starts the mnesia db
%%------------------------------------------------------------------------------

init()->
    application:set_env(mnesia, dir, "C:\\mnesia"),
    mnesia:create_schema([node()]),
   mnesia:start(),
    mnesia:create_table(users, [{attributes, record_info(fields ,users)},{disc_copies,[node()]}]).

%%------------------------------------------------------------------------------
%% test() initiates and starts the database, then it tries to register test,test
%% in the users table, then shows what have been added in the database,used for
%% testing the module.
%%------------------------------------------------------------------------------

test()->
init(),
start(),
mnesia:wait_for_tables([users],20000),
regist(test,test),
showUsers().
   
start()->
 application:set_env(mnesia, dir, "C:\\mnesia"),
 mnesia:start().

logout()->
    mnesia:stop(),loged_out.
%%------------------------------------------------------------------------------
%% login function checks that the username is not already registered and also
%% that the user inputs correct user combination.
%%------------------------------------------------------------------------------		
login(UserName,PassWord)->
    StringPass=hash(atom_to_list(PassWord)),
    Rows= do(qlc:q([X||X<-mnesia:table(users),
		       X#users.username==UserName,
		       X#users.password==StringPass])),
    if Rows/=[]->
	    logged_in;
       true -> wrong_user_combination end.
%%------------------------------------------------------------------------------
%% showUsers displays the registered users, used to test the module.
%%------------------------------------------------------------------------------
showUsers() ->
    do(qlc:q([X || X <- mnesia:table(users)])).

%%------------------------------------------------------------------------------
%%checks the format of the input and then depending on the format changes type
%% to lıst.
%%------------------------------------------------------------------------------
check_format(Char)->
    if  
	true == is_atom(Char)->
	    Is_atom= atom_to_list(Char),Is_atom;
        true == is_integer(Char) ->
	    Is_integer = integer_to_list(Char),Is_integer	     	    
    end.
    
%%------------------------------------------------------------------------------
%% To register first thing to type must be a letter, then u can mix letters and 
%%integers. regist/2 checks that the input password is not shorter than 3 char.
%%------------------------------------------------------------------------------
regist(User,Pass)->
    Check_format = check_format(Pass),
    LengthPass = length(Check_format),
    Ron= do(qlc:q([{X#users.username} || X <- mnesia:table(users),X#users.username==User])),
    if
	LengthPass<4->
	    your_pass_short;
	Ron /=[] ->
	    please_change_username;
	true->
	    Userinfo = #users{username=User,password=hash(Check_format)},
	    F=fun()->
		      mnesia:write(Userinfo)
	      end,
	    mnesia:transaction(F)
 
    end.
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
%%------------------------------------------------------------------------------
%If the method returns true, then the test and the password is saved in the 
%%table users. When this test is executed the second time, the method will give
%% please_change_username in the case were the test exist in the users table.
%%------------------------------------------------------------------------------
register_test1()->
    Test1 = [{users,test,
	      359108155753809325019470807488856167700318395733943595527120}] ==  pass:all(),Test1.

%%------------------------------------------------------------------------------
%% Testing that the register can handle passwords wich contains combination of
%% letters and integers,if the registration is completed, method should return 
%% true.
%% make sure that the user name used in this function is not already registered, 
%%will otherwise return false.
%%------------------------------------------------------------------------------

register_test2()->
    {atomic,ok} == regist(usera,a2b3).

%%------------------------------------------------------------------------------
%% Register_test() shall be executed before loginCorrect_test(). 
%%Returnes logged_in when the user is registered in the table, else 
%%wrong_user_combination.
%%------------------------------------------------------------------------------
loginCorrect_test()->
    start(),
    logged_in = pass:login(tesat,tesat).
%%------------------------------------------------------------------------------
%method tries to log-in with error username and password, shall return true if
%% module working properly.
%%------------------------------------------------------------------------------
loginWrong_test()->
    start(),
    wrong_user_combination == pass:login(wrong,wrong).
%%------------------------------------------------------------------------------
%% Shall return true if the user is loged out. we should see an info report 
%%telling that mnesia is stopped.
%%-----------------------------------------------------------------------------
logout_test()->
    start(),
    loged_out == pass:logout().
