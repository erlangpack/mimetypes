mimetypes
=========

`mimetypes` is an Erlang library to fetch MIME extension/name mappings.

Usage
-----

``` erlang
$ erl -pa ebin                  
Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.4  (abort with ^G)
1> application:start(mimetypes).
ok
2> mimetypes:extension(<<"js">>).
<<"application/javascript">>
3> mimetypes:extension(<<"mb">>).
<<"application/mathematica">>
4> mimetypes:extension(<<"html">>).
<<"text/html">>
5> mimetypes:extension(<<"m3u8">>).    
[<<"application/vnd.apple.mpegurl">>,
 <<"application/x-mpegurl">>]
6> mimetypes:filename("/a/b.js").
<<"application/javascript">>
7> mimetypes:extensions("application/mathematica").
[<<"ma">>,<<"nb">>,<<"mb">>]
```

Additional types
----------------

Additional mappings can be loaded at runtime using the ```mimetypes:load/2```
function. If this function is timing out it is possible to increase the
timeout using ```mimetypes:load/3```.

``` erlang
1> application:start(mimetypes).
ok
2> mimetypes:load(default, [{<<"ext">>, <<"new/type">>}]).
ok
3> mimetypes:extension(<<"ext">>).
[<<"application/vnd.novadigm.ext">>,<<"new/type">>]
2> mimetypes:load(default, [{<<"ext">>, <<"new/type">>}], 10000).
ok
```

Additional mappings can also be specified using the application environment
variables for the ```mimetypes``` application. These can be added to the
config file specified using the ```erl -config``` flag.

``` erlang
[{mimetypes, [
    %% Wait for all additional types to be loaded before returning from
    %% application:start(mimetypes). Set to false to return immidiately
    %% and load them in the background.
    {load_async, true},
    %% Timeout to use for calls to mimetypes:load/3 while loading additional
    %% mimetypes. The default is the same as for mimetypes:load/2. You may
    %% need to increase this value.
    {load_timeout, 5000},
    {load, [
        {default, [
            {<<"ext">>, <<"new/type">>}
        ]}
    ]}
]}].
```
