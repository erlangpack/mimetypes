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