[![Build Status][gh badge]][gh]
[![Hex.pm version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Erlang Versions][erlang version badge]][gh]

mimetypes
=========

`mimetypes` is an Erlang library to fetch MIME extension/name mappings.

Usage
-----

```erlang
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
7> mimetypes:extensions(<<"application/mathematica">>).
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

<!-- Badges -->
[hexpm]: https://hex.pm/packages/mimetypes
[hexpm version]: https://img.shields.io/hexpm/v/mimetypes.svg?style=flat-curcle "Hex version"
[hexpm downloads]: https://img.shields.io/hexpm/dt/mimetypes.svg?style=flat-curcle
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-curcle
[hexdocs]: https://hexdocs.pm/mimetypes
[gh]: https://github.com/erlangpack/mimetypes/actions/workflows/test.yaml
[gh badge]: https://github.com/erlangpack/mimetypes/workflows/Test/badge.svg
[erlang version badge]: https://img.shields.io/badge/Supported%20Erlang%2FOTP-22%20to%2024-blue.svg?style=flat-curcle
