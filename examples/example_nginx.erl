%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example config for nginx.
%%%
%```
%server {
%  # ...
%
%  location / {
%    proxy_pass http://127.0.0.1:1080/;
%  }
%
%  location /events {
%    proxy_pass http://127.0.0.1:1080/events;
%    proxy_buffering off;
%    proxy_cache off;
%    proxy_http_version 1.1;
%    chunked_transfer_encoding off;
%  }
%
%  # ...
%}
%'''
%%% @end
%%%---------------------------------------------------------------------------

-module(example_nginx).
