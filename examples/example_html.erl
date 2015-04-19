%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example HTML to check Server-Sent Events in the browser.
%%%
%```
%<html>
%<head>
%<title>server-sent events test</title>
%<meta http-equiv="content-type" content="text/html; charset=utf-8" />
%<style>
%
%pre {
%  border: solid 1px black;
%}
%
%</style>
%<script>
%
%  var es = new EventSource("/events");
%
%  es.onmessage = function(e) {
%    var logs = document.getElementById("logs");
%    logs.textContent += e.data + "\n";
%  };
%
%</script>
%</head>
%<body>
%
%<h1>Test of server-sent events</h1>
%
%<pre id="logs">
%</pre>
%
%</body>
%</html>
%'''
%%% @end
%%%---------------------------------------------------------------------------

-module(example_html).
