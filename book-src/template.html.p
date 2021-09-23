<!doctype html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
  <title>◊(select 'chapter* doc)</title>
  <style>
    body {
        max-width: 40em;
        margin-left: auto;
        margin-right: auto;
    }

    span.sidenote {
        float: left;
        clear: left;
        text-align: right;
        font-size: 0.9em;
        width: 50%;
        margin-left: -60%;
    }

    span.box {
        border:  1px solid;
        padding: 0.5pt 2.5pt;
        margin:  0 1.5pt 0 0;
    }
  </style>
</head>
<body>
◊(->html doc #:splice? #t)
</body>
</html>
