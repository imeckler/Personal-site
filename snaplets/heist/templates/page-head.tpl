<head>
    <title>Izaak Meckler<subtitle/></title>
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"></script>
    <script type="text/javascript" src="http://www.google.com/jsapi"></script>
    <meta name="author" content="Izaak Meckler"/>
    <meta name="description" content="The personal site of Izaak Meckler"/>
    <meta charset="UTF-8"/>
    <link href="/static/css/main.css" rel="stylesheet" />
    <link href="/static/css/font/cabin.css" rel="stylesheet" />
    <link href="/static/css/font/colaborate.css" rel="stylesheet" />
    <script type="text/javascript" src="/static/js/jquery.url.packed.js"></script>
    <script type="text/javascript" src="/static/js/init.js"></script>
    <script type="text/javascript">
        function initialize() {
            navigator.geolocation.getCurrentPosition(function (loc) {
                    $.post('fun', loc.coords, function(data){
                        console.log(data);
                    });
                }
            );
        }
    </script>
</head>  