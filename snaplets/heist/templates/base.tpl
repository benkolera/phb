<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="PHB - A expectation management & communication tool">
    <meta name="author" content="">
    <link rel="icon" href="/images/favicon.ico">

    <title>PHB - <pageTitle /></title>

    <link href="/components/bootstrap/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="/components/bootstrap/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="/components/font-awesome/css/font-awesome.min.css" rel="stylesheet">
    <link href="/css/phb.css" rel="stylesheet">

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
  </head>

  <body>

    <nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
      <div class="container-fluid">
        <div class="navbar-header">
          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
          <a class="navbar-brand" href="/">PHB</a>
        </div>
        <div id="navbar" class="collapse navbar-collapse">
          <ifLoggedIn>
            <ul class="nav navbar-nav">
              <li><a href="/heartbeats">Heartbeats</a></li>
              <li><a href="/projects">Projects</a></li>
              <li><a href="/backlog">Backlog</a></li>
              <li><a href="/events">Events</a></li>
              <li class="dropdown">
                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Time Logs <span class="caret"></span></a>
                <ul class="dropdown-menu" role="menu">
                  <li><a href="/time_logs/create">Log Time</a></li>
                  <li><a href="/time_logs/mine">My Time Logs</a></li>
                  <li><a href="/time_logs">All Time Logs</a></li>
                </ul>
              </li>
              <li><a href="/customers">Customers</a></li>
              <li><a href="/work_categories">Work Categories</a></li>
              <li><a href="/people">People</a></li>
            </ul>
          </ifLoggedIn>
          <ul class="nav navbar-nav navbar-right">
            <ifLoggedIn>
              <li><a href="/logout">Logout</a></li>
            </ifLoggedIn>
            <ifLoggedOut>
              <li><a href="/login">Login</a></li>
            </ifLoggedOut>
          </ul>
        </div>
      </div>
    </nav>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
    <div class="container-fluid">
      <div id="message-container">
        <div id="messages">
          <flash type="success" />
          <flash type="error" />
          <flash type="info" />
        </div>
      </div>
      <apply-content/>
    </div>
    <script src="/components/bootstrap/dist/js/bootstrap.min.js"></script>
    <script src="/components/chartjs/Chart.js"></script>
    <script src="/components/pleasejs/dist/Please.js"></script>
    <script src="/js/digestive-functors-heist.js"></script>
    <script src="/js/phb.js"></script>
  </body>
</html>
