<apply template="base">
  <listTimeLogs>
    <h1>Time Logs</h1>
    <div class="row">
      <div class="col-sm-12 col-md-6">
        <ifQueries>
          <h2>Current Filters</h2>
          <timeLogQueryLinks>
            <a class="btn btn-default" href="/time_logs?${href}">
              <title /> <i class="fa fa-trash"></i>
            </a>
          </timeLogQueryLinks>
        </ifQueries>
        <h2>Add Filter</h2>
        <a href="${currentUrl}&user=me" class="btn btn-default">Owner: Me</a>
        <div class="btn-group">
          <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-expanded="false">
            Owner <span class="caret"></span>
          </button>
          <ul class="dropdown-menu" role="menu">
            <possibleOwners>
              <li><a href="${currentUrl}&user=${userId}"><userName /></a></li>
            </possibleOwners>
          </ul>
        </div>
        <a href="${currentUrl}&period=today" class="btn btn-default">Period: Today</a>
        <a href="${currentUrl}&period=this_week" class="btn btn-default">Period: This Week</a>
        <a href="${currentUrl}&period=this_month" class="btn btn-default">Period: This Month</a>
        <hr/>
        <a href="/time_logs/create" class="btn btn-success">New Time Log</a>
      </div>
      <div class="col-sm-12 col-md-6">
        <summary>
          <apply template="_time_graph" />
        </summary>
      </div>
    </div>
    <apply template="_all" />
    <a href="/time_logs/create" class="btn btn-success">New Time Log</a>
  </listTimeLogs>
</apply>
