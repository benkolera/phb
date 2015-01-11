  <heartbeat>
    <div class="row">
      <div class="col-sm-9">
        <h1>Development Heartbeat - <start /> to <finish /></h1>
      </div>
      <div class="col-sm-3">
        <div class="pull-right" style="padding-top: 20px">
          <a class="btn btn-sm btn-default ${prevClass}" href="${prevHref}">Prev Heartbeat</a>
          <a class="btn btn-sm btn-default ${nextClass}" href="${nextHref}">Next Heartbeat</a>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col col-sm-7">
        <div><highlights /></div>
        <h2>Upcoming Events</h2>
        <div><upcomingEvents /></div>
        <h2>Successes</h2>
        <table class="table">
          <thead>
            <tr>
              <th>Who</th>
              <th>What</th>
              <th>Achievement</th>
            </tr>
          </thead>
          <tbody>
            <successRow>
              <tr>
                <td><who /></td>
                <td><what /></td>
                <td><achievement /></td>
              </tr>
            </successRow>
          </tbody>
        </table>
      </div>
      <div class="col-sm-5">
        <div class="row">
          <div class="col-sm-6">
            <canvas id="timeBreakdown" width="300" height="300"></canvas>
          </div>
          <div class="col-sm-6">
            <div id="timeBreakdownLegend" class="pull-right"></div>
          </div>
        </div>
      </div>
    </div>
    <h2>Projects</h2>
    <table class="table table-centered">
      <thead>
        <tr>
          <th width="15%">Project</th>
          <th width="10%">Customer</th>
          <th width="10%">Status</th>
          <th width="12%">Stakeholders</th>
          <th width="15%">Target Completion</th>
          <th width="8%">Effort / Duration</th>
          <th width="30%">Notes / Next Steps</th>
        </tr>
      </thead>
      <tbody>
        <projectRow>
          <tr>
            <td><name /></td>
            <td><customers/></td>
            <td class="${statusClass}"><status /></td>
            <td><stakeholders /></td>
            <td>
              <targets>
                <target>
                  <div class="row">
                    <div class="col-sm-12">
                      <desc /> - <dayOrHandwavy />
                    </div>
                  </div>
                </target>
              </targets>
            </td>
            <td><effort>
                <effortDays /> Person Days<br />
                Started: <effortStart />
            </effort></td>
            <td><notes /></td>
          </tr>
        </projectRow>
      </tbody>
    </table>
    <h2>Backlog</h2>
    <table class="table table-centered">
      <thead>
        <tr>
          <th width="15%">Project</th>
          <th width="10%">Customer</th>
          <th width="10%">Status</th>
          <th width="12%">Stakeholders</th>
          <th width="53%">Notes / Next Steps</th>
        </tr>
      </thead>
      <tbody>
        <backlogRow>
          <tr>
            <td><name /></td>
            <td><customers /></td>
            <td class="${statusClass}"><status /></td>
            <td><stakeholders /></td>
            <td><notes /></td>
          </tr>
        </backlogRow>
      </tbody>
    </table>
    <h2>Operational Events</h2>
    <table class="table table-centered">
      <thead>
        <tr>
          <th width="15%">Escalation or Event</th>
          <th width="10%">Customer</th>
          <th width="30%">What Happened</th>
          <th width="10%">Impact to Client</th>
          <th width="10%">Duration</th>
          <th width="5%">SLA Met</th>
          <th width="20%">Notes / Next Steps</th>
        </tr>
      </thead>
      <tbody>
        <eventRow>
          <tr>
            <td><name /></td>
            <td><customers /></td>
            <td><desc /></td>
            <td><impact /></td>
            <td><duration /></td>
            <td class="${statusClass}"><status /></td>
            <td><notes /></td>
          </tr>
        </eventRow>
      </tbody>
    </table>
    <h2>Action Items</h2>
    <table class="table table-centered">
      <thead>
        <tr>
          <th width="15%">Who</th>
          <th width="10%">Customer</th>
          <th width="10%">Status</th>
          <th width="10">Due</th>
          <th width="55%">Notes / Next Steps</th>
        </tr>
      </thead>
      <tbody>
        <actionRow>
          <tr>
            <td><who /></td>
            <td><customer /></td>
            <td><status /></td>
            <td><due /></td>
            <td><notes /></td>
          </tr>
        </actionRow>
      </tbody>
    </table>
    <timeLogData />
<script>
$( function () {
var config = {

};
var timeBreakdownChart = PS.Phb.heartbeatTimebreakdown("timeBreakdown")(timeLogData)(config)();
$("#timeBreakdownLegend").append(timeBreakdownChart.generateLegend());
});
</script>
  </heartbeat>
