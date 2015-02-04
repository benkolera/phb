<div class="col-sm-offset-1 col-sm-5 col-md-offset-0 col-md-6">
  <canvas id="${uuid}" width="300" height="300"></canvas>
</div>
<div class="col-sm-6">
  <div id="timeBreakdownLegend" class="pull-right"></div>
</div>
<timeLogData />
<script>
$( function () {
var timeBreakdownChart = PS.Phb.heartbeatTimebreakdown("<uuid />")(timeLogData)();
console.log(timeBreakdownChart);
$("#timeBreakdownLegend").append(timeBreakdownChart.generateLegend());
});
</script>
