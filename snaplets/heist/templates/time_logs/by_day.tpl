<apply template="base">
  <byDayTimeLogs>
    <div class="row">
      <div class="col-sm-9">
        <h1>Time Logs - <timeLogTitle /></h1>
      </div>
      <div class="col-sm-3">
        <div class="pull-right" style="padding-top: 20px">
          <a class="btn btn-xs btn-default ${prevClass}" href="${prevHref}">
            <i class="fa fa-backward hidden-sm hidden-xs" />
            Prev Day
          </a>
          <a class="btn btn-xs btn-default ${nextClass}" href="${nextHref}">
            Next Day
            <i class="fa fa-forward hidden-sm hidden-xs" />
          </a>
        </div>
      </div>
    </div>
    <apply template="_all" />
  </byDayTimeLogs>
</apply>
