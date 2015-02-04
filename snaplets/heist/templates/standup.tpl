<apply template="base">
  <standup>
    <standupPerson>
      <h1><name /></h1>
      <div class="row">
        <div class="col-sm-9">
          <div class="row">
            <div class="col-sm-1">
              <h2>Yesterday</h2>
            </div>
          </div>
          <div class="row">
            <div class="col-sm-6">
              <ul>
                <yesterdayRow>
                  <li class="${taskClass}"><taskName /> - <taskHours />hrs</li>
                </yesterdayRow>
              </ul>
            </div>
            <div class="col-sm-6">
              <yesterdayTimeBreakdown>
                <apply template="_time_graph" />
              </yesterdayTimeBreakdown>
            </div>
          </div>
        </div>
        <div class="col-sm-3">
          <h2>Today</h2>
          <ul>
            <todayRow>
              <li><taskName /></li>
            </todayRow>
          </ul>
        </div>
      </div>
    </standupPerson>
  </standup>
</apply>
