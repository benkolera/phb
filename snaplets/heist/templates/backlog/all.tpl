<apply template="base">
  <h1>Backlog</h1>
  <div class="pull-right">
    <a href="/backlog/create" class="btn btn-success">New Backlog</a>
  </div>
  <allBacklog>
    <h2>Active</h2>
    <activeBacklog>
      <apply template="_all" />
    </activeBacklog>
    <h2>Completed</h2>
    <completedBacklog>
      <apply template="_all" />
    </completedBacklog>
  </allBacklog>
</apply>
