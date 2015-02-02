<apply template="base">
  <h1>Tasks</h1>
  <div class="pull-right">
    <a href="/tasks/create" class="btn btn-success">New Task</a>
  </div>
  <allProjects>
    <h2>Active</h2>
    <activeProjects>
      <apply template="_all" />
    </activeProjects>
    <h2>Last 20 Completed</h2>
    <completedProjects>
      <apply template="_all" />
    </completedProjects>
  </allProjects>
</apply>
