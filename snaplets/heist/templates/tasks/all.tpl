<apply template="base">
  <h1>Tasks</h1>
  <div class="pull-right">
    <a href="/tasks/create" class="btn btn-success">New Task</a>
  </div>
  <allTasks>
    <h2>Active</h2>
    <activeTasks>
      <apply template="_all" />
    </activeTasks>
    <h2>Last 25 Completed</h2>
    <completedTasks>
      <apply template="_all" />
    </completedTasks>
  </allTasks>
</apply>
