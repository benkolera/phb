<apply template="base">
  <h1>All Projects</h1>
  <div class="pull-right">
    <a href="/projects/create" class="btn btn-success">New Project</a>
  </div>
  <allProjects>
    <h2>Active</h2>
    <activeProjects>
      <apply template="_all" />
    </activeProjects>
    <h2>Completed</h2>
    <completedProjects>
      <apply template="_all" />
    </completedProjects>
  </allProjects>
</apply>
