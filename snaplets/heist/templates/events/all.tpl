<apply template="base">
  <h1>All Events</h1>
  <div class="pull-right">
    <a href="/events/create" class="btn btn-success">New Event</a>
  </div>
  <allEvents>
    <h2>Active</h2>
    <activeEvents>
      <apply template="_all" />
    </activeEvents>
    <h2>Completed</h2>
    <completedEvents>
      <apply template="_all" />
    </completedEvents>
  </allEvents>
</apply>
