<table class="table table-centred">
  <thead>
    <tr>
      <th width="15%">Username</th>
      <th width="10%">Date</th>
      <th width="5%">Minutes</th>
      <th width="25%">Task</th>
      <th>Notes</th>
      <th width="5%"></th>
  </thead>
  <tbody>
    <timeLogRow>
      <tr>
        <td><a href="${currentUrl}&user=${personId}"><username /></a></td>
        <td><a href="${currentUrl}&period=${day}"><day /></a></td>
        <td><minutes /></td>
        <td><a href="/tasks/${taskId}/edit"><taskName /></a></td>
        <td><notes /></td>
        <td><a class="btn btn-default btn-sm" href="/time_logs/${id}/edit">Edit</a></td>
      </tr>
    </timeLogRow>
  </tbody>
</table>
