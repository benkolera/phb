<timeLogForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="form-group">
    <dfLabel ref="username" class="col-sm-2 control-label">
      Person:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelect ref="person" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="link" class="col-sm-2 control-label">
      Task:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelect ref="task" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="day" class="col-sm-2 control-label">
      Date:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="date" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="day" class="col-sm-2 control-label">
      Notes:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputTextArea ref="desc" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit name="action" value="Update" class="btn btn-primary" />
    </div>
  </div>
</timeLogForm>
