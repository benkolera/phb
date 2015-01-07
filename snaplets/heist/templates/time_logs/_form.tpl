<timeLogForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="form-group">
    <dfLabel ref="username" class="col-sm-2 control-label">
      Username:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelect ref="username" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="day" class="col-sm-2 control-label">
      Day:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="day" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="minutes" class="col-sm-2 control-label">
      Time Worked (HH:MM):
    </dfLabel>
    <div class="col-sm-1">
      <dfInput type="number" ref="hours" class="form-control" />
    </div>
    <div class="col-sm-1">
      <dfInput type="number" ref="minutes" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="link" class="col-sm-2 control-label">
      Time Against:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelectGroup ref="link" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="desc" class="col-sm-2 control-label">
      Notes:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputTextArea ref="desc" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit name="action" value="${action}" class="btn btn-primary" />
      <ifCreate>
        <dfInputSubmit name="action" value="Create & Continue" class="btn btn-default" />
      </ifCreate>
    </div>
  </div>
</timeLogForm>
