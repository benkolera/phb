<taskForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="form-group">
    <dfLabel ref="username" class="col-sm-2 control-label">
      Person *:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelect ref="person" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="desc" class="col-sm-2 control-label">
      Name *:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputText ref="name" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="link" class="col-sm-2 control-label">
      Against *:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelectGroup ref="link" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="day" class="col-sm-2 control-label">
      Start *:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="start" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="day" class="col-sm-2 control-label">
      Finish:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="finish" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit name="action" value="${action}" class="btn btn-primary" />
    </div>
  </div>
</taskForm>
