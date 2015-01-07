<personForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="form-group">
    <dfLabel ref="name" class="col-sm-2 control-label">
      Name:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputText ref="name" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="email" class="col-sm-2 control-label">
      Email:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="email" ref="email" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="department" class="col-sm-2 control-label">
      Department:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputText ref="department" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-1">
      <div class="checkbox">
        <label>
          <dfInputCheckbox ref="receivesHeartbeat"/> Receives Heartbeat
        </label>
      </div>
    </div>
    <div class="col-sm-1">
      <div class="checkbox">
        <label>
          <dfInputCheckbox ref="logsTime"/> Logs Time
        </label>
      </div>
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit value="Submit" />
    </div>
  </div>
</personForm>
