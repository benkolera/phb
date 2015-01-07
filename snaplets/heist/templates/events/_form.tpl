<eventForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="form-group">
    <dfLabel ref="username" class="col-sm-2 control-label">
      Name:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputText ref="name" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="customers" class="col-sm-2 control-label">
      Customers Affected:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputList ref="customers">
        <ul class="list-unstyled">
          <dfListItem>
            <li itemAttrs>
              <div class="input-group">
                <dfInputSelect ref="" class="form-control" />
                <span class="input-group-btn">
                  <button class="btn btn-xs btn-danger" removeControl value="Remove">
                    <i class="fa fa-trash" />
                  </button>
                </span>
              </div>
            </li>
          </dfListItem>
        </ul>
        <button class="btn btn-xs btn-default" addControl>
          <i class="fa fa-plus" /> Add Another
        </button>
      </dfInputList>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="desc" class="col-sm-2 control-label">
      What Happened:
    </dfLabel>
    <div class="col-sm-4">
      <dfInputTextArea ref="desc" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="impact" class="col-sm-2 control-label">
      Impact:
    </dfLabel>
    <div class="col-sm-4">
      <dfInputTextArea ref="impact" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="start" class="col-sm-2 control-label">
      Start:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="start.date" class="form-control" />
    </div>
    <div class="col-sm-2">
      <dfInput type="time" ref="start.time" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="finish" class="col-sm-2 control-label">
      Finish:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="finish.date" class="form-control" />
    </div>
    <div class="col-sm-2">
      <dfInput type="time" ref="finish.time" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="status" class="col-sm-2 control-label">
      SLA Met:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputSelect ref="status" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="notes" class="col-sm-2 control-label">
      Notes:
    </dfLabel>
    <div class="col-sm-4">
      <dfInputTextArea ref="notes" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit value="Submit" />
    </div>
  </div>
</eventForm>
