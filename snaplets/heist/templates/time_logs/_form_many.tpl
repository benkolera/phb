<timeLogFormMany class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="row">
    <div class="col-sm-4">
      <div class="form-group">
        <dfLabel ref="username" class="col-sm-6 control-label">
          Username:
        </dfLabel>
        <div class="col-sm-6">
          <dfInputSelect ref="username" class="form-control"/>
        </div>
      </div>
     </div>
    <div class="col-sm-4">
      <div class="form-group">
        <dfLabel ref="day" class="col-sm-2 control-label">
          Day:
        </dfLabel>
        <div class="col-sm-6">
          <dfInput type="date" ref="day" class="form-control"/>
        </div>
      </div>
    </div>
  </div>
  <br/>
  <div class="row"><div class="col-sm-12">
    <div class="form-group">
      <dfLabel ref="rows" class="col-sm-2 control-label">
        Logs
      </dfLabel>
      <dfInputList ref="rows">
        <div class="col-sm-10">
          <div class="row">
            <div class="col-sm-2 subform-heading">
              Time Worked (HH:MM)
            </div>
            <div class="col-sm-3 subform-heading">
              Against
            </div>
            <div class="col-sm-6 subform-heading">
              Notes
            </div>
            <div class="col-sm-1 subform-heading">
            </div>
          </div>
          <dfListItem>
            <div itemAttrs>
              <div class="row">
                <div class="col-sm-2">
                  <div class="row">
                    <div class="col-sm-6">
                      <dfInput type="number" ref="hours" class="form-control log-hour" />
                    </div>
                    <div class="col-sm-6">
                      <dfInput type="number" ref="minutes" class="form-control log-minute" />
                    </div>
                  </div>
                </div>
                <div class="col-sm-2">
                  <dfInputSelectGroup ref="link" class="form-control" />
                </div>
                <div class="col-sm-6">
                  <dfInputText ref="desc" class="form-control" />
                </div>
                <div class="col-sm-1">
                  <button class="btn btn-xs btn-danger" removeControl value="Remove">
                    <i class="fa fa-trash" />
                  </button>
                </div>
              </div>
            </div>
          </dfListItem>
          <button class="btn btn-xs btn-default" addControl>
            <i class="fa fa-plus" /> Add Another
          </button>
          <hr/>
          <div class="row">
            <div class="col-sm-2">
              <b>Total Time</b>:
              <span id="log-total-hours">00</span> :
              <span id="log-total-minutes">00</span>
            </div>
          </div>
        </div>
      </dfInputList>
    </div>
  </div></div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-2">
      <dfInputSubmit name="action" value="Create" class="btn btn-primary" />
    </div>
  </div>
</timeLogFormMany>

<script>
  $(function () { PS.Phb.initTimeLogManyForm() });
</script>
