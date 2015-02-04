<timeLogUserDate class="form-horizontal">
  <div class="row">
    <div class="col-sm-4">
      <div class="form-group">
        <label for="person" class="col-sm-6 control-label">
          Person:
        </label>
        <div class="col-sm-6">
          <dfInputSelect ref="person" class="form-control"/>
        </div>
      </div>
     </div>
    <div class="col-sm-4">
      <div class="form-group">
        <dfLabel ref="date" class="col-sm-2 control-label">
          Day:
        </dfLabel>
        <div class="col-sm-6">
          <dfInput type="date" ref="date" class="form-control"/>
        </div>
      </div>
    </div>
  </div>
  <div class="form-group">
    <div class="col-sm-offset-2 col-sm-10">
      <dfInputSubmit name="action" value="Change Person/Date" class="btn btn-default btn-sm" />
      <hr/>
    </div>
  </div>
</timeLogUserDate>
<timeLogFormRows class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="row"><div class="col-sm-12">
    <dfInputHidden ref="person" />
    <dfInputHidden ref="date" />
    <div class="form-group">
      <dfLabel ref="rows" class="col-sm-2 control-label">
        Logs:
      </dfLabel>
      <dfInputList ref="rows">
        <div class="col-sm-10">
          <div class="row">
            <div class="col-sm-2 subform-heading">
              Time Worked (HH:MM)
            </div>
            <div class="col-sm-2 subform-heading">
              Task
            </div>
            <div class="col-sm-5 subform-heading">
              Notes
            </div>
            <div class="col-sm-1 subform-heading">
              Completed
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
                  <dfInputSelectGroup ref="task" class="form-control" />
                </div>
                <div class="col-sm-5">
                  <dfInputText ref="desc" class="form-control" />
                </div>
                <div class="col-sm-1">
                  <div style="margin: 0 auto; width: 10px;" class="checkbox">
                    <label>
                      <dfInputCheckbox ref="completed"/>
                    </label>
                  </div>
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
</timeLogFormRows>
<script>
  $(function () { PS.Phb.initTimeLogManyForm() });
</script>
