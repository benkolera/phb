<projectForm class="form-horizontal">
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
    <dfLabel ref="customers" class="col-sm-2 control-label">
      Customers:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputList ref="customers">
        <ul class="list-unstyled">
          <dfListItem>
            <li itemAttrs>
              <div class="input-group">
                <dfInputSelect ref="" class="form-control" />
                <span class="input-group-btn">
                  <button class="btn btn-danger" removeControl value="Remove">
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
    <dfLabel ref="status" class="col-sm-2 control-label">
      Status
    </dfLabel>
    <div class="col-sm-2">
      <dfInputText ref="statusPhase" class="form-control"/>
    </div>
    <div class="col-sm-2">
      <dfInputSelect ref="statusColour" class="form-control"/>
    </div>
    <div class="col-sm-2">
      <dfInputTextArea ref="statusDesc" class="form-control"/>
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="stakeholders" class="col-sm-2 control-label">
      Stakeholders:
    </dfLabel>
    <div class="col-sm-2">
      <dfInputList ref="stakeholders">
        <ul class="list-unstyled">
          <dfListItem>
            <li itemAttrs>
              <div class="input-group">
                <dfInputSelect class="form-control" ref="" />
                <span class="input-group-btn">
                  <button class="btn btn-danger" removeControl value="Remove">
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
    <dfLabel ref="targets" class="col-sm-2 control-label">
      Targets
    </dfLabel>
    <dfInputList ref="targets">
      <div class="col-sm-10">
        <dfListItem>
          <div itemAttrs>
            <div class="row">
              <div class="col-sm-2">
                <dfInputText ref="desc" class="form-control" />
              </div>
              <div class="col-sm-2">
                <dfInput ref="date" type="date" class="form-control" />
              </div>
              <div class="col-sm-2">
                <dfInputText ref="handwavy" placeholder="Handwavy target" class="form-control" />
              </div>
              <div class="col-sm-2">
                <button class="btn btn-danger" removeControl value="Remove">
                  <i class="fa fa-trash" />
                </button>
              </div>
            </div>
          </div>
        </dfListItem>
        <div class="row"><div class="col-sm-2">
            <button class="btn btn-xs btn-default" addControl>
              <i class="fa fa-plus" /> Add Another
            </button>
        </div></div>
      </div>
    </dfInputList>
  </div>
  <div class="form-group">
    <dfLabel ref="started" class="col-sm-2 control-label">
      Start:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="started" class="form-control" />
    </div>
  </div>
  <div class="form-group">
    <dfLabel ref="finished" class="col-sm-2 control-label">
      Finished:
    </dfLabel>
    <div class="col-sm-2">
      <dfInput type="date" ref="finished" class="form-control" />
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
</projectForm>
