<heartbeatForm class="form-horizontal">
  <dfChildErrorList ref="" />
  <div class="row"><div class="col-sm-12">
    <div class="form-group">
      <dfLabel ref="day" class="col-sm-2 control-label">
        Period:
      </dfLabel>
      <div class="col-sm-2">
        <dfInput type="date" ref="start" class="form-control"/>
      </div>
      <div class="col-sm-2">
        <dfInput type="date" ref="finish" class="form-control"/>
      </div>
    </div>
  </div></div>
  <div class="row"> <div class="col-sm-12">
    <div class="form-group">
      <dfLabel ref="highlights" class="col-sm-2 control-label">
        Highlights
      </dfLabel>
      <dfInputList ref="highlights">
        <div class="col-sm-10">
          <dfListItem>
            <div itemAttrs>
              <div class="row"><div class="col-sm-12">
                <div class="input-group">
                  <dfInputText class="form-control" ref="" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div></div>
              </div>
            </div>
          </dfListItem>
          <button class="btn btn-xs btn-default" addControl>
            <i class="fa fa-plus" /> Add Another
          </button>
        </div>
      </dfInputList>
    </div>
  </div></div>
  <div class="row"><div class="col-sm-12">
    <div class="form-group">
      <dfLabel ref="upcoming" class="col-sm-2 control-label">
        Upcoming
      </dfLabel>
      <dfInputList ref="upcoming">
        <div class="col-sm-10">
          <dfListItem>
            <div itemAttrs>
              <div class="row"><div class="col-sm-12">
                <div class="input-group">
                  <dfInputText class="form-control" ref="" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div></div>
              </div>
            </div>
          </dfListItem>
          <button class="btn btn-xs btn-default" addControl>
            <i class="fa fa-plus" /> Add Another
          </button>
        </div>
      </dfInputList>
    </div>
  </div></div>
  <div class="row"><div class="col-sm-12">
    <div class="form-group">
      <dfLabel ref="upcoming" class="col-sm-2 control-label">
        Successes
      </dfLabel>
      <dfInputList ref="successes">
        <div class="col-sm-10">
          <div class="row">
            <div class="col-sm-4 subform-heading">
              What
            </div>
            <div class="col-sm-4 subform-heading">
              Achievements
            </div>
            <div class="col-sm-2 subform-heading">
              People
            </div>
            <div class="col-sm-2 subform-heading">
            </div>
          </div>
          <dfListItem>
            <div itemAttrs>
              <div class="row">
                <div class="col-sm-4">
                  <dfInputText ref="what" class="form-control" />
                </div>
                <div class="col-sm-4">
                  <dfInputList ref="achievements">
                    <dfListItem>
                      <div itemAttrs><div class="row">
                          <div class="col-sm-12">
                            <div class="input-group">
                              <dfInputText ref="" class="form-control" />
                              <span class="input-group-btn">
                                <button class="btn btn-danger" removeControl value="Remove">
                                  <i class="fa fa-trash" />
                                </button>
                              </span>
                            </div>
                          </div>
                      </div></div>
                    </dfListItem>
                    <div class="row"><div class="col-sm-1">
                      <button class="btn btn-xs btn-default" addControl>
                        <i class="fa fa-plus" />Add Another Achievement
                      </button>
                    </div></div>
                  </dfInputList>
                </div>
                <div class="col-sm-2">
                  <dfInputList ref="people">
                    <dfListItem>
                      <div itemAttrs><div class="row">
                          <div class="col-sm-12">
                            <div class="input-group">
                              <dfInputSelect ref="" class="form-control" />
                              <span class="input-group-btn">
                                <button class="btn btn-danger" removeControl value="Remove">
                                  <i class="fa fa-trash" />
                                </button>
                              </span>
                            </div>
                          </div>
                      </div></div>
                    </dfListItem>
                    <div class="row"><div class="col-sm-1">
                      <button class="btn btn-xs btn-default" addControl>
                        <i class="fa fa-plus" />Add Another Person
                      </button>
                    </div></div>
                  </dfInputList>
                </div>
                <div class="col-sm-2">
                  <button class="btn btn-xs btn-danger" removeControl value="Remove">
                    <i class="fa fa-trash" /> Remove Success
                  </button>
                </div>
              </div>
              <hr />
            </div>
          </dfListItem>
          <button class="btn btn-xs btn-default" addControl>
            <i class="fa fa-plus" /> Add Another Success
          </button>
        </div>
      </dfInputList>
    </div>
  </div></div>
  <div class="row">
    <div class="form-group">
      <label class="col-sm-2 control-label">
        Links
      </label>
      <div class="col-sm-2">
        <dfLabel ref="projects" class="subform-heading">
          Projects
        </dfLabel>
        <dfInputList ref="projects">
          <dfListItem>
            <div itemAttrs><div class="row">
              <div class="col-sm-12">
                <div class="input-group">
                  <dfInputSelect ref="" class="form-control" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div>
              </div>
            </div></div>
          </dfListItem>
          <div class="row"><div class="col-sm-1">
              <button class="btn btn-xs btn-default" addControl>
                <i class="fa fa-plus" />Add Another
              </button>
          </div></div>
        </dfInputList>
      </div>
      <div class="col-sm-2">
        <dfLabel ref="backlog" class="subform-heading">
          Backlog
        </dfLabel>
        <dfInputList ref="backlog">
          <dfListItem>
            <div itemAttrs><div class="row">
              <div class="col-sm-12">
                <div class="input-group">
                  <dfInputSelect ref="" class="form-control" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div>
              </div>
            </div></div>
          </dfListItem>
          <div class="row"><div class="col-sm-1">
              <button class="btn btn-xs btn-default" addControl>
                <i class="fa fa-plus" />Add Another
              </button>
          </div></div>
        </dfInputList>
      </div>
      <div class="col-sm-2">
        <dfLabel ref="events" class="subform-heading">
          Events
        </dfLabel>
        <dfInputList ref="events">
          <dfListItem>
            <div itemAttrs><div class="row">
              <div class="col-sm-12">
                <div class="input-group">
                  <dfInputSelect ref="" class="form-control" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div>
              </div>
            </div></div>
          </dfListItem>
          <div class="row"><div class="col-sm-1">
              <button class="btn btn-xs btn-default" addControl>
                <i class="fa fa-plus" />Add Another
              </button>
          </div></div>
        </dfInputList>
      </div>
      <div class="col-sm-2">
        <dfLabel ref="actions" class="subform-heading">
          Actions
        </dfLabel>
        <dfInputList ref="actions">
          <dfListItem>
            <div itemAttrs><div class="row">
              <div class="col-sm-12">
                <div class="input-group">
                  <dfInputSelect ref="" class="form-control" />
                  <span class="input-group-btn">
                    <button class="btn btn-danger" removeControl value="Remove">
                      <i class="fa fa-trash" />
                    </button>
                  </span>
                </div>
              </div>
            </div></div>
          </dfListItem>
          <div class="row"><div class="col-sm-1">
              <button class="btn btn-xs btn-default" addControl>
                <i class="fa fa-plus" />Add Another
              </button>
          </div></div>
        </dfInputList>
      </div>
    </div>
  </div>
  <div class="row"><div class="col-sm-12">
    <div class="form-group">
      <div class="col-sm-offset-2 col-sm-2">
        <dfInputSubmit value="Submit" />
      </div>
    </div>
  </div></div>
</heartbeatForm>
