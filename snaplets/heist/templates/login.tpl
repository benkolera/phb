<apply template="base">
  <login>
    <h1>Login</h1>
    <loginForm class="form-horizontal">
      <dfChildErrorList ref="" />
      <div class="form-group">
        <dfLabel ref="name" class="col-sm-2 control-label">
          Username:
        </dfLabel>
        <div class="col-sm-2">
          <dfInputText ref="username" class="form-control"/>
        </div>
      </div>
      <div class="form-group">
      <dfLabel ref="name" class="col-sm-2 control-label">
        Password:
      </dfLabel>
      <div class="col-sm-2">
        <dfInputPassword ref="password" class="form-control"/>
      </div>
      </div>
      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-1">
          <div class="checkbox">
            <label>
              <dfInputCheckbox ref="rememberMe"/> Remember me
            </label>
          </div>
        </div>
      </div>
      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-2">
          <dfInputSubmit value="Submit" />
        </div>
      </div>
    </loginForm>
  </login>
</apply>
