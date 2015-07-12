import Ember from 'ember';

export default Ember.Controller.extend({
  actions: {
    loginAction: function() {
      Ember.$.post(
        "/api/login",
        JSON.stringify({
          username: this.get("username"),
          password: this.get("password")
        }),
        "json"
      ).done( function() {
        console.log("good!");
      }).fail( function() {
        console.log("bad!");
      });
    }
  }
});
