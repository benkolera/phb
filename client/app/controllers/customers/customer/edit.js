import Ember from 'ember';

export default Ember.Controller.extend({
  actions: {
    saveCustomer: function() {
      this.get("model").save();
    }
  }
});
