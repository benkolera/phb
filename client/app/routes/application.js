import Ember from 'ember';

export default Ember.Route.extend({
  actions: {
    error: function(error) {
      if (error.errors[0] && error.errors[0].status === 401) {
        this.transitionTo('login');
        return false;
      }
      return true;
    }
  }
});
