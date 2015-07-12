import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.route('login');
  this.route('snapshots');
  this.route('projects');
  this.route('backlog');
  this.route('events');
  this.route('tasks', function() {
    this.route('create');
  });
  this.route('timeLogs', function() {
    this.route('create');
  });
  this.route('customers', function () {
    this.route('create');
    this.route('customer', { path: ':customer_id'}, function () {
      this.route('edit');
    });
  });
  this.route('people');
  this.route('supportCategories');
});

export default Router;
