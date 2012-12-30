'use strict';

angular.module('todoServices', ['ngResource']).
    factory('Todo', function($resource) {
  return $resource('/api/todo/:todoId', {}, {
    query: {method:'GET', isArray:true},
    save:  {method:'POST'}
  });
});
