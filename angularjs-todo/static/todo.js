'use strict';

function TodoCtrl($scope, Todo) {

  $scope.todos = Todo.query();

  $scope.addTodo = function() {
      var newTodo = { text:$scope.todoText, done:false };
      Todo.save(newTodo, function (todo) {
          $scope.todos.push(todo);
      });
      $scope.todoText = '';
  };
 
    // Persist immediate as clicked on
    $scope.checkTodo = function (todo) {
        todo.$save();
    };

    $scope.remaining = function() {
        var count = 0;
        angular.forEach($scope.todos, function(todo) {
            count += todo.done ? 0 : 1;
        });
        return count;
    };
    
    $scope.archive = function() {
        var oldTodos = $scope.todos;
        $scope.todos = [];
        angular.forEach(oldTodos, function(todo) {
            if (!todo.done)
                $scope.todos.push(todo);
        });
    };
}

angular.module('todoApp', ['todoServices']).
  config(['$routeProvider', function($routeProvider) {
/*  $routeProvider.
/*      when('/phones', {templateUrl: 'partials/phone-list.html',   controller: PhoneListCtrl}).
      when('/phones/:phoneId', {templateUrl: 'partials/phone-detail.html', controller: PhoneDetailCtrl}).      otherwise({redirectTo: '/phones'});*/

}]);
