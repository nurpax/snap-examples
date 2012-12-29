function TodoCtrl() {
    var scope = this;
    
    // initialize controller's model    
    this.todos = [{
        text: "learn Angular",
        done: true},
    {
        text: "build Angular app",
        done: false},
    {
        text: "show off Angular app",
        done: false}];
    
    // Define member functions
    this.addTodo = function() {
        scope.todos.push({
            text: scope.todoText,
            done: false
        });
        scope.remaining++;
        scope.todoText = "";
    };

    this.recalc = function() {
        scope.remaining = scope.todos.length;
        angular.forEach(scope.todos, function(todo) {
            if (todo.done) {
                scope.remaining--;
            }
        });
    };

    this.removeDone = function() {
        var todos = scope.todos;
        scope.todos = [];
        
        angular.forEach(todos, function(todo) {
            if (!todo.done) {
                scope.todos.push(todo);
            }
        });
    };

    // call recalc once to update the model's 'remaining' value
    // Note: this needs to be called after definition
    this.recalc();
}​
