HomeController = ($scope, $http) ->

    #$http.get('http://localhost:8000/auth')

    # Fetching localStorage items
    # ---------------------------
    sfood = localStorage.getItem('selected_food')
    $scope.selected_food = (sfood && JSON.parse sfood) || []

    splans = localStorage.getItem('saved_plans')
    $scope.saved_plans = (splans && JSON.parse splans) || {}

    $scope.calories = localStorage.getItem('calories') || '2000'
    $scope.$watch 'calories', -> localStorage.setItem 'calories', $scope.calories

    # Fetching json database items
    # ----------------------------
    $http.get('/nutricount/data/food.json').success (food) -> $scope.food = food

    $http.get('/nutricount/data/values.json').success (nutrients) ->
      $scope.nutrients = nutrients
      nutrients = _.keys nutrients
      $scope.nutrients1 = nutrients[0..7]
      $scope.nutrients2 = nutrients[7..15]
      $scope.nutrients3 = nutrients[16..25]

    # Functions
    # ----------------------------

    save_food  = -> localStorage.setItem('selected_food', JSON.stringify $scope.selected_food)
    save_plans = -> localStorage.setItem('saved_plans',   JSON.stringify $scope.saved_plans)

    $scope.itemsExist = -> $scope.selected_food[0]

    $scope.clear = -> $scope.selected_food = []

    $scope.save = ->
      name = prompt 'What would you like to save this as?'
      if name
        $scope.saved_plans[name] = _.map $scope.selected_food, (food) -> _.clone food
        save_plans()

    $scope.loadPlan = (plan) ->
      $scope.selected_food = _.map plan, (food) -> _.clone food

    $scope.removePlan = (plan) -> delete $scope.saved_plans[plan]; save_plans()

    $scope.foodSelect = ->
      

    $scope.addFood = ->
      food = _.find $scope.food, (food) -> food.name is angular.element('select').val()
      if food
        sfood = _.find($scope.selected_food, (sfood) -> sfood.name == food.name)
        if sfood
          $scope.addServing sfood
        else
          food.servings = 1
          $scope.selected_food.push food
          save_food()
        angular.element('select').select2('val', '')

    $scope.removeFood = (food) ->
      if food.servings > 1
        food.servings -= 1
      else
        $scope.selected_food = _.reject $scope.selected_food, (selected_food) -> selected_food == food
      save_food()

    $scope.addServing = (food) -> food.servings += 1; save_food()

    $scope.calculate = (nutrient) ->
      num = 0
      _.each $scope.selected_food, (food) ->
        amount = parseFloat food[nutrient]
        if amount
          _(food.servings).times -> num += amount
      Math.round(num * 100) / 100

    $scope.calculate_total = (nutrient) ->
      if nutrient is 'Calories (kcal)'
        $scope.calories
      else
        Math.round( ($scope.nutrients[nutrient] * ($scope.calories / 2000)) * 100 ) / 100

    $scope.separator = (nutrient) ->
      if nutrient == 'Total Fat (g)' || nutrient == 'Saturated Fat (g)' || nutrient == 'Sodium (mg)'
        ' < '
      else
        ' / '

    $scope.nutrientStatus = (nutrient) ->
      percent_done = Math.round( $scope.calculate(nutrient) / $scope.calculate_total(nutrient) * 100 ) / 100
      if percent_done < 0.75
        if nutrient == 'Total Fat (g)' || nutrient == 'Saturated Fat (g)' || nutrient == 'Sodium (mg)'
          'done'
        else
          'insufficient'
      else if 1 > percent_done >= 0.75
        'close'
      else if 1 <= percent_done
        if nutrient == 'Total Fat (g)' || nutrient == 'Saturated Fat (g)' || nutrient == 'Sodium (mg)'
          'insufficient'
        else
          'done'


angular.module('nutricount', ['ui'])
  .provider('authService', ->
    buffer = []
    @pushToBuffer = (config, deferred) ->
      buffer.push
        config: config
        deferred: deferred

    @$get = ['$rootScope', '$injector', ($rootScope, $injector) ->
      retry = (config, deferred) ->
        $http = $http or $injector.get '$http'
        $http(config).then (rsp) -> deferred.resolve rsp

      retryAll = ->
        i = 0
        while i < buffer.length
          retry buffer[i].config, buffer[i].deferred
          ++i
        buffer = []
      $http = undefined
      loginConfirmed: ->
        $rootScope.$broadcast 'auth:loginConfirmed'
        retryAll()
    ]
    '' # to combat coffeescript's implicit return
  )
  .directive('init', ($http, $timeout, authService) -> (scope) ->
    window.scope = scope
    scope.form = {}

    # scope.$on 'auth:loginConfirmed', ->
    scope.$on 'auth:loginRequired', ->
      console.log 'yo'
      angular.element('#login').bPopup { modalClose: false }, -> angular.element('#login input[type=email]').focus()

    scope.login = ->
      angular.element('#login').bPopup().close()
      $http({
        method: 'POST'
        url: '/login'
        data: $.param { email: @form.email, password: @form.password }
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
      }).success (rsp, status, headers) ->
        if rsp.success
          scope.form = {}
          authService.loginConfirmed()
        else
          scope.$broadcast 'auth:loginRequired'

    scope.signup = ->
      angular.element('#signup').bPopup().close()
      if @form.password == @form.password_confirmation
        $http({
          method: 'POST'
          url: '/register'
          data: $.param { name: @form.name, email: @form.email, password: @form.password }
          headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
        }).success (rsp, status, headers) -> scope.form = {}; authService.loginConfirmed() if rsp.success

    scope.signup_link = ->
      angular.element('#login').bPopup().close()
      $timeout(
        -> angular.element('#signup').bPopup({ modalClose: false }); angular.element('#signup input[type=text]:first').focus()
        400
      )
    scope.signin_link = ->
      angular.element('#signup').bPopup().close()
      $timeout(
        -> angular.element('#login').bPopup({ modalClose: false }); angular.element('#login input[type=email]').focus()
        400
      )
  )
  .config([ '$routeProvider', '$locationProvider', '$httpProvider', 'authServiceProvider', ($routeProvider, $locationProvider, $httpProvider, authServiceProvider) ->
    interceptor = ['$rootScope', '$q', ($rootScope, $q) ->
      success = (rsp) -> rsp
      error = (rsp) ->
        console.log rsp.headers()
        if rsp.status == 401
          console.log 'err'
          deferred = $q.defer()
          authServiceProvider.pushToBuffer rsp.config, deferred
          $rootScope.$broadcast 'auth:loginRequired'
          deferred.promise
        else
          $q.reject rsp

      (promise) -> promise.then success, error
    ]
    $httpProvider.responseInterceptors.push interceptor

    $routeProvider.when('/nutricount',
      templateUrl: '/nutricount/partials/home.html'
      controller: HomeController
    ).otherwise redirectTo: '/nutricount'

    $locationProvider.html5Mode true
  ])
