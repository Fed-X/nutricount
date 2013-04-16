HomeController = ($rootScope, $scope, $http, authService) ->

    $http.get('http://localhost:8000/auth')
      .success (user) -> authService.loginConfirmed user unless user is 'null'

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
      loginConfirmed: (user) ->
        $rootScope.$broadcast 'auth:loginConfirmed', user
        retryAll()
    ]
    '' # to combat coffeescript's implicit return
  )
  .config([ '$routeProvider', '$locationProvider', '$httpProvider', 'authServiceProvider', ($routeProvider, $locationProvider, $httpProvider, authServiceProvider) ->
    interceptor = ['$rootScope', '$q', ($rootScope, $q) ->
      success = (rsp) -> rsp
      error = (rsp) ->
        if rsp.status == 401
          deferred = $q.defer()
          authServiceProvider.pushToBuffer rsp.config, deferred
          $rootScope.$broadcast 'auth:loginRequired'
          deferred.promise
        else
          $q.reject rsp

      (promise) -> promise.then success, error
    ]
    $httpProvider.responseInterceptors.push interceptor

    $routeProvider.when('/',
      templateUrl: '/partials/home.html'
      controller: HomeController
    ).otherwise redirectTo: '/'

    $locationProvider.html5Mode true
  ])
  .run ($http, $timeout, $rootScope, authService) ->
    $rootScope.form = {}

    $rootScope.$on 'auth:loginConfirmed', (event, user) ->
      $rootScope.user = user

    $rootScope.$on 'auth:loginRequired', ->
      $timeout(
        (-> angular.element('#login').bPopup { modalClose: false }, -> angular.element('#login input[type=email]').focus())
        , 500
      )

    $rootScope.login_modal = -> angular.element('.modal#login').bPopup()

    $rootScope.login = ->
      $http({
        method: 'POST'
        url: '/login'
        data: $.param { email: @form.email, password: @form.password }
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
      }).success (rsp, status, headers) ->
        if rsp.user
          $rootScope.form = {}
          angular.element('#login').bPopup().close()
          authService.loginConfirmed rsp.user
        else
          angular.element('.modal#login .message').text rsp.message

    $rootScope.signup_modal = -> angular.element('.modal#signup').bPopup()

    $rootScope.signup = ->
      if @form.password == @form.password_confirmation
        $http({
          method: 'POST'
          url: '/register'
          data: $.param { name: @form.name, email: @form.email, password: @form.password }
          headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
        }).success (rsp, status, headers) ->
          if rsp.user
            $rootScope.form = {}
            angular.element('#signup').bPopup().close()
            authService.loginConfirmed rsp.user
          else
            angular.element('.modal#signup .message').text rsp.message

    $rootScope.signup_link = ->
      angular.element('#login').bPopup().close()
      $timeout(
        -> angular.element('#signup').bPopup({ modalClose: false }); angular.element('#signup input[type=text]:first').focus()
        400
      )
    $rootScope.signin_link = ->
      angular.element('#signup').bPopup().close()
      $timeout(
        -> angular.element('#login').bPopup({ modalClose: false }); angular.element('#login input[type=email]').focus()
        400
      )
