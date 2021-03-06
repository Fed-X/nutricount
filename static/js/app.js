// Generated by CoffeeScript 1.6.2
(function() {
  var HomeController;

  HomeController = function($rootScope, $scope, $http, authService) {
    window.s = $scope;
    window.rs = $rootScope;
    $http.get('http://localhost:8000/auth').success(function(user) {
      if (user !== 'null') {
        return authService.loginConfirmed(user);
      }
    });
    $scope.calculate = function(nutrient) {
      var num;

      num = 0;
      _.each($scope.selected_food, function(food) {
        var amount;

        amount = parseFloat(food[nutrient]);
        if (amount) {
          return _(food.servings).times(function() {
            return num += amount;
          });
        }
      });
      return Math.round(num * 100) / 100;
    };
    $scope.calculate_total = function(nutrient) {
      if (nutrient === 'Calories (kcal)') {
        return $scope.calories;
      } else {
        return Math.round(($scope.nutrients[nutrient] * ($scope.calories / 2000)) * 100) / 100;
      }
    };
    $scope.separator = function(nutrient) {
      if (nutrient === 'Total Fat (g)' || nutrient === 'Saturated Fat (g)' || nutrient === 'Sodium (mg)') {
        return ' < ';
      } else {
        return ' / ';
      }
    };
    return $scope.nutrientStatus = function(nutrient) {
      var percent_done;

      percent_done = Math.round($scope.calculate(nutrient) / $scope.calculate_total(nutrient) * 100) / 100;
      if (percent_done < 0.75) {
        if (nutrient === 'Total Fat (g)' || nutrient === 'Saturated Fat (g)' || nutrient === 'Sodium (mg)') {
          return 'done';
        } else {
          return 'insufficient';
        }
      } else if ((1 > percent_done && percent_done >= 0.75)) {
        return 'close';
      } else if (1 <= percent_done) {
        if (nutrient === 'Total Fat (g)' || nutrient === 'Saturated Fat (g)' || nutrient === 'Sodium (mg)') {
          return 'insufficient';
        } else {
          return 'done';
        }
      }
    };
  };

  angular.module('nutricount', ['ui']).provider('authService', function() {
    var buffer;

    buffer = [];
    this.pushToBuffer = function(config, deferred) {
      return buffer.push({
        config: config,
        deferred: deferred
      });
    };
    this.$get = [
      '$rootScope', '$injector', function($rootScope, $injector) {
        var $http, retry, retryAll;

        retry = function(config, deferred) {
          var $http;

          $http = $http || $injector.get('$http');
          return $http(config).then(function(rsp) {
            return deferred.resolve(rsp);
          });
        };
        retryAll = function() {
          var i;

          i = 0;
          while (i < buffer.length) {
            retry(buffer[i].config, buffer[i].deferred);
            ++i;
          }
          return buffer = [];
        };
        $http = void 0;
        return {
          loginConfirmed: function(user) {
            $rootScope.$broadcast('auth:loginConfirmed', user);
            return retryAll();
          }
        };
      }
    ];
    return '';
  }).config([
    '$routeProvider', '$locationProvider', '$httpProvider', 'authServiceProvider', function($routeProvider, $locationProvider, $httpProvider, authServiceProvider) {
      var interceptor;

      interceptor = [
        '$rootScope', '$q', function($rootScope, $q) {
          var error, success;

          success = function(rsp) {
            return rsp;
          };
          error = function(rsp) {
            var deferred;

            if (rsp.status === 401) {
              deferred = $q.defer();
              authServiceProvider.pushToBuffer(rsp.config, deferred);
              $rootScope.$broadcast('auth:loginRequired');
              return deferred.promise;
            } else {
              return $q.reject(rsp);
            }
          };
          return function(promise) {
            return promise.then(success, error);
          };
        }
      ];
      $httpProvider.responseInterceptors.push(interceptor);
      $routeProvider.when('/', {
        templateUrl: '/partials/home.html',
        controller: HomeController
      }).otherwise({
        redirectTo: '/'
      });
      return $locationProvider.html5Mode(true);
    }
  ]).run(function($http, $timeout, $rootScope, authService) {
    $rootScope.form = {};
    $rootScope.$on('auth:loginConfirmed', function(event, user) {
      return $rootScope.user = user;
    });
    $rootScope.$on('auth:loginRequired', function() {
      return $timeout((function() {
        return angular.element('#login').bPopup({
          modalClose: false
        }, function() {
          return angular.element('#login input[type=email]').focus();
        });
      }), 500);
    });
    $rootScope.login_modal = function() {
      return angular.element('.modal#login').bPopup();
    };
    $rootScope.login = function() {
      return $http({
        method: 'POST',
        url: '/login',
        data: $.param({
          email: this.form.email,
          password: this.form.password
        }),
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        }
      }).success(function(rsp, status, headers) {
        if (rsp.user) {
          $rootScope.form = {};
          angular.element('#login').bPopup().close();
          return authService.loginConfirmed(rsp.user);
        } else {
          return angular.element('.modal#login .message').text(rsp.message);
        }
      });
    };
    $rootScope.signup_modal = function() {
      return angular.element('.modal#signup').bPopup();
    };
    $rootScope.signup = function() {
      if (this.form.password === this.form.password_confirmation) {
        return $http({
          method: 'POST',
          url: '/register',
          data: $.param({
            name: this.form.name,
            email: this.form.email,
            password: this.form.password
          }),
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
          }
        }).success(function(rsp, status, headers) {
          if (rsp.user) {
            $rootScope.form = {};
            angular.element('#signup').bPopup().close();
            return authService.loginConfirmed(rsp.user);
          } else {
            return angular.element('.modal#signup .message').text(rsp.message);
          }
        });
      }
    };
    $rootScope.signup_link = function() {
      angular.element('#login').bPopup().close();
      return $timeout(function() {
        angular.element('#signup').bPopup({
          modalClose: false
        });
        return angular.element('#signup input[type=text]:first').focus();
      }, 400);
    };
    return $rootScope.signin_link = function() {
      angular.element('#signup').bPopup().close();
      return $timeout(function() {
        angular.element('#login').bPopup({
          modalClose: false
        });
        return angular.element('#login input[type=email]').focus();
      }, 400);
    };
  });

}).call(this);
