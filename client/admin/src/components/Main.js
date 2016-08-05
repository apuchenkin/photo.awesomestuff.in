require('normalize.css/normalize.css');
require('styles/app.less');

import React from 'react'
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router'

// let yeomanImage = require('../images/yeoman.png');

const AUTH = "auth"

const App = React.createClass({
  render() {
    return (
      <div>App
        <div>{this.props.children}</div>
      </div>
    )
  }
})

const Admin = React.createClass({
  getInitialState() {
    return {
      token: localStorage.getItem(AUTH) || '',
      category: this.props.location.query ? this.props.location.query.category : null,
      categories: [],
      photos: []
    };
  },

  fetchCategories () {
    let me = this;

    fetch('/api/v1/category')
      .then(response => {
        return response.text();
      })
      .then(strem => {
        me.setState({categories: JSON.parse(strem)});
      })
  },

  fetchPhotos (category) {
    let me = this;

    fetch('/api/v1/category/' + category + '/photo', {
        headers: {
          'Authorization': me.state.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
      })
      .then(response => {
        return response.text();
      })
      .then(strem => {
        me.setState({photos: JSON.parse(strem)});
      })
  },

  componentDidMount: function() {
    let me = this,
        state = me.state;

    me.fetchCategories();
    if (state.category) {
      me.fetchPhotos(state.category);
    }
  },

  logout() {
    localStorage.removeItem(AUTH);
    this.props.router.push('/auth');
  },

  render() {
    let state = this.state,
        style = {background: 'red'};

    return (
      <div>Admin ({state.token}):
        <header className="main">
          <h1 className="title">
          {/*<span>
            {{admin.selected.length}} selected
          </span>
            <span class="show-hidden" ng-click="admin.toggleHidden();" ng-class="{active: admin.isShowHidden}">
              hidden
            </span>*/}

            <div className="tools">
              {/*
              <button ng-disabled="admin.selected.length !== 1" ng-click="admin.toggleVisibility(admin.selected[0]);">
                Show/Hide
              </button>
              <button ng-disabled="!admin.selected.length" ng-click="admin.drop();">Drop</button>
              */}
              <button style={style} onClick={this.logout}>Logout</button>
            </div>
          </h1>
        </header>
        <Categories data={state.categories} />
        <Photos data={state.photos} />
      </div>
    )
  }
})

const Categories = React.createClass({
  render() {
    let categories = this.props.data.map(function(category) {
          return (
            <Category data={category} key={category.id} />
          );
    });

    return (
      <nav className="aside">
        <ul>
          <li className="item">{categories}</li>
        </ul>
      </nav>
    );
  }
})

const Photos = React.createClass({
  render() {
    let photos = this.props.data.map(function(photo) {
      return (
        <Photo data={photo} key={photo.id} />
      );
    });

    return (
      <div className="photos">
        <ul>
          <li>{photos}</li>
        </ul>
      </div>
    );
  }
})

const Category = React.createClass({
  render() {
    let category = this.props.data;
    return (
        <Link to={`/?category=${category.id}`} activeClassName="active">{category.name}</Link>
    );
  }
})

const Photo = React.createClass({
  render() {
    let photo = this.props.data;

    return (
      <div>
        <img src={"/api/v1/" + photo.thumb} height="160" />
      </div>
    );
  }
})

// <nav ng-cloak ng-if="admin.categories" class="aside">
//   <ul>
//     <li class="item"
//         ng-cloak
//         ui-on-drop="admin.onDrop($data, c);"
//         drop-validate="c.name !== admin.category"
//         ng-class="{active: c.name == admin.category}"
//         ng-click="admin.setCategory(c);"
//         ng-repeat="c in admin.categories">{{c.name}}
//     </li>
//   </ul>
// </nav>

const checkAuth = (nextState, replace, callback) => {
  if (!localStorage.getItem(AUTH)) {
    replace('/auth');
  }
  callback();
}

const Auth = React.createClass({
  getInitialState() {
    return {email: '', password: ''};
  },

  submit() {
    localStorage.setItem(AUTH, 'Basic ' + window.btoa([this.state.email, this.state.password].join(':')));
    this.props.router.push('/');
  },

  render() {
    return (
      <div>
        <form onSubmit={this.submit}>
          Username ({this.state.email}):
          <input name="email"
            type="email"
            value={this.state.email}
            onChange={e => this.setState({email: e.target.value})}
          />
          Password ({this.state.password}):
          <input name="password"
            type="passord"
            value={this.state.password}
            onChange={e => this.setState({password: e.target.value})}
          />
          <input type="submit" />
        </form>
      </div>
    )
  }
})

const NoMatch = React.createClass({
  render() {
    return (
      <div>NoMatch</div>
    )
  }
})

// const Users = React.createClass({
//   render() {
//     return (
//       <div>
//         <h1>Users</h1>
//         <div className="master">
//           <ul>
//             {/* use Link to route around the app */}
//             {this.state.users.map(user => (
//               <li key={user.id}><Link to={`/user/${user.id}`}>{user.name}</Link></li>
//             ))}
//           </ul>
//         </div>
//         <div className="detail">
//           {this.props.children}
//         </div>
//       </div>
//     )
//   }
// })
//
// const User = React.createClass({
//   componentDidMount() {
//     this.setState({
//       // route components are rendered with useful information, like URL params
//       user: findUserById(this.props.params.userId)
//     })
//   },
//
//   render() {
//     return (
//       <div>
//         <h2>{this.state.user.name}</h2>
//         {/* etc. */}
//       </div>
//     )
//   }
// })

class AppComponent extends React.Component {
  render() {
    return (
      <Router history={browserHistory}>
        <Route path="/(?category=:category)" component={App}>
          <IndexRoute onEnter={checkAuth} component={withRouter(Admin)} />
          <Route path="auth" component={withRouter(Auth)} />
          <Route path="*" component={NoMatch} />
        </Route>
      </Router>
    );
  }
}

AppComponent.defaultProps = {
};

export default AppComponent;
