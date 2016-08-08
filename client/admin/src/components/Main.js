require('normalize.css/normalize.css');
require('styles/app.less');

import React from 'react';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import { DragSource, DropTarget, DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import classNames from 'classnames';

// let yeomanImage = require('../images/yeoman.png');

const AUTH = 'auth';
const PHOTO = 'photo';

const App = React.createClass({
  render() {
    return (
      <div>App
        <div>{this.props.children}</div>
      </div>
    )
  }
})

const Admin = withRouter(React.createClass({
  getInitialState() {
    return {
      token: localStorage.getItem(AUTH) || '',
      category: this.props.params ? this.props.params.category : null,
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
      <div className="admin">
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
        <div className="content">
          <Categories data={state.categories} />
          <Photos data={state.photos} />
        </div>
      </div>
    )
  }
}));

const Categories = React.createClass({
  render() {
    let categories = this.props.data.map(function(category) {
          return (
            <li className="item" key={category.id} >
              <Category data={category} />
            </li>
          );
    });

    return (
      <nav className="aside">
        <ul>{categories}</ul>
      </nav>
    );
  }
})

const Photos = React.createClass({
  render() {
    let photos = this.props.data.map(function(photo) {
      return (
        <li key={photo.id} >
          <Photo data={photo} />
        </li>
      );
    });

    return (
      <div className="photos">
        <ul>{photos}</ul>
      </div>
    );
  }
})

const Category = React.createClass({
  render() {
    let category = this.props.data;
    return (
        <Link to={`/category/${category.id}`} activeClassName="active">{category.name}</Link>
    );
  }
})

const photoSource = {
  beginDrag: function (props) {
    // Return the data describing the dragged item
    var item = { id: props.id };
    return item;
  },

  endDrag: function (props, monitor, component) {
    if (!monitor.didDrop()) {
      return;
    }

    // When dropped on a compatible target, do something
    var item = monitor.getItem();
    var dropResult = monitor.getDropResult();
    // CardActions.moveCardToList(item.id, dropResult.listId);
  }
}

const photoDrop = {

}

/**
 * Specifies which props to inject into your component.
 */
function collect(connect, monitor) {
  return {
    // Call this function inside render()
    // to let React DnD handle the drag events:
    connectDragSource: connect.dragSource(),
    // You can ask the monitor about the current drag state:
    isDragging: monitor.isDragging()
  };
}

function collectDrop(connect, monitor) {
  return {
    highlighted: monitor.canDrop(),
    hovered: monitor.isOver(),
    connectDropTarget: connect.dropTarget()
  };
}

const Photo = DragSource(PHOTO, photoSource, collect)(DropTarget(PHOTO, photoDrop, collectDrop)(React.createClass({
  render() {
    let photo = this.props.data;

    // These two props are injected by React DnD,
    // as defined by your `collect` function above:
    var isDragging = this.props.isDragging;
    var connectDragSource = this.props.connectDragSource;
    var connectDropTarget = this.props.connectDropTarget;
    var highlighted = this.props.highlighted;
    var hovered = this.props.hovered;

    return connectDragSource(connectDropTarget(
      <div className={classNames({
          'photo': true,
          'photo--highlighted': highlighted,
          'photo--hovered': hovered,
          'dragging': isDragging
        })}>
        <div className="views">{photo.views}</div>
        {photo.hasParent && <div class="parent"></div>}
        {/*
        <div class="group" ng-style="admin.groupStyle[item.group]" ng-if="item.group"
             ng-click="admin.unGroup(item);"></div>
         */}
        <img src={"/api/v1/" + photo.thumb} height="160" />
      </div>
    ));
  }
})));


const checkAuth = (nextState, replace, callback) => {
  if (!localStorage.getItem(AUTH)) {
    replace('/auth');
  }
  callback();
}

const Auth = withRouter(React.createClass({
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
}))

const NoMatch = React.createClass({
  render() {
    return (
      <div>NoMatch</div>
    )
  }
})

class AppComponent extends React.Component {
  render() {
    return (
      <Router history={browserHistory}>
        <Route path="/" component={App} >
          <IndexRoute onEnter={checkAuth} component={Admin} />
          <Route path="auth" component={Auth} />
          <Route path="category/:category" component={Admin} />
          <Route path="*" component={NoMatch} />
        </Route>
      </Router>
    );
  }
}

AppComponent.defaultProps = {
};

export default DragDropContext(HTML5Backend)(AppComponent);
