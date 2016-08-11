require('normalize.css/normalize.css');
require('styles/app.less');

import React from 'react';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import { DragSource, DropTarget, DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import classNames from 'classnames';
import Auth from './Auth';
import PhotoService from '../service/Photo';
import CategoryService from '../service/Category';

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
      selection: [],
      categories: [],
      photos: [],
      showHidden: false
    };
  },

  fetchCategories () {
    let me = this,
        categoryService = new CategoryService(me.state.token);

    categoryService.fetchCategories()
      .then(categories => me.setState({categories: categories}));
  },

  fetchPhotos () {
    let me = this,
        state = me.state,
        photoService = new PhotoService(state.token);

    me.setState({photos: []});
    photoService.fetchPhotos(state.category, state.showHidden)
      .then(photos => {
        let parent = state.categories.find(c => c.id == state.category);

        if (parent && parent.parent) {
          photoService.updateParents(photos, parent.parent, state.showHidden).then(result => me.setState({photos: result}));
        } else {
          me.setState({photos: photos});
        }
      });
  },

  componentDidMount() {
    let me = this,
        state = me.state;

    me.fetchCategories();
    if (state.category) {
      me.fetchPhotos();
    }
  },

  componentWillReceiveProps(props) {
    this.setState({category: props.params && props.params.category}, this.fetchPhotos);
  },

  logout() {
    localStorage.removeItem(AUTH);
    this.props.router.push('/auth');
  },

  isSelected(photo) {
    return this.state.selection && this.state.selection.length && this.state.selection.find(p => p.id == photo.id);
  },

  select(photo, shift) {
    let selection = this.state.selection;

    if (shift) {
      this.isSelected(photo)
        ? selection = selection.filter(p => p.id != photo.id)
        : selection.push(photo);
      } else {
        selection = [photo];
      }

    this.setState({selection: selection});
  },

  toggleHidden() {
    this.setState({showHidden: !this.state.showHidden}, this.fetchPhotos);
  },

  toggleVisibility(photo) {
    photo.hidden = !photo.hidden;
    return fetch('/api/v1/photo/' + photo.id, {
        method: 'PATCH',
        headers: {
          'Authorization': this.state.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify({hidden: photo.hidden})
      });
        // item.$save(['hidden']);
  },

  dropPhotos() {
    return true;
  },

  render() {
    let state = this.state,
        style = {background: 'red'};

    return (
      <div className="admin">
        <header className="main">
          <h1 className="title">
          <span>
            {state.selection.length} selected
          </span>
          <span className={classNames({
              'show-hidden': true,
              'active': state.showHidden,
            })}
            onClick={this.toggleHidden}>
            hidden
          </span>
          <div className="tools">
            <button disabled={state.selection.length !== 1} onClick={this.toggleVisibility.bind(this, state.selection[0])}>
              Show/Hide
            </button>
            <button disabled={!state.selection.length} onClick={this.dropPhotos}>Drop</button>
            <button style={style} onClick={this.logout}>Logout</button>
          </div>
          </h1>
        </header>
        <div className="content">
          <Categories data={state.categories} />
          <Photos data={state.photos} admin={this} />
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
    let
      admin = this.props.admin,
      photos = this.props.data.map(function(photo) {
        return (
          <li key={photo.id} >
            <Photo data={photo} admin={admin} />
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
  // componentWillReceiveProps(props) {
  //   debugger;
  // },
  render() {
    let
      photo = this.props.data,
      admin = this.props.admin
    ;

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
          'dragging': isDragging,
          'selected': admin.isSelected(photo),
          'hasParent': photo.hasParent,
          'isHidden': photo.hidden
        })}
        onClick={e => admin.select(photo, e.ctrlKey)}
        >
        <div className="views">{photo.views}</div>
        {photo.hasParent && <div className="parent"></div>}
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
