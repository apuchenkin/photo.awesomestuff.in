// require('normalize.css/normalize.css');

import React from 'react';
import { Router, Route, Link, browserHistory, IndexRoute, withRouter } from 'react-router';
import { DragSource, DropTarget, DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import classNames from 'classnames';
import Auth from './Auth';

import config from '../../../src/etc/config.json';
import PhotoService from '../../../lib/service/Photo';
import CategoryService from '../../../lib/service/Category';

require('../styles/app.less');

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
      groups: [],
      showHidden: false
    };
  },

  fetchCategories () {
    let me = this,
        categoryService = new CategoryService({
          apiEndpoint: config.apiEndpoint,
        });

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
          photoService.updateParents(photos, parent.parent, state.showHidden).then(me.setPhotos);
        } else {
          me.setPhotos(photos);
        }
      });
  },

  setPhotos(photos) {
    let
      photoService = new PhotoService(this.state.token),
      groups = photoService.groupColors(photos);

    this.setState({photos: photos, groups: groups});
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
    let me = this;

    me.setState({showHidden: !me.state.showHidden}, () => {
      me.cleanSelection();
      me.fetchPhotos();
    });
  },

  toggleVisibility(photo) {
    let me = this,
        photoService = new PhotoService(me.state.token);

    photoService.patchPhoto(photo, {hidden: !photo.hidden})
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  },

  deletePhotos() {
    let me = this,
        category = me.state.categories.find(c => c.id == me.state.category),
        categoryService = new CategoryService(me.state.token);

    if (category) {
      categoryService.unlinkPhotos(category, me.state.selection)
        .then(() => {
          me.cleanSelection();
          me.fetchPhotos();
        });
    }
  },

  addToCategory(category, photos) {
    let me = this,
        categoryService = new CategoryService(me.state.token);

    categoryService.linkPhotos(category, [photos])
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  },

  cleanSelection() {
    this.setState({selection: []});
  },

  ungroup(photo) {
    let me = this,
        photoService = new PhotoService(me.state.token);

    photoService.removeGroup(photo.group, [photo])
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  },

  group(photos) {
    let me = this,
        photoService = new PhotoService(me.state.token),
        group = photos.find(p => !!p.group),
        promise;

    if (group) {
      promise = photoService.appendGroup(group, photos);
    } else {
      promise = photoService.group(photos);
    }

    promise.then(() => {
      me.cleanSelection();
      me.fetchPhotos();
    });
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
            <button disabled={!state.selection.length || state.selection.filter(p => !!p.group).length} onClick={this.group.bind(this, state.selection)}>
              Group
            </button>
            <button disabled={!state.selection.length} onClick={this.deletePhotos}>Delete</button>
            <button style={style} onClick={this.logout}>Logout</button>
          </div>
          </h1>
        </header>
        <div className="content">
          <Categories data={state.categories} admin={this} />
          <Photos data={state.photos} admin={this} />
        </div>
      </div>
    )
  }
}));

const Categories = React.createClass({
  render() {
    let
      admin = this.props.admin,
      categories = this.props.data.map(function(category) {
          return (
            <li className="item" key={category.id} >
              <Category data={category} admin={admin} />
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

const categoryDrop = {
  drop: function({admin, data}, monitor) {
    admin.addToCategory(data, monitor.getItem());
  },
  canDrop(props, monitor) {
    return true;
  }
}

const Category = DropTarget(PHOTO, categoryDrop, collectDrop)(React.createClass({
  render() {
    let category = this.props.data;

    var dropTarget = this.props.dropTarget;
    var highlighted = this.props.highlighted;
    var hovered = this.props.hovered;

    return dropTarget(
        <div className={classNames({
            'category': true,
            'isHidden': category.hidden,
            'category--hovered': hovered
          })}>
          <Link to={`/category/${category.id}`} activeClassName="active">{category.name}</Link>
        </div>
      );
  }
}))

const photoSource = {
  beginDrag: function (props) {
    return props.data;
  }
}

const photoDrop = {
  drop: function({admin, data}, monitor) {
    admin.group([monitor.getItem(), data]);
  },
  canDrop(props, monitor) {
    return props.data.id != monitor.getItem().id;
  }
}

/**
 * Specifies which props to inject into your component.
 */
function collectDrag(connect, monitor) {
  return {
    // Call this function inside render()
    // to let React DnD handle the drag events:
    dragSource: connect.dragSource(),
    // You can ask the monitor about the current drag state:
    isDragging: monitor.isDragging()
  };
}

function collectDrop(connect, monitor) {
  return {
    highlighted: monitor.canDrop(),
    hovered: monitor.isOver() && monitor.canDrop(),
    dropTarget: connect.dropTarget()
  };
}

const Photo = DragSource(PHOTO, photoSource, collectDrag)(DropTarget(PHOTO, photoDrop, collectDrop)(React.createClass({
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
    var dragSource = this.props.dragSource;
    var dropTarget = this.props.dropTarget;
    var highlighted = this.props.highlighted;
    var hovered = this.props.hovered;

    return dragSource(dropTarget(
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
        onDoubleClick={() => console.log(photo)}
        >
        <div className="views">{photo.views}</div>
        {photo.hasParent && <div className="parent"></div>}
        {photo.group && <div className="group" style={{background: admin.state.groups[photo.group]}} onClick={admin.ungroup.bind(admin, photo)}></div>}
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
