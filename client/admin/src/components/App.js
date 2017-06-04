import React from 'react';
import classNames from 'classnames';
import {
  withRouter,
} from 'react-router-dom';

import config from '../../../src/etc/config.json';
import PhotoService from '../../../lib/service/Photo';
import CategoryService from '../../../lib/service/Category';
import Category from './Category';
import Photo from './Photo';

const AUTH = 'auth';

const Categories = (props) => {
  const categories = props.data.map(category => (
    <li className="item" key={category.id} >
      <Category data={category} admin={props.admin} />
    </li>
  ));

  return (
    <nav className="aside">
      <ul>{categories}</ul>
    </nav>
  );
};

const Photos = (props) => {
  const photos = props.data.map(photo => (
    <li key={photo.id} >
      <Photo data={photo} admin={props.admin} />
    </li>
  ));

  return (
    <div className="photos">
      <ul>{photos}</ul>
    </div>
  );
};

class App extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      token: localStorage.getItem(AUTH) || '',
      category: props.params ? props.params.category : null,
      selection: [],
      categories: [],
      photos: [],
      groups: [],
      showHidden: false,
    };
  }

  setPhotos(photos) {
    const photoService = new PhotoService(this.state.token);
    const groups = photoService.groupColors(photos);

    this.setState({ photos, groups });
  }

  fetchPhotos() {
    const me = this;
    const state = me.state;
    const photoService = new PhotoService(state.token);

    me.setState({ photos: [] });
    photoService.fetchPhotos(state.category, state.showHidden)
      .then((photos) => {
        const parent = state.categories.find(c => c.id === state.category);

        if (parent && parent.parent) {
          photoService.updateParents(photos, parent.parent, state.showHidden).then(me.setPhotos);
        } else {
          me.setPhotos(photos);
        }
      });
  }

  fetchCategories() {
    let me = this,
      categoryService = new CategoryService({
        apiEndpoint: config.apiEndpoint,
      });

    categoryService.fetchCategories()
      .then(categories => me.setState({ categories }));
  }


  componentDidMount() {
    let me = this,
      state = me.state;

    me.fetchCategories();
    if (state.category) {
      me.fetchPhotos();
    }
  }

  componentWillReceiveProps(props) {
    this.setState({ category: props.params && props.params.category }, this.fetchPhotos);
  }

  logout() {
    localStorage.removeItem(AUTH);
    this.props.router.push('/auth');
  }

  isSelected(photo) {
    return this.state.selection && this.state.selection.length && this.state.selection.find(p => p.id == photo.id);
  }

  select(photo, shift) {
    let selection = this.state.selection;

    if (shift) {
      this.isSelected(photo)
        ? selection = selection.filter(p => p.id != photo.id)
        : selection.push(photo);
    } else {
      selection = [photo];
    }

    this.setState({ selection });
  }

  toggleHidden() {
    const me = this;

    me.setState({ showHidden: !me.state.showHidden }, () => {
      me.cleanSelection();
      me.fetchPhotos();
    });
  }

  toggleVisibility(photo) {
    let me = this,
      photoService = new PhotoService(me.state.token);

    photoService.patchPhoto(photo, { hidden: !photo.hidden })
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  }

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
  }

  addToCategory(category, photos) {
    let me = this,
      categoryService = new CategoryService(me.state.token);

    categoryService.linkPhotos(category, [photos])
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  }

  cleanSelection() {
    this.setState({ selection: [] });
  }

  ungroup(photo) {
    let me = this,
      photoService = new PhotoService(me.state.token);

    photoService.removeGroup(photo.group, [photo])
      .then(() => {
        me.cleanSelection();
        me.fetchPhotos();
      });
  }

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
  }

  render() {
    let state = this.state,
      style = { background: 'red' };

    return (
      <div className="admin">
        <header className="main">
          <h1 className="title">
            <span>
              {state.selection.length} selected
          </span>
            <span
              className={classNames({
                'show-hidden': true,
                active: state.showHidden,
              })}
              onClick={this.toggleHidden}
            >
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
    );
  }
}

export default withRouter(App);
