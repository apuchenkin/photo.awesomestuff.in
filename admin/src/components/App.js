import React from 'react';
import classNames from 'classnames';
import {
  withRouter,
} from 'react-router-dom';

import config from '../../../client/src/etc/config.json';
import PhotoService from '../../../client/lib/service/Photo';
import CategoryService from '../../../client/lib/service/Category';

import Photo from './Photo';
import Upload from './Upload';
import Categories from './Categories';


const Photos = ({ data, admin }) => {
  const photos = data.map(photo => (
    <li key={photo.id} >
      <Photo data={photo} admin={admin} />
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
      selection: [],
      categories: [],
      photos: [],
      groups: [],
      showHidden: false,
    };

    this.photoService = new PhotoService({
      apiEndpoint: config.apiEndpoint,
    });

    this.categoryService = new CategoryService({
      apiEndpoint: config.apiEndpoint,
    });
  }

  setPhotos(photos) {
    const groups = this.photoService.groupColors(photos);

    this.setState({ photos, groups });
  }

  fetchPhotos(category) {
    const me = this;
    const state = me.state;
    const photoService = this.photoService;

    me.setState({ photos: [] });
    photoService.fetchPhotos(category, state.showHidden)
      .then((photos) => {
        const parent = state.categories.find(c => c.id === category);

        if (parent && parent.parent) {
          photoService.updateParents(photos, parent.parent, state.showHidden).then(me.setPhotos);
        } else {
          me.setPhotos(photos);
        }
      });
  }

  fetchCategories() {
    this.categoryService.fetchCategories()
      .then(categories => this.setState({ categories }));
  }


  componentDidMount() {
    const state = this.state;
    const category = this.props.match && this.props.match.params.category;

    this.fetchCategories();
    if (category) {
      this.fetchPhotos(category);
    }
  }

  componentWillReceiveProps(props) {
    const category = props.match && props.match.params.category;
    if (category) {
      this.fetchPhotos(category);
    }
  }

  isSelected(photo) {
    return this.state.selection && this.state.selection.length && this.state.selection.find(p => p.id == photo.id);
  }

  createCategory() {
    console.log(123);
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
    this.categoryService
      .patchPhoto(photo, { hidden: !photo.hidden })
      .then(() => {
        this.cleanSelection();
        this.fetchPhotos();
      });
  }

  deletePhotos() {
    const category = this.state.categories.find(c => c.id === this.state.category);

    if (category) {
      this.categoryService.unlinkPhotos(category, this.state.selection)
        .then(() => {
          this.cleanSelection();
          this.fetchPhotos();
        });
    }
  }

  addToCategory(category, photos) {
    this.categoryService.linkPhotos(category, [photos])
      .then(() => {
        this.cleanSelection();
        this.fetchPhotos();
      });
  }

  cleanSelection() {
    this.setState({ selection: [] });
  }

  ungroup(photo) {
    this.photoService.removeGroup(photo.group, [photo])
      .then(() => {
        this.cleanSelection();
        this.fetchPhotos();
      });
  }

  group(photos) {
    const photoService = this.photoService;
    const group = photos.find(p => !!p.group);
    let promise;

    if (group) {
      promise = photoService.appendGroup(group, photos);
    } else {
      promise = photoService.group(photos);
    }

    promise.then(() => {
      this.cleanSelection();
      this.fetchPhotos();
    });
  }

  render() {
    const state = this.state;
    const category = this.props.match.params.category;

    return (
      <div className="admin">
        <div className="aside">
          <Categories data={state.categories} admin={this} />
        </div>
        {category && (
          <div className="content">
            <div className="toolbox">
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
              </div>
            </div>
            <Upload category={category}>
              <Photos data={state.photos} admin={this} />
            </Upload>
          </div>
        )}
      </div>
    );
  }
}

export default withRouter(App);
