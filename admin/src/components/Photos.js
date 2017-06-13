import React from 'react';
import classNames from 'classnames';

import Photo from './Photo';
import Upload from './Upload';

import PhotoService from '../../../client/lib/service/Photo';

class Photos extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      photos: [],
      selection: [],
      groups: [],
      showHidden: false,
    };
  }

  componentDidMount() {
    this.update();
  }

  componentWillReceiveProps() {
    this.update();
  }

  setPhotos(photos) {
    const groups = PhotoService.groupColors(photos);

    this.setState({ photos, groups });
  }

  cleanSelection() {
    this.setState({ selection: [] });
  }

  update() {
    const { categories, category, admin } = this.props;
    const state = this.state;
    const photoService = admin.photoService;

    this.setState({ photos: [] });
    admin.categoryService.fetchPhotos(category, state.showHidden)
      .then((photos) => {
        const parent = categories.find(c => c.id === category);

        if (parent && parent.parent) {
          photoService.updateParents(photos, parent.parent, state.showHidden).then(this.setPhotos);
        } else {
          this.setPhotos(photos);
        }
      });
  }

  isSelected(photo) {
    return this.state.selection
      && this.state.selection.length
      && this.state.selection.find(p => p.id === photo.id);
  }

  select(photo, shift) {
    let selection = this.state.selection;

    if (shift) {
      this.isSelected(photo)
        ? selection = selection.filter(p => p.id !== photo.id)
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
      me.update();
    });
  }

  toggleVisibility(photo) {
    this.props.admin.categoryService
      .patchPhoto(photo, { hidden: !photo.hidden })
      .then(() => {
        this.cleanSelection();
        this.fetchPhotos();
      });
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
    const { admin, category } = this.props;
    const { photos, selection, showHidden } = this.state;

    const photoItems = photos.map(photo => (
      <li key={photo.id} >
        <Photo photo={photo} admin={admin} parent={this} />
      </li>
    ));

    return (
      <div className="photos">
        <div className="toolbox">
          <span>
            {selection.length} selected
          </span>
          <button
            className={classNames({
              'show-hidden': true,
              active: showHidden,
            })}
            onClick={this.toggleHidden}
          >
            hidden
          </button>
          <div className="tools">
            <button disabled={selection.length !== 1} onClick={this.toggleVisibility.bind(this, selection[0])}>
              Show/Hide
            </button>
            <button disabled={!selection.length || selection.filter(p => !!p.group).length} onClick={this.group.bind(this, selection)}>
              Group
            </button>
            <button disabled={!selection.length} onClick={this.deletePhotos}>Delete</button>
          </div>
        </div>
        <Upload category={category}>
          <ul>{photoItems}</ul>
        </Upload>
      </div>
    );
  }
}

export default Photos;
