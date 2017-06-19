import React from 'react';
import {
  Route,
  Switch,
  withRouter,
} from 'react-router-dom';
import { connect } from 'react-redux';

import Photo from './Photo';
import Upload from './Upload';
// import Translations from './Translations';

import { loadPhotos } from '../store/photo/actions';

class Photos extends React.PureComponent {

  constructor(props) {
    super(props);

    this.state = {
      selection: [],
    };

    this.toggleVisibility = this.toggleVisibility.bind(this);
    this.update = this.update.bind(this);
    this.cleanSelection = this.cleanSelection.bind(this);
    this.select = this.select.bind(this);
    this.isSelected = this.isSelected.bind(this);
    this.group = this.group.bind(this);
    this.ungroup = this.ungroup.bind(this);
    this.delete = this.delete.bind(this);
  }

  componentWillMount() {
    this.update();
  }

  cleanSelection() {
    this.setState({ selection: [] });
  }

  update() {
    this.setState({ selection: [] }, () => {
      const { category } = this.props;
      this.props.loadPhotos(category);
    });
  }

  delete(photos) {
    return () => {
      const { category, admin: { categoryService } } = this.props;

      if (category) {
        categoryService
          .unlinkPhotos(category, photos)
          .then(this.update);
      }
    };
  }

  isSelected(photo) {
    const { selection } = this.state;

    return selection
      && selection.length
      && selection.find(p => p.id === photo.id);
  }

  select(photo, shift) {
    this.setState(({ selection }) => {
      let selection$;

      if (shift) {
        selection$ = this.isSelected(photo)
          ? selection.filter(p => p.id !== photo.id)
          : [...selection, photo];
      } else {
        selection$ = [photo];
      }

      return {
        selection: selection$,
      };
    });

    return true;
  }

  toggleHidden() {
    this.setState(({ showHidden }) => ({ showHidden: !showHidden }),
      this.update,
    );
  }

  makeFeatured(photo) {
    return () => {
      this.props.admin.categoryService.update(this.props.category.name, {
        featured: photo.id,
      });
    };
  }

  toggleVisibility(photo) {
    return () => this.props.admin.photoService
      .patchPhoto(photo.id, { hidden: !photo.hidden })
      .then(this.update);
  }

  ungroup(photo) {
    return () => {
      this.props.admin.photoService
      .removeGroup(photo.group, [photo])
      .then(this.update);
    };
  }

  group(photos) {
    return () => {
      const photoService = this.props.admin.photoService;
      const photo = photos.find(p => !!p.group);
      const promise = photo
        ? photoService.appendGroup(photo.group, photos)
        : photoService.group(photos)
      ;

      promise.then(this.update);
    };
  }

  render() {
    const { admin, category, match, photos, groups } = this.props;
    const { selection } = this.state;
    const canGroup = selection.length > 1 && selection.filter(p => !!p.group).length;

    // const photoTranslations = photo => (
    //   <Translations
    //     service={admin.photoService}
    //     entity={photo}
    //     backUrl={match.url}
    //     field="description"
    //   />
    // );

    const photoItems = photos.map(p => (
      <li key={p.id} >
        <Photo photo={p} group={groups[p.group]} admin={admin} parent={this} />
      </li>
    ));

    const PhotosCmp = (
      <div className="photos">
        <div className="toolbox">
          <span>
            {selection.length} selected
          </span>
          <div className="tools">
            <button disabled={selection.length !== 1} onClick={this.makeFeatured(selection[0])}>
              Feature
            </button>
            <button disabled={selection.length !== 1} onClick={this.toggleVisibility(selection[0])}>
              Show/Hide
            </button>
            <button disabled={!canGroup} onClick={this.group(selection)}>
              Group
            </button>
            <button disabled={!selection.length} onClick={this.delete(selection)}>Delete</button>
          </div>
        </div>
        <Upload category={category}>
          <ul>{photoItems}</ul>
        </Upload>
      </div>
    );

    return (
      <Switch>
        {/* {photos.length && (
          <Route
            path={`${match.url}/:id/translation`}
            render={({ match }) => {
              const photo = photos.find(p => p.id === Number(match.params.id));
              return photoTranslations(photo);
            }}
          />
        )} */}
        <Route render={() => PhotosCmp} />
      </Switch>
    );
  }
}

export default connect(
  ({ photo: { photos, groups } }) => ({
    photos,
    groups,
  }),
  dispatch => ({
    loadPhotos: category => dispatch(loadPhotos(category)),
  }),
)(withRouter(Photos));
