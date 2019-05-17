import * as React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import * as queryString from 'query-string';

import Photo from './photo';
import Upload from './upload';
import { PhotoProvider, PhotoContext } from '@app/context';
import PhotoTranslations from './translation/photo';
import Pagination from './pagination';
import { __RouterContext } from 'react-router';

interface Props {
  category: Category;
}

const Photos: React.FunctionComponent<Props> = ({ category }) => {
  const { match } = React.useContext(__RouterContext);
  const {
    getPhotos,
    getPhoto,
    getTotal,
    getGroups,
    getSelectionCount,
    isSelected,
    deletePhotos,
  } = React.useContext(PhotoContext);

  const { page } = queryString.parse(location.search);

  const total = getTotal();
  const photos = getPhotos(page && Number(page));
  const groups = getGroups();

  // const singleSelect = selection.length === 1;

  const photoItems = photos.map((photo: Photo) => (
    <li key={photo.id} >
      <Photo
        photo={photo}
        featured={category.featured === photo.src}
        group={groups[photo.group]}
      />
    </li>
  ));

  const count = getSelectionCount();
  // const canGroup = count > 1 && selection.filter(p => !!p.group).length;

  const PhotosCmp = (
    <div className="photos">
      <div className="toolbox">
        <span>
          {count} selected
        </span>
        <div className="tools">
          {/* <button disabled={count == 1} onClick={() => this.makeFeatured(selection[0])}>
            Feature
          </button> */}
          <button disabled={!count} onClick={() => toggleVisibility()}>
            Show/Hide
          </button>
          {/* <button disabled={!canGroup} onClick={() => this.group(selection)}>
            Group
          </button> */}
          <button disabled={!count} onClick={() => deletePhotos(photos.filter(isSelected))}>
            Delete
          </button>
        </div>
      </div>
      <Upload category={category}>
        <ul>{photoItems}</ul>
      </Upload>
      <Pagination total={total} />
    </div>
  );

  return (
    <Switch>
      <Route
        path={`${match.url}/:id/translation`}
        render={({ match: { params } }) => {
          const photo = getPhoto(Number(params.id));
          if (photo) {
            return <PhotoTranslations photo={photo} />;
          }
        }}
      />
      <Route render={() => PhotosCmp} />
    </Switch>
  );
}

export default ({ category }: Props) => (
  <PhotoProvider category={category}>
    <Photos category={category} />
  </PhotoProvider>
);