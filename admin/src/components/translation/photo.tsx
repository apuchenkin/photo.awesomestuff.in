import * as React from 'react';
import Translations from './translations';
import { getThumb } from '@app/service/photo';

const FIELDS = ['description'];

interface Props {
  photo: Photo;
}

const PhotoTranslations: React.FunctionComponent<Props> = ({ photo }) => (
  <Translations
    fields={FIELDS}
    translations={photo.translations}
    title={photo.src}
  >
    <img alt={photo.src} src={getThumb(800, photo.src)} />
  </Translations>
);

export default PhotoTranslations;
