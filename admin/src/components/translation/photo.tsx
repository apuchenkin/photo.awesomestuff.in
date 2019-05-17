import * as React from 'react';
import Translations from './translations';
import { getThumb } from '@app/service/photo';
import TranslationProvider, { UpdateTranslations } from '@app/context/translation';
import { PhotoContext } from '@app/context';

const FIELDS = ['description'];

interface Props {
  photo: Photo;
}

const PhotoTranslations: React.FunctionComponent<Props> = ({ photo }) => {
  const { updatePhoto } = React.useContext(PhotoContext);

  const update: UpdateTranslations = translations => updatePhoto({
    ...photo,
    translations,
  });

  return (
    <TranslationProvider translations={photo.translations} update={update}>
      <Translations
        fields={FIELDS}
        translations={photo.translations}
        title={photo.src}
      >
        <img alt={photo.src} src={getThumb(800, photo.src)} />
      </Translations>
    </TranslationProvider>
  );
}

export default PhotoTranslations;
