import React from 'react';
import { connect } from 'react-redux';

import Translations from './Translations';

import {
  load as loadTranslations,
  create as createTranslations,
  update as updateTranslation,
  remove as removeTranslation,
} from '../../store/translation/actions';

const FIELDS = ['title'];

const CategoryTranslations = ({
  translations,
  category,
  backUrl,
  load,
  create,
  update,
  remove,
}) => (
  <Translations
    translations={translations}
    load={load(category)}
    create={create(category)}
    update={update}
    remove={remove}
    fiels={FIELDS}
    title={category.name}
    backUrl={backUrl}
  />
);
/* {entity.src && <img alt={entity.name}
 src={utils.getSrc(entity.src, 800, 600, true)} />} */

export default connect(
  ({ translation: { translations } }) => ({
    translations,
  }),
  dispatch => ({
    load: category => () => dispatch(loadTranslations({ refType: 'category', refId: category.id })),
    create: category => translation => dispatch(createTranslations(Object.assign({}, translation, {
      refType: 'category',
      refId: category.id,
      field: 'title',
    }))),
    update: (translation, data) => dispatch(updateTranslation(translation, data)),
    remove: translation => dispatch(removeTranslation(translation)),
  }),
)(CategoryTranslations);
