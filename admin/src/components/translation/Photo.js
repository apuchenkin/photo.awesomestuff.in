import React from 'react';
import { connect } from 'react-redux';

import Translations from './Translations';
import utils from '../../../../client/src/lib/utils';

import {
  load as loadTranslations,
  create as createTranslations,
  update as updateTranslation,
  remove as removeTranslation,
} from '../../store/translation/actions';

const FIELDS = ['title'];

class PhotoTranslations extends React.PureComponent {

  componentDidMount() {
    this.props.load(this.props.photo);
  }

  componentWillReceiveProps(props) {
    if (this.props.photo !== props.photo) {
      this.props.load(props.photo);
    }
  }

  render() {
    const {
      translations,
      photo,
      backUrl,
      create,
      update,
      remove,
    } = this.props;

    return (
      <Translations
        translations={translations}
        create={create(photo)}
        update={update}
        remove={remove}
        fields={FIELDS}
        title={photo.name}
        backUrl={backUrl}
      >
        <img alt={photo.name} src={utils.getSrc(photo.src, 800, 600, true)} />
      </Translations>
    );
  }
}

export default connect(
  ({ translation: { translations } }) => ({
    translations,
  }),
  dispatch => ({
    load: photo => dispatch(loadTranslations({ refType: 'photo', refId: photo.id })),
    create: photo => translation => dispatch(createTranslations(Object.assign({}, translation, {
      refType: 'photo',
      refId: photo.id,
      field: 'description',
    }))),
    update: (translation, data) => dispatch(updateTranslation(translation, data)),
    remove: translation => dispatch(removeTranslation(translation)),
  }),
)(PhotoTranslations);
