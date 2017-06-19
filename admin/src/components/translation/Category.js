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

class CategoryTranslations extends React.PureComponent {

  componentDidMount() {
    this.props.load(this.props.category);
  }

  componentWillReceiveProps(props) {
    if (this.props.category !== props.category) {
      this.props.load(props.category);
    }
  }

  render() {
    const {
      translations,
      category,
      backUrl,
      create,
      update,
      remove,
    } = this.props;

    return (
      <Translations
        translations={translations}
        create={create(category)}
        update={update}
        remove={remove}
        fiels={FIELDS}
        title={category.name}
        backUrl={backUrl}
      />
    );
  }
}

/* {entity.src && <img alt={entity.name}
 src={utils.getSrc(entity.src, 800, 600, true)} />} */

export default connect(
  ({ translation: { translations } }) => ({
    translations,
  }),
  dispatch => ({
    load: category => dispatch(loadTranslations({ refType: 'category', refId: category.id })),
    create: category => translation => dispatch(createTranslations(Object.assign({}, translation, {
      refType: 'category',
      refId: category.id,
      field: 'title',
    }))),
    update: (translation, data) => dispatch(updateTranslation(translation, data)),
    remove: translation => dispatch(removeTranslation(translation)),
  }),
)(CategoryTranslations);
