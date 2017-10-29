import React from 'react';
import { compose } from 'redux';
import { number, string, shape, arrayOf, bool, func } from 'prop-types';
import { Helmet } from 'react-helmet';
import { injectIntl, intlShape, defineMessages } from 'react-intl';
import withRouter from 'found/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Figure from '../components/photo/figure';
import PhotoLink from '../components/link/photo';
import { fromCategory } from '../components/link/category';

import IconLeft from '../components/photo/icons/left';
import IconRight from '../components/photo/icons/right';

import style from '../components/photo/photo.less';

const messages = defineMessages({
  prev: {
    id: 'prev',
    defaultMessage: 'Prev',
  },
  next: {
    id: 'next',
    defaultMessage: 'Next',
  },
});

class Photo extends React.PureComponent {
  constructor(props) {
    super(props);

    this.goNext = this.goNext.bind(this);
    this.close = this.close.bind(this);
  }

  goNext(next) {
    const { category, router } = this.props;
    const url = category.parent
      ? `${category.parent.name}/${category.name}`
      : category.name;

    return () => router.push(`/${url}/photo/${next.id}`);
  }

  close() {
    const { category, router } = this.props;
    const url = category.parent
      ? `${category.parent.name}/${category.name}`
      : category.name;

    router.push(`/${url}`);
  }

  render() {
    const { intl, category, photos, config, isLoading, data: { photo } } = this.props;

    const pidx = photos.findIndex(p => p.id === photo.id);
    const prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1];
    const next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1];
    const backUrl = `/${category.parent ? `${category.parent.name}/${category.name}` : category.name}`;
    const onClick = this.goNext(next);

    // store.dispatch(setMeta({
    //   langs: photo.langs,
    //   title: photo.description,
    //   description: intl.formatMessage({ id: 'meta.description.photo' }, {
    //     author: photo.author && photo.author.name,
    //     title: photo.description,
    //   }),
    // }));

    return (
      // eslint-disable-next-line jsx-a11y/no-static-element-interactions
      <div className={isLoading ? `${style.photo} ${style.loading}` : style.photo} onClick={this.close}>
        <Helmet>
          <title>{photo.description}</title>
          <meta
            name="description"
            content={intl.formatMessage({ id: 'meta.description.photo' }, {
              author: photo.author && photo.author.name,
              title: photo.description,
            })}
          />
        </Helmet>
        <Figure
          photo={photo}
          backUrl={backUrl}
          onClick={onClick}
          config={config}
        />
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={prev && prev.id}
          className={`${style.nav} ${style.prev}`}
          title={intl.formatMessage(messages.prev)}
        >
          <IconLeft />
        </PhotoLink>
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={next && next.id}
          className={`${style.nav} ${style.next}`}
          title={intl.formatMessage(messages.next)}
        >
          <IconRight />
        </PhotoLink>
      </div>
    );
  }
}

Photo.propTypes = {
  category: shape({ name: string.isRequired }).isRequired,
  photos: arrayOf(shape({ id: number.isRequired })).isRequired,
  intl: intlShape.isRequired,
};

export default compose(
  withStyles(style),
  injectIntl,
  withRouter,
)(Photo);
