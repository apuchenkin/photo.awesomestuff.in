import React from 'react';
import { connect } from 'react-redux';
import { injectIntl, intlShape, defineMessages } from 'react-intl';
import withRouter from 'react-router/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import { routerShape } from 'react-router/lib/PropTypes';
import { bind } from 'decko';

import Figure from './figure';
import { stopLoading } from '../../actions/loader';
import Component from '../../lib/PureComponent';
import PhotoLink from '../link/photo';
import { fromCategory } from '../link/category';

import style from './photo.less';

const
  { number, string, shape, arrayOf, bool, func } = React.PropTypes
  ;

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

class Photo extends Component {

  static propTypes = {
    category: shape({ name: string.isRequired }).isRequired,
    photos: arrayOf(shape({ id: number.isRequired })).isRequired,
    photo: shape().isRequired,
    intl: intlShape.isRequired,
    router: routerShape.isRequired,
    isLoading: bool.isRequired,
    stopLoading: func.isRequired,
  }

  componentWillMount() {
    if (isBrowser) {
      // stops the loading initiated by server
      this.props.stopLoading();
    }
  }

  @bind
  goNext(next) {
    const
      { category, router } = this.props,
      url = category.parent ? `${category.parent.name}/${category.name}` : category.name
      ;

    return () => router.push(`/${url}/photo/${next.id}`);
  }

  @bind
  close() {
    const
      { category, router } = this.props,
      url = category.parent ? `${category.parent.name}/${category.name}` : category.name
      ;

    router.push(`/${url}`);
  }

  render() {
    const
      { intl, photo, category, photos, isLoading } = this.props,
      pidx = photos.findIndex(p => p.id === photo.id),
      prev = photos[pidx - 1 < 0 ? photos.length - 1 : pidx - 1],
      next = photos[pidx + 1 > photos.length - 1 ? 0 : pidx + 1],
      backUrl = `/${category.parent ? `${category.parent.name}/${category.name}` : category.name}`,
      onClick = this.goNext(next);

    return (
      // eslint-disable-next-line jsx-a11y/no-static-element-interactions
      <div className={isLoading ? `${style.photo} ${style.loading}` : style.photo} onClick={this.close}>
        <Figure
          photo={photo}
          backUrl={backUrl}
          onClick={onClick}
        />
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={prev && prev.id}
          className={`${style.nav} ${style.prev}`}
          title={intl.formatMessage(messages.prev)}
        >
          <i className="icon-left-open" />
        </PhotoLink>
        <PhotoLink
          {...fromCategory(category)}
          onClick={e => e.stopPropagation()}
          photoId={next && next.id}
          className={`${style.nav} ${style.next}`}
          title={intl.formatMessage(messages.next)}
        >
          <i className="icon-right-open" />
        </PhotoLink>
      </div>
    );
  }
}

export default connect(
  state => ({ isLoading: state.isLoading.count > 0 }),
  dispatch => ({
    stopLoading: () => dispatch(stopLoading()),
  })
)(withStyles(style)(withRouter(injectIntl(Photo))));
