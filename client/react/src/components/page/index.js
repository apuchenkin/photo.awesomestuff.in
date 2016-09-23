import React from 'react';
import shallowCompare from 'react-addons-shallow-compare';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import style from './style.less';

const { string } = React.PropTypes;

class Page extends React.Component {

  static propTypes = {
    content: string.isRequired,
  }

  shouldComponentUpdate(nextProps, nextState) {
    return shallowCompare(this, nextProps, nextState);
  }

  render() {
    return (
      // eslint-disable-next-line react/no-danger
      <div className={style.page} dangerouslySetInnerHTML={{ __html: this.props.content }} />
    );
  }
}

export default withStyles(style)(Page);
